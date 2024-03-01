## setup
library(rgdal)
library(tidyverse)
library(wdpar)
library(sf)
library(fasterize)
library(raster)
target.crs <- st_crs("ESRI:54009")

##############################################################################################################
####################### -------------------      Filter WDPA IDs         ----------- #########################
##############################################################################################################
data <- read_csv("./data/WDPA_Jun2021_Public_csv/WDPA_Jun2021_Public_csv.csv", #read in September 2020 WDPA data (n = 262,804)
                 col_types = cols(WDPA_PID = col_character())) #keep WDPA_PID as character to prevent NAs
length(unique(data$WDPA_PID)) #265919

data <- data %>%
  filter(TYPE == 'Polygon' &  #remove point files
           MARINE == 0 & #remove marine areas
           DESIG_ENG != "Marine Protected Area" & #ensure marine protected areas are removed
           (STATUS %in% c('Designated', 'Established', 'Inscribed')) & 
           DESIG_ENG != 'UNESCO-MAB Biosphere Reserve' & #remove, as in Jones et al. (2018) Science, 360(6390), 788-791. 
           (! WDPA_PID %in% c("555655917",  "555656005", "555656013", "555665477", "555656021",
                              "555665485", "555556142", "187", "555703455", "555563456", "15894"))) #remove remaining marine PAs misclassified by WDPA as terrestrial
length(unique(data$WDPA_PID)) #234623
sum(data$GIS_AREA) #22045145

wdpa_filter <- data %>% filter(GIS_AREA >= 200) #filter to PAs greater than 200km2
length(unique(wdpa_filter$WDPA_PID)) # 9270
sum(wdpa_filter$GIS_AREA) #20356754

rm(data)
##############################################################################################################
###################### ------------- Join Spatial Geometries to filtered IDs  ---------- #####################
##############################################################################################################
path <- paste0(getwd(), "/data/WDPA_Jun2021_Public_shp/") 
files <- dir(path = path, recursive = TRUE, pattern = "*\\.shp") #get shapefiles in directory

wdpa_spatial <- map(files, function(x){
  paste0(path, x) %>% 
    read_sf %>%  #read all files
    filter(WDPA_PID %in% wdpa_filter$WDPA_PID)}) %>% # use WDPA_PID for IDs
  do.call(rbind, .) %>%   #bind filtered shapefiles
  st_set_precision(1e5) %>% 
  st_transform(crs = 4326) %>% #change crs for wdpar package (wdpa_clean function)
  mutate(PERIMETER = st_cast(geometry) %>% st_length(), #calculate perimeter
         PA_RATIO = as.numeric(PERIMETER/(GIS_AREA)), #calculate perimeter-area ratio 
         PA_DEF = as.double(PA_DEF)) %>% #keep PA_DEF numeric for rasterize step
  filter(PA_RATIO < quantile(PA_RATIO, 0.75)) %>% #remove PAs in top quantile of perimeter-area ratio (removes PAs that are narrow)
  st_transform(crs = paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0", #need this WGS84 notation for wdpa_clean() to work
                           "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")) %>%
  st_make_valid() %>% #correct geometry issues before the rasterize step
  st_cast("MULTIPOLYGON")


length(unique(wdpa_spatial$WDPA_PID)) #2,809  #r2 - 3918 #r final - 6,952

rm(wdpa_filter)

##############################################################################################################
###################### ------------------    Use Best Practice Cleaning   -------------- #####################
##############################################################################################################
wdpa_spatial <- wdpa_clean(wdpa_spatial,
                           crs = paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0", #need this WGS84 notation for wdpa_clean() to work
                                       "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"),
                           snap_tolerance = 1, #set snap to grid default value
                           geometry_precision = 1500, #set precision for spatial processing to default value
                           erase_overlaps = TRUE, #dissolve any overlapping boundaries
                           verbose = interactive()) #show progress
#length(unique(wdpa_spatial$WDPA_PID)) #6719

##############################################################################################################
################## -------------  Rasterize to fill geometry holes within PAs  -------------- ################
##############################################################################################################
source("./code/gdal_polygonizeR.R") #load function for faster polygonization

hole_pix <- 3 #set size of hole to be filled (unit is pixel)
r <- raster(wdpa_spatial, res = 500) #create 500m template raster
wdpa_r <- fasterize(wdpa_spatial[,3], r, field = "PA_DEF", fun = "first", background = 0) #convert polygons to raster
wdpa_r <- raster::focal(wdpa_r, w = matrix(1, hole_pix, hole_pix)) #create focal window to fill holes (NOTE: this shifts the boundary out by hole_pix*500m/2)
rclmat <- matrix(c(0,NA,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1), ncol=2, byrow=TRUE) #evaluate the output of hole_pix kernel (and next line reclassify)
wdpa_r <- reclassify(wdpa_r, rclmat) #reclassify so all values > 0 and <= 9 become 1, otherwise NA. Anything that is > 0 should mean it is a part of a PA 

system.time(wdpa_shape <- gdal_polygonizeR(wdpa_r)) #change raster back to polygons with filled holes

wdpa_shape <- wdpa_shape %>% 
  st_as_sf(.) %>% 
  st_set_crs(st_crs(wdpa_spatial)) %>%
  st_transform(crs = target.crs) %>% 
  dplyr::select(-DN) %>% 
  st_buffer(., -(hole_pix*500/2)) %>% #shrink the boundary back to original dimensions (pre-filling holes)
  filter(!st_is_empty(.)) #remove any geometries that became empty after shrinking  

nrow(wdpa_shape) #4339

rm(wdpa_r, r, hole_pix, rclmat)

###########################################################################################################
################## -------------  Add BIOME to the WDPA polygon attributes  -------------- ################
###########################################################################################################
#load ecoregion data to label PAs by ecoregion
ecoregion <- read_sf("data/Ecoregions2017/Ecoregions2017.shp") %>%  #dataset provider - RESOLVE Biodiversity and Wildlife Solutions Bioscience; An Ecoregions-Based Approach to Protecting Half the Terrestrial Realm doi:10.1093/biosci/bix014
  st_transform(crs = target.crs) %>% 
  group_by(BIOME_NAME) %>%  
  summarize()

#join ecoregions to wdpa polygons (n = )   <<<< take a while <<<
wdpa_eco <- wdpa_spatial %>%
  st_transform(crs = target.crs) %>% 
  st_make_valid() %>% 
  st_join(., st_make_valid(ecoregion), left = TRUE, largest = TRUE) #join BIOME to PAs, keep largest BIOME label
## warning - attribute variables are assumed to be spatially constant throughout all geometries
length(unique(wdpa_eco$WDPA_PID)) #6952

st_write(wdpa_shape, "./data/GLOBAL/Global_wdpa_footprint_June2021.shp") #write file of filtered PA frontiers, or whether protected meets unprotected
st_write(st_cast(wdpa_eco, "MULTIPOLYGON"), "./data/GLOBAL/Global_wdpa_wInfo_June2021.shp") #write file of filtered PAs with attribute information  
# rm(wdpa_spatial, ecoregion)

###########################################################################################################
################## -----------------------------  END CODE  ------------------------------ ################
###########################################################################################################
