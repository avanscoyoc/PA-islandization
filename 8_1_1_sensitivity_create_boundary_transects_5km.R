#--------------------------------------------------------
# PART 2 CREATE PAIRED SAMPLE POINTS ALONG PA BOUNDARIES
#--------------------------------------------------------
# setup
library(tidyverse)
library(sf)
target.crs <- st_crs("ESRI:54009")

#------------------------------------------------------
# Setup initial parameters
#------------------------------------------------------
source("./code/evenspace.R") #load function. this function creates points along lines at a set distance
source("./code/transect.R")  #load function. this function creates transects perpendicular to lines

#set parameters
sample_dist = 500 # transect spacing (meters)
transect_unit = 2500 # distance between samples along a transect (meters)
transect_pts = 1 # number of points on each side of boundary point
buffer_dist = transect_unit*transect_pts + 500 #size of inner buffer, evaluates point validity
pixel_size = 500 # MODIS data resolution (meters)

# read data 
bd <- st_read( "./data/GLOBAL/Global_wdpa_footprint_June2021.shp") %>% #read in filtered PA frontiers, or whether protected meets unprotected
  st_transform(target.crs) %>% mutate(LINEID = 1:nrow(.)) #add unique ID for footprints
pg <- st_read("./data/GLOBAL/Global_wdpa_wInfo_June2021.shp") %>% #write file of filtered PAs with attribute information  
  st_transform(target.crs) #PA polygons with attributes. making sure projection is correct

#------------------------------------------------------
# Generate sample points
#------------------------------------------------------
#format dataframe for evenspace function to run 
bd.subset <- map(bd$LINEID, ~subset(bd, LINEID==.)) # footprint polygon to boundary line
length(bd.subset) #4,339

#create list of boundary coordinates (n = 4,339)
bd.coord <- bd.subset %>% 
  map(st_coordinates) %>% 
  map(., function(x) {x[,1:2]})

#create boundary points with 500-meter spacing (n = 4,339)
div.pts <- map(bd.coord, function(x){evenspace(x, sample_dist)})
length(div.pts) #4,339

#create transect points ({x y} = boundary points, {x0 y0} = inside points, {x1 y1} = outside points)
transect.pts <- map(div.pts, function(x){transect(x,transect_unit,transect_pts)}) 

#convert all transects to sf objects
transect.pts <- map(transect.pts, function(x){st_as_sf(x, coords = c("x", "y")) %>% 
    st_set_crs(target.crs)})
length(transect.pts) #4，339

rm(div.pts, bd.coord)

#----------------------------------------------------------------------------
#  Count the total possible transects (pre-filtering) for each PA & export
#  To later calculate the percent of each PA perimeter that was evaluated
#----------------------------------------------------------------------------
transect.pts.bd <- transect.pts%>%
  data.table::rbindlist(., idcol = 'index') %>% #bind all lists with id index
  st_as_sf() %>%
  unite(ID, index, transectID, sep = "_") %>%
  filter(point_position == 0)
nrow(transect.pts.bd) #3,136,704

#------------------------------------------------------
# Evaluation 1: Remove inner points that intersect with 
# the inner buffer or are outside of the PA boundary
#------------------------------------------------------
#create boundary inner buffer
bd.inner.buffer <- map(bd.subset, function(x){st_buffer(x, -buffer_dist)}) 

#get inner transect points
transect.pts.inner <- map(transect.pts, function(x){x %>% filter(point_position < 0)}) 
length(transect.pts.inner) #4,339

#label which inner points intersect the inner buffer
transect.pts.inner <- pmap(list(transect.pts.inner, bd.inner.buffer, bd.subset), #list of all points generated, list of polygons, list of polygones
                           function(x,y,z){
                             if (st_is_empty(y)) {x %>% # delete PAs that are too small for inner boundary 
                                 mutate(inner.valid = 0, boundary.valid  = 0)}
                             else { x %>% 
                                 mutate(inner.valid = as.integer(!(st_intersects(x, y, sparse=FALSE))), #if don't intersect inner buffer, TRUE
                                        boundary.valid = as.integer((st_intersects(x, z, sparse = FALSE))))} #if does intersect whole polygon, TRUE
                           })
length(transect.pts.inner) #4,339

#identify transect IDs that intersect with boundary or are outside of PA
bad.ID <- map(transect.pts.inner, 
              function(x){unique((x %>% filter((inner.valid == 0 | boundary.valid == 0),))$transectID)})  #checked. 

#update transects
transect.pts <- map2(transect.pts, bad.ID, function(x,y){x %>% filter(!(transectID %in% y))}) #delete entire transect if any point is bad
length(transect.pts) #4,339
transect.good <- which(lapply(transect.pts, nrow) > 0) #find PAs with at least 1 transect
length(transect.good) #3789 
transect.pts <- transect.pts[transect.good] #filter to only PAs with good transects 
length(transect.pts) #3789

rm(transect.pts.inner, transect.good, bad.ID, bd.subset, transect.pts.bd)

#------------------------------------------------------
# Evaluation 2: Remove any points without info or with 
# more than 1 unique polygon after spatial join 
#------------------------------------------------------
#merge all lists into one multipolygon file
transect.join <- transect.pts %>% 
  data.table::rbindlist(., idcol = 'index') %>% #bind all lists with id index
  st_as_sf() %>% 
  unite(ID, index, transectID, sep = "_") #create unique ID for joining points with info
nrow(transect.join) #7,364,286 
remove(transect.pts)

# st_write((transect.join %>% filter(point_position == 0))[1:543330,],   paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_pt1.shp"))
# st_write((transect.join %>% filter(point_position == 0))[543331:1086660,],  paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_pt2.shp"))
# st_write((transect.join %>% filter(point_position == 0))[1086661:1629990,],  paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_pt3.shp"))
# st_write((transect.join %>% filter(point_position == 0))[1629991:1931875,],  paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_pt4.shp"))
# st_write((transect.join %>% filter(point_position == 0))[1931876:2454762,],  paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_pt5.shp"))


## spatial join in ArcGIS because of low computational power here
bd.pts.w.info1 <- st_read(paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_wInfo_pt1.shp"))
bd.pts.w.info2 <- st_read(paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_wInfo_pt2.shp"))
bd.pts.w.info3 <- st_read(paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_wInfo_pt3.shp"))
bd.pts.w.info4 <- st_read(paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_wInfo_pt4.shp"))
bd.pts.w.info5 <- st_read(paste0("./data/GLOBAL/sensitivity_analysis/eva2_", transect_unit*transect_pts*2/1000, "km_transects_bd_pts_wInfo_pt5.shp"))
bd.pts.w.info <- rbind(bd.pts.w.info1, bd.pts.w.info2, 
                       bd.pts.w.info3, bd.pts.w.info4, bd.pts.w.info5) %>%
     dplyr::select(-Join_Count, -TARGET_FID, -JOIN_FID)

nrow(bd.pts.w.info) # 2,745,694
length(unique(bd.pts.w.info$WDPA_PID))  #6,132 PAs original, 6808 now. 

rm(bd.pts.w.info1, bd.pts.w.info2, bd.pts.w.info3, bd.pts.w.info4, bd.pts.w.info5)

#if a boundary point is within 750m of 2 parks delete it
rep.id <- bd.pts.w.info[which(duplicated(bd.pts.w.info$ID)),]$ID
length(rep.id) # 302,763

#remove transects with repeated id because we don't know which park it belongs to.
bd.pts.w.info <- bd.pts.w.info %>% filter(!(ID %in% rep.id))
nrow(bd.pts.w.info)  # 2,193,378  transects

#identify which IDs didn't join to a WDPA_PID (n = 1,130,139)
bad.ID <- unique((bd.pts.w.info %>% filter(is.na(WDPA_PID)))$ID) 
length(bad.ID)  # 0 

#remove points that didn't join to a nearby WDPA_PID
bd.pts.w.info <- bd.pts.w.info %>% filter(!(ID %in% bad.ID))

nrow(bd.pts.w.info)  # 2,193,378 
length(unique(bd.pts.w.info$WDPA_PID)) # 5810 unique PAs

# find WDPAPID that has multiple polygons - rename the transects so that each of them is unique
bd.pts.w.info <- bd.pts.w.info %>% group_by(WDPA_PID) %>% mutate (trnscID_new = row_number())

#turn bd points with info into a dataframe
bd.pts.w.info.df <- as.data.frame(bd.pts.w.info) %>% dplyr::select(-geometry)
nrow(bd.pts.w.info.df) #2,193,378 

#delete the rest of transect if the boundary point was deleted
transect.join <- transect.join %>% filter((ID %in% unique(bd.pts.w.info$ID))) 
nrow(transect.join) # 6,580,134  (filtered from 7,364,286)

rm(bad.ID, rep.id)

#------------------------------------------------------
# Export
#------------------------------------------------------
#join point coordinates to attribute dataframe so each point has the attribute data
transect.pts.w.info <- as.data.frame(cbind(transect.join, st_coordinates(transect.join))) %>%           
  dplyr::select(-geometry) %>% 
  left_join(bd.pts.w.info.df, by = c("ID" = "ID"))
nrow(transect.pts.w.info) # 6,580,134
rm(bd.pts.w.info.df, bd.pts.w.info)

#organize info
transect.pts.w.info <- transect.pts.w.info %>% 
  separate(ID, c("index", "transectID")) %>%
  group_by(WDPA_PID) %>% 
  dplyr::select(WDPA_PID, trnscID_new, point_position, X, Y, 
                BIOME_NAME, ISO3, GIS_AREA)  %>% ######
rename(trnscID = trnscID_new, BIOME = BIOME_NAME) #select attributes  
nrow(transect.pts.w.info)  # 6,580,134

#create point-only spatial dataframe for GEE
transect.pts <- transect.pts.w.info %>% 
  dplyr::select(WDPA_PID, trnscID, point_position, X, Y) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(target.crs)
nrow(transect.pts) # 6,580,134
length(unique(transect.pts$WDPA_PID)) # 5810

# export points with all info for statistic analysis later
write_csv(transect.pts.w.info, paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "km_w_info.csv"))

# export points without info for GEE water filtering
# this is because we are too poor to afford google cloud storage. If you have access to GCS, feel free to do this in ONE step! :) 
# from here we uploaded these files to GEE. But this can also be done using R package 'rgee'
st_write(transect.pts[1:1000000,],  paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt1.shp"))
st_write(transect.pts[(1000001:2000000),],  paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt2.shp"))
st_write(transect.pts[(2000001:3000000),],  paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt3.shp"))
st_write(transect.pts[(3000001:4000000),],  paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt4.shp"))
st_write(transect.pts[(4000001:5000000),], paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt5.shp"))
st_write(transect.pts[(5000001:6000000),], paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt6.shp"))
st_write(transect.pts[(6000001:6580134),], paste0("./result/sensitivity_analysis/gee_transects_", transect_unit*transect_pts*2/1000, "_pt7.shp"))

#------------------------------------------------------
# END CODE
#------------------------------------------------------
