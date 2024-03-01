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
  transect_pts = 2 # number of points on each side of boundary point
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
  
  # # if using a very good machine 
  # ori.bd.pts.w.info <- st_join(transect.pts.bd, pg, #get boundary points
  #                           join = st_is_within_distance,
  #                           dist = 750)  #updated because many WDPA_PIDs eliminated @750m
  
  # # if not so good machine, perform spatial join in ArcGIS then read in dataframe again
  # st_write(transect.pts.bd[1:620000,],  "./data/GLOBAL/ori_transects_bd_pts_pt1.shp")
  # st_write(transect.pts.bd[620001:1240000,],  "./data/GLOBAL/ori_transects_bd_pts_pt2.shp")
  # st_write(transect.pts.bd[1240001:1860000,],  "./data/GLOBAL/ori_transects_bd_pts_pt3.shp")
  # st_write(transect.pts.bd[1860001:2480000,],  "./data/GLOBAL/ori_transects_bd_pts_pt4.shp")
  # st_write(transect.pts.bd[2480001:3136704,],  "./data/GLOBAL/ori_transects_bd_pts_pt5.shp")
  # --- operate in arcgis (add parameters --------------------------------------------------) ---- #
  # ori.bd.pts.w.info1 <- st_read("./data/GLOBAL/ori_transects_bd_pts_wInfo_pt1.shp")
  # ori.bd.pts.w.info2 <- st_read("./data/GLOBAL/ori_transects_bd_pts_wInfo_pt2.shp")
  # ori.bd.pts.w.info3 <- st_read("./data/GLOBAL/ori_transects_bd_pts_wInfo_pt3.shp")
  # ori.bd.pts.w.info4 <- st_read("./data/GLOBAL/ori_transects_bd_pts_wInfo_pt4.shp")
  # ori.bd.pts.w.info5 <- st_read("./data/GLOBAL/ori_transects_bd_pts_wInfo_pt5.shp")
  # ori.bd.pts.w.info <- rbind(ori.bd.pts.w.info1, ori.bd.pts.w.info2, ori.bd.pts.w.info3, ori.bd.pts.w.info4, ori.bd.pts.w.info5) %>% 
  #   dplyr::select(-Join_Count, -TARGET_FID, JOIN_FID)
  # rm(ori.bd.pts.w.info1, ori.bd.pts.w.info2, ori.bd.pts.w.info3, ori.bd.pts.w.info4, ori.bd.pts.w.info5)
  # write_csv(ori.bd.pts.w.info, "./result/GLOBAL_ori_transects_w_info.csv")
  
  ori.bd.pts.w.info <- read_csv("./result/GLOBAL_ori_transects_w_info.csv" , col_types = cols(WDPA_PID = col_character()))  %>% 
    dplyr::select(-Join_Count, -TARGET_FID, JOIN_FID)
  
  ori.bd.pts.sum <- ori.bd.pts.w.info %>% filter(!is.na(WDPA_PID)) %>% group_by(WDPA_PID) %>% summarise(ntrsct = n())
  st_geometry(ori.bd.pts.sum) <- NULL
  write_csv(ori.bd.pts.sum, "./result/GLOBAL_ori_transects_count.csv")
  rm(ori.bd.pts.w.info, ori.bd.pts.sum)

#------------------------------------------------------
# Evaluation 1: Remove inner points that intersect with 
# the inner buffer or are outside of the PA boundary
#------------------------------------------------------
  #create boundary inner buffer
  bd.inner.buffer <- map(bd.subset, function(x){st_buffer(x, -buffer_dist)}) #5500m inside boundary
  
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
  length(transect.good) #3,174
  transect.pts <- transect.pts[transect.good] #filter to only PAs with good transects 
  length(transect.pts) #3,174
  
  rm(transect.pts.inner, transect.good, bad.ID, bd.subset)

#------------------------------------------------------
# Evaluation 2: Remove any points without info or with 
# more than 1 unique polygon after spatial join 
#------------------------------------------------------
  #merge all lists into one multipolygon file
  transect.join <- transect.pts %>% 
    data.table::rbindlist(., idcol = 'index') %>% #bind all lists with id index
    st_as_sf() %>% 
    unite(ID, index, transectID, sep = "_") #create unique ID for joining points with info
  nrow(transect.join) #10,866,600 (2,173,320 transects)
  
  #### get remaining transects with PA attributes
  # if high-capacity computer 
  # bd.pts.w.info <- st_join((transect.join %>% 
  #                             filter(point_position == 0)), pg, 
  #                          join = st_is_within_distance, 
  #                          dist = 750) 
  # # # # if not so good machine, perform spatial join in ArcGIS then read in dataframe again
  # st_write((transect.join %>% filter(point_position == 0))[1:543330,],  "./data/GLOBAL/eva2_transects_bd_pts_pt1.shp")
  # st_write((transect.join %>% filter(point_position == 0))[543331:1086660,],  "./data/GLOBAL/eva2_transects_bd_pts_pt2.shp")
  # st_write((transect.join %>% filter(point_position == 0))[1086661:1629990,],  "./data/GLOBAL/eva2_transects_bd_pts_pt3.shp")
  # st_write((transect.join %>% filter(point_position == 0))[1629991:2173320,],  "./data/GLOBAL/eva2_transects_bd_pts_pt4.shp")
  # bd.pts.w.info1 <- st_read("./data/GLOBAL/eva2_transects_bd_pts_wInfo_pt1.shp")
  # bd.pts.w.info2 <- st_read("./data/GLOBAL/eva2_transects_bd_pts_wInfo_pt2.shp")
  # bd.pts.w.info3 <- st_read("./data/GLOBAL/eva2_transects_bd_pts_wInfo_pt3.shp")
  # bd.pts.w.info4 <- st_read("./data/GLOBAL/eva2_transects_bd_pts_wInfo_pt4.shp")
  # bd.pts.w.info <- rbind(bd.pts.w.info1, bd.pts.w.info2, bd.pts.w.info3, bd.pts.w.info4) %>% 
  #      dplyr::select(-Join_Count, -TARGET_FID, JOIN_FID)
  # rm(bd.pts.w.info1, bd.pts.w.info2, bd.pts.w.info3, bd.pts.w.info4)
   
  nrow(bd.pts.w.info) #2,449,742 transects
  length(unique(bd.pts.w.info$WDPA_PID))  #6,281 PAs
  
  #if a boundary point is within 750m of 2 parks delete it (n = 478) <- parks or transects 
  rep.id <- bd.pts.w.info[which(duplicated(bd.pts.w.info$ID)),]$ID
  length(rep.id) #282,309
 
  #remove transects with repeated id because we don't know which park it belongs to.
  bd.pts.w.info <- bd.pts.w.info %>% filter(!(ID %in% rep.id))
  nrow(bd.pts.w.info) #1,934,806 transects
  
  #identify which IDs didn't join to a WDPA_PID (n = 1,130,139)
  bad.ID <- unique((bd.pts.w.info %>% filter(is.na(WDPA_PID)))$ID) 
  length(bad.ID) # 5,017
  #remove points that didn't join to a nearby WDPA_PID
  bd.pts.w.info <- bd.pts.w.info %>% filter(!(ID %in% bad.ID))
  
  nrow(bd.pts.w.info) #r1 1,164,525 #r2 1,429,954 #r-final 1,929,789
  length(unique(bd.pts.w.info$WDPA_PID)) #r1 2689 parks   #r2 3374 parks  #r-final 5,330
  
  # find WDPAPID that has multiple polygons - rename the transects so that each of them is unique
  bd.pts.w.info <- bd.pts.w.info %>% group_by(WDPA_PID) %>% mutate (trnscID_new = row_number())
  
  #turn bd points with info into a dataframe
  bd.pts.w.info.df <- as.data.frame(bd.pts.w.info) %>% dplyr::select(-geometry)
  nrow(bd.pts.w.info.df) #1,929,789
  
  #delete the rest of transect if the boundary point was deleted
  transect.join <- transect.join %>% filter((ID %in% unique(bd.pts.w.info$ID))) 
  nrow(transect.join) #9,648,945
  
  rm(bad.ID, rep.id)

#------------------------------------------------------
# Export
#------------------------------------------------------
  #join point coordinates to attribute dataframe so each point has the attribute data
  transect.pts.w.info <- as.data.frame(cbind(transect.join, st_coordinates(transect.join))) %>%           
    dplyr::select(-geometry) %>% 
    left_join(bd.pts.w.info.df, by = c("ID" = "ID"))
  nrow(transect.pts.w.info) #9,648,945
  #rm(bd.pts.w.info.df, bd.pts.w.info)
  
  #organize info
  transect.pts.w.info <- transect.pts.w.info %>% 
    separate(ID, c("index", "transectID")) %>%
    group_by(WDPA_PID) %>% 
    dplyr::select(WDPA_PID, trnscID_new, point_position, X, Y, 
                  NAME, DESIG, DESIG_TYPE, IUCN_CAT, BIOME_NAME, 
                  STATUS, STATUS_YR, GOV_TYPE, OWN_TYPE, ISO3, GIS_AREA, PERIMETER)  %>% ######
    rename(trnscID = trnscID_new, BIOME = BIOME_NAME) #select attributes  
  nrow(transect.pts.w.info) #r1 5,822,625  #r2 6,443,860 #r-final 9,648,945
  
  #create point-only spatial dataframe for GEE
  transect.pts <- transect.pts.w.info %>% 
    dplyr::select(WDPA_PID, trnscID, point_position, X, Y) %>% 
    st_as_sf(coords = c("X", "Y")) %>% 
    st_set_crs(target.crs)
  nrow(transect.pts) #9,648,945
  length(unique(transect.pts$WDPA_PID)) #5330
  
  # export points with all info for statistic analysis later
  write_csv(transect.pts.w.info, "./result/gee_transects_w_info.csv") 
  
  # export points without info for GEE water filtering
  # this is because we are too poor to afford google cloud storage. If you have access to GCS, feel free to do this in ONE step! :) 
  # from here we uploaded these files to GEE. But this can also be done using R package 'rgee'
  st_write(transect.pts[1:1072105,], "./data/GLOBAL/gee_transects_pt1.shp")
  st_write(transect.pts[(1072106:1608158),], "./data/GLOBAL/gee_transects_pt2_1.shp") 
  st_write(transect.pts[(1608159:2144210),], "./data/GLOBAL/gee_transects_pt2_2.shp") 
  st_write(transect.pts[(2144211:3216315),], "./data/GLOBAL/gee_transects_pt3.shp")
  st_write(transect.pts[(3216316:4288420),], "./data/GLOBAL/gee_transects_pt4.shp")
  st_write(transect.pts[(4288421:5360525),], "./data/GLOBAL/gee_transects_pt5.shp")
  st_write(transect.pts[(5360526:6432630),], "./data/GLOBAL/gee_transects_pt6.shp")
  st_write(transect.pts[(6432631:7504735),], "./data/GLOBAL/gee_transects_pt7.shp")
  st_write(transect.pts[(7504736:8576840),], "./data/GLOBAL/gee_transects_pt8.shp")
  st_write(transect.pts[(8576841:9000000),], "./data/GLOBAL/gee_transects_pt9_1.shp")
  st_write(transect.pts[(9000001:9648945),], "./data/GLOBAL/gee_transects_pt9_2.shp")
  
#------------------------------------------------------
# END CODE
#------------------------------------------------------
