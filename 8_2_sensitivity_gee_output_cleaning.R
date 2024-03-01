library(sf)
library(tidyverse)
library(hrbrthemes)
target.crs <- st_crs("ESRI:54009")

# transect_unit = 7500 # distance between samples along a transect (meters)
# transect_pts = 1 # number of points on each side of boundary point
# n_transect_points <- 3  # how many points along one transect?

### step 0: reduce file size ----------------
# Read GEE results, simplify the table and save them by getting rid of the long geom info
path <- paste0(getwd(), "./result/sensitivity_analysis/raw_gee_output/")
files <- dir(path = path, recursive = TRUE, pattern = "*\\.csv") #get all files in directory

for (x in files) {
  paste0(path, x) %>%
    read_csv(., col_types = cols(WDPA_PI = col_character())) %>%
    select(WDPA_PI, trnscID, pnt_pst, year, first) %>%
    arrange(WDPA_PI, trnscID, year, pnt_pst) %>%
    write_csv(., paste0("./result/sensitivity_analysis/sim_gee_output/", x))
  }

### step 1: Organization and filter water. ------------
## Read GEE results and filter out transects that do not have extactly 3 points
path <- paste0(getwd(), "/result/sensitivity_analysis/sim_gee_output/")
files <- dir(path = path, recursive = TRUE, pattern = "*\\.csv") #get all files in directory

sens_15km <- files[1:5]
sens_5km <- files[6:12]

df_15km <- map(sens_15km, function(x) {
   paste0(path, x) %>%
     read_csv(., col_types = cols(WDPA_PI = col_character()))
}) %>% do.call(rbind,.)
nrow(df_15km) #  94,207,260

df_5km <- map(sens_5km, function(x) {
  paste0(path, x) %>%
    read_csv(., col_types = cols(WDPA_PI = col_character()))
}) %>% do.call(rbind,.)
nrow(df_5km) # 122,492,560

# delete the transects do not have all 5 points. Using 2020 as the standard year

df_15km <- df_15km %>% filter (year == 2020) %>% 
  group_by(WDPA_PI, trnscID) %>% 
  summarise(n = n()) %>% 
  filter (n == 3) %>% 
  left_join(df_15km)
nrow(df_15km) # 86,143,020
length(unique(df_15km$WDPA_PI)) # 4185 based by PAs surrounded by water
write_csv(df_15km, "result/sensitivity_analysis/GLOBAL_sim_sens_15km_landOnly.csv")

df_5km <- df_5km %>% filter (year == 2020) %>% 
  group_by(WDPA_PI, trnscID) %>% 
  summarise(n = n()) %>% 
  filter (n == 3) %>% 
  left_join(df_5km)
nrow(df_5km) #206,182,600
length(unique(df_5km$WDPA_PI)) #5269. 5330 - 5269 based by PAs surrounded by water
write_csv(df_5km, "result/sensitivity_analysis/GLOBAL_sim_sens_5km_landOnly.csv")
