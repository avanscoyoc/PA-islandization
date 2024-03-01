library(sf)
library(tidyverse)
library(hrbrthemes)
target.crs <- st_crs("ESRI:54009")
n_transect_points <- 5  # how many points along one transect?

### step 0: reduce file size ----------------
# Read GEE results, simplify the table and save them by getting rid of the long geom info
path <- paste0(getwd(), "/result/raw_gee_output/")
files <- dir(path = path, recursive = TRUE, pattern = "*\\.csv") #get all files in directory

for (x in files) {
  paste0(path, x) %>%
    read_csv(., col_types = cols(WDPA_PI = col_character())) %>%
    select(WDPA_PI, trnscID, pnt_pst, year, first) %>%
    arrange(WDPA_PI, trnscID, year, pnt_pst) %>%
    write_csv(., paste0("result/sim_gee_output/", x))
  }

### step 1: Organization and filter water. ------------
## Read GEE results and filter out transects that do not have extactly 5 points
path <- paste0(getwd(), "/result/sim_gee_output/")
files <- dir(path = path, recursive = TRUE, pattern = "*\\.csv") #get all files in directory
df <- map(files, function(x) {
   paste0(path, x) %>%
     read_csv(., col_types = cols(WDPA_PI = col_character()))
}) %>% do.call(rbind,.)
nrow(df)# 180,092,020 records total

# delete the transects do not have all 5 points. Using 2020 as the standard year
df <- df %>% filter (year == 2020) %>% 
  group_by(WDPA_PI, trnscID) %>% 
  summarise(n = n()) %>% 
  filter (n == n_transect_points) %>% 
  left_join(df)
nrow(df) #206,182,600
length(unique(df$WDPA_PI)) #5269. 5330 - 5269 based by PAs surrounded by water
write_csv(df, "result/GLOBAL_sim_transects_landOnly.csv")