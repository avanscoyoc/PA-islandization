library(tidyverse)

clean <- function(df) {
  df <- df %>% 
    mutate(BIOME = case_when(                                           
      BIOME %in% c("Deserts & Xeric Shrublands") ~ "Desert",
      BIOME %in% c("Tropical & Subtropical Coniferous Forests",
                   "Tropical & Subtropical Coniferous Forests",
                   "Tropical & Subtropical Moist Broadleaf Forests",
                   "Tropical & Subtropical Dry Broadleaf Forests") ~ "Tropical-Forests",
      BIOME %in% c("Mediterranean Forests, Woodlands & Scrub",
                   "Temperate Conifer Forests",
                   "Temperate Broadleaf & Mixed Forests") ~ "Temperate-Forests",
      BIOME %in% c("Boreal Forests/Taiga") ~ "Boreal-Forests",
      BIOME %in% c("Tropical & Subtropical Grasslands, Savannas & Shrublands",
                   "Temperate Grasslands, Savannas & Shrublands",
                   "Montane Grasslands & Shrublands",
                   "Flooded Grasslands & Savannas"
      ) ~ "Grassland & shrubland",
      (BIOME == "N/A" | is.na(BIOME)) ~ "Rock & Ice",
      TRUE ~ BIOME)) 
  return(df)
}

#### --------------- obtain PA ID that has at least 30% evaluted ---------------- ##### 
df_5km <- read_csv("result/sensitivity_analysis/GLOBAL_edge_level_sens_5km_w_info.csv", 
                   col_types = cols(WDPA_PID = col_character())) %>% clean(.)  #nrow 5810
df_10km <- read_csv("result/GLOBAL_edge_all_results.csv", 
                    col_types = cols(WDPA_PID = col_character())) %>% clean(.) # nrow 5330
df_15km <- read_csv("result/sensitivity_analysis/GLOBAL_edge_level_sens_15km_w_info.csv", 
                              col_types = cols(WDPA_PID = col_character())) %>% clean(.) #nrow 4223

# how many transects from each transect length are evaluated for at least 30%
ID_5km <- (df_5km %>% filter(perc_evaluated_2020 >= 0.3))$WDPA_PID # n = 5096
ID_10km <- (df_10km %>% filter(perc_evaluated_2020 >= 0.3))$WDPA_PID # n = 4471 
ID_15km <- (df_15km %>% filter(perc_evaluated_2020 >= 0.3))$WDPA_PID # n = 4223

#######################################################################################
############ ###### ###### ###### ###### ###### ###### 5km  ################## ########
#######################################################################################
#### --------------- summarize at global level ------------ #####
transects_5km <- read_csv("result/sensitivity_analysis/GLOBAL_sim_sens_5km_landOnly.csv", 
               col_types = cols(WDPA_PI = col_character())) %>% filter(WDPA_PID %in% ID_5km) 

transects_5km <- transects_5km %>% dplyr::select(-n) %>% pivot_wider(names_from = pnt_pst, values_from = first) 
names(transects_5km) <- c("WDPA_PID", "trnscID", "year", "inner1", "bd", "outer1" )
# if boundary gradient is larger than one of the inners and one of the outers, edge exists.
transects_5km <- transects_5km  %>% mutate(edge = ifelse(( bd > inner1) & (bd > outer1), 1, 0)) 

## merge PA info 
transects_5km.w.info <- transects_5km %>% left_join(df_5km %>% select(WDPA_PID, BIOME), by = c("WDPA_PID" = "WDPA_PID")) 

rm(transects_5km)

length(unique(transects_5km.w.info$WDPA_PID)) # n PID = 5770
ntrnsc = nrow(transects_5km.w.info)/20 #1,807,170

# # of transects by BIOME
ntrnsc.biome <- transects_5km.w.info %>% group_by(year, BIOME) %>% summarise (n = n()) %>% ungroup() %>%
  filter(year == 2020) %>% select(-year)
# BIOME                      n
# <chr>                  <int>
#   1 Boreal-Forests        223938
# 2 Desert                213510
# 3 Grassland & shrubland 488138
# 4 Mangroves               2273
# 5 Rock & Ice              6292
# 6 Temperate-Forests     324837
# 7 Tropical-Forests      468742
# 8 Tundra                 79440

# % transect show edge in 2020 per biome 
transects_5km.w.info %>%filter (year == 2020) %>% 
  group_by(year, BIOME) %>% 
  filter(edge == 1) %>% 
  summarise (n_edge = n()) %>% 
  left_join(ntrnsc.biome) %>%
  mutate(perc_edge = n_edge/n)
# year BIOME                 n_edge      n perc_edge
# <dbl> <chr>                  <int>  <int>     <dbl>
#   1  2020 Boreal-Forests         71851 223938     0.321
# 2  2020 Desert                 67528 213510     0.316
# 3  2020 Grassland & shrubland 160585 488138     0.329
# 4  2020 Mangroves                577   2273     0.254
# 5  2020 Rock & Ice              1840   6292     0.292
# 6  2020 Temperate-Forests     103396 324837     0.318
# 7  2020 Tropical-Forests      150938 468742     0.322
# 8  2020 Tundra                 24880  79440     0.313

# -- total percent edge ---- # 
nrow(transects_5km.w.info %>%filter (year == 2020) %>% filter(edge == 1))/ntrnsc
#_--- 

#######################################################################################
############ ###### ###### ###### ###### ###### ##### 10km  ################## ########
#######################################################################################
transects_10km <- read_csv("result/GLOBAL_sim_transects_landOnly.csv",
                           col_types = cols(WDPA_PI = col_character())) %>% filter(WDPA_PI %in% ID_10km) %>% rename ("WDPA_PID" = "WDPA_PI")
transects_10km <- transects_10km %>% dplyr::select(-n) %>% pivot_wider(names_from = pnt_pst, values_from = first) 
names(transects_10km) <- c("WDPA_PI", "trnscID", "year", "inner2", "inner1", "bd", "outer1","outer2" )
# if boundary gradient is larger than one of the inners and one of the outers, edge exists.
transects_10km.w.info <- transects_10km  %>% 
  mutate(edge = ifelse(( bd > inner2) & (bd > outer2), 1, 0))  %>%  # <- only evaluate the 10km pair 
  left_join(df_10km %>% select(WDPA_PID, BIOME), by = c("WDPA_PI" = "WDPA_PID")) 

length(unique(transects_10km.w.info$WDPA_PID)) # n PID = 4471
ntrnsc = nrow(transects_10km.w.info)/20 #1,516,412

# # of transects by BIOME
ntrnsc.biome <- transects_10km.w.info %>% group_by(year, BIOME) %>% summarise (n = n()) %>% ungroup() %>%
  filter(year == 2020) %>% select(-year)
# # A tibble: 8 × 2
# BIOME                      n
# <chr>                  <int>
#   1 Boreal-Forests        171101
# 2 Desert                188581
# 3 Grassland & shrubland 425980
# 4 Mangroves                885
# 5 Rock & Ice              5874
# 6 Temperate-Forests     256004
# 7 Tropical-Forests      410630
# 8 Tundra                 57357

# % transect show edge in 2020 per biome 
transects_10km.w.info %>%filter (year == 2020) %>% 
  group_by(year, BIOME) %>% 
  filter(edge == 1) %>% 
  summarise (n_edge = n()) %>% 
  left_join(ntrnsc.biome) %>%
  mutate(perc_edge = n_edge/n)
# year BIOME                 n_edge      n perc_edge
# <dbl> <chr>                  <int>  <int>     <dbl>
#   1  2020 Boreal-Forests         56240 171101     0.329
# 2  2020 Desert                 62645 188581     0.332
# 3  2020 Grassland & shrubland 152522 425980     0.358
# 4  2020 Mangroves                342    885     0.386
# 5  2020 Rock & Ice              1655   5874     0.282
# 6  2020 Temperate-Forests      84206 256004     0.329
# 7  2020 Tropical-Forests      141284 410630     0.344
# 8  2020 Tundra                 17884  57357     0.312

# -- total percent edge ---- # 
nrow(transects_10km.w.info %>%filter (year == 2020) %>% filter(edge == 1))/ntrnsc

rm(transects_15km)

#######################################################################################
############ ###### ###### ###### ###### ###### ##### 15km  ################## ########
#######################################################################################
transects_15km <- read_csv("result/sensitivity_analysis/GLOBAL_sim_sens_15km_landOnly.csv", 
                          col_types = cols(WDPA_PI = col_character())) %>% filter(WDPA_PI %in% ID_15km) 
transects_15km <- transects_15km %>% dplyr::select(-n) %>% pivot_wider(names_from = pnt_pst, values_from = first) 
names(transects_15km) <- c("WDPA_PID", "trnscID", "year", "inner1", "bd", "outer1" )
# if boundary gradient is larger than one of the inners and one of the outers, edge exists.
transects_15km.w.info <- transects_15km  %>% 
  mutate(edge = ifelse(( bd > inner1) & (bd > outer1), 1, 0))  %>% 
  left_join(df_15km %>% select(WDPA_PID, BIOME), by = c("WDPA_PID" = "WDPA_PID")) 

length(unique(transects_15km.w.info$WDPA_PID)) # n PID = 5770
ntrnsc = nrow(transects_15km.w.info)/20 #1,807,170

# # of transects by BIOME
ntrnsc.biome <- transects_15km.w.info %>% group_by(year, BIOME) %>% summarise (n = n()) %>% ungroup() %>%
  filter(year == 2020) %>% select(-year)
# BIOME                      n
# <chr>                  <int>
#   1 Boreal-Forests        160071
# 2 Desert                170866
# 3 Grassland & shrubland 382576
# 4 Mangroves                527
# 5 Rock & Ice              5880
# 6 Temperate-Forests     200566
# 7 Tropical-Forests      375208
# 8 Tundra                 65538

# % transect show edge in 2020 per biome 
transects_15km.w.info %>%filter (year == 2020) %>% 
  group_by(year, BIOME) %>% 
  filter(edge == 1) %>% 
  summarise (n_edge = n()) %>% 
  left_join(ntrnsc.biome) %>%
  mutate(perc_edge = n_edge/n)
# year BIOME                 n_edge      n perc_edge
# <dbl> <chr>                  <int>  <int>     <dbl>
#   1  2020 Boreal-Forests         53467 160071     0.334
# 2  2020 Desert                 57217 170866     0.335
# 3  2020 Grassland & shrubland 139701 382576     0.365
# 4  2020 Mangroves                235    527     0.446
# 5  2020 Rock & Ice              1672   5880     0.284
# 6  2020 Temperate-Forests      66891 200566     0.334
# 7  2020 Tropical-Forests      133231 375208     0.355
# 8  2020 Tundra                 21307  65538     0.325

# -- total percent edge ---- # 
nrow(transects_15km.w.info %>%filter (year == 2020) %>% filter(edge == 1))/ntrnsc

rm(transects_15km)
