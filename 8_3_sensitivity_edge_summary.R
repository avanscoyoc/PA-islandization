##############################################################################################################
## this code summarizes edge situation for 2020 and 2001 and calculated percent transects evaluated for the two years.
## this code also creates null model to be compared to calculated results in 2020 and 2001 ###################
############################## generated data for Figure 5, figure S4 ########################################
##############################################################################################################

library(sf)
library(tidyverse)
library(hrbrthemes)

#######################################
########## for 15 km ##################
#######################################


### step 1: calculating transects numbers at each previous filtering step ------------
df <- read_csv( "result/sensitivity_analysis/GLOBAL_sim_sens_15km_landOnly.csv", col_types = cols(WDPA_PI = col_character())) %>% 
  rename (WDPA_PID = WDPA_PI)
nrow(df %>% filter(pnt_pst == 0, year == 2020)) # # of transects remained after removing water transects - 1,435,717
length(unique(df$WDPA_PID)) # # of PA remained after removing water transects - 4185

# Read transect file with PA info 
info.df <- read_csv("result/sensitivity_analysis/gee_transects_15km_w_info.csv", col_types = cols(WDPA_PID = col_character())) 
nrow(info.df %>% filter(point_position == 0)) # 1,686,993, # of transects evaluated before removing water transects, feb 2023
IDs <- unique(info.df$WDPA_PID) 
length(IDs) #4223 # of PAs evaluated before removing water transects 

## summarize # of transects for each WDPA_PID per step   
summary.df1 <- read_csv("result/GLOBAL_ori_transects_count.csv") %>% filter(WDPA_PID %in% IDs) # ORIGINAL TRANSECTS
#1 TO COUNT TRANSECTS AFTER FILTERING BASED ON LOCATION (SCRIPT 2), BASED ON DF INPUT OF GEE
summary.df2 <- info.df %>% filter(point_position == 0) %>%
  group_by(WDPA_PID) %>% summarise(ntrsct_2 = n())      
#2.1 TO COUNT TRANSECTS AFTER DELETING PONINTS THAT FALL IN WATER(2020), BASED ON DF OUTPUT OF GEE
summary.df3 <- df %>% filter(pnt_pst == 0, year == 2020) %>% group_by(WDPA_PID) %>% summarise(ntrsct_2020_eva = n()) 
summary.df <- summary.df1 %>% left_join(summary.df2) %>% left_join(summary.df3) 
summary.df[is.na(summary.df)] <- 0
#2.2 TO COUNT TRANSECTS AFTER DELETING PONINTS THAT FALL IN WATER (2001)
summary.df4 <- df %>% filter(pnt_pst == 0, year == 2001) %>% group_by(WDPA_PID) %>% summarise(ntrsct_2001_eva = n()) 
summary.df <- summary.df %>% left_join(summary.df4)
summary.df[is.na(summary.df)] <- 0
rm(summary.df1, summary.df2, summary.df3, summary.df4) 


##############################################################################################################
####################### ---calculating percentage boundary edge for 2020 and 2001--- #########################
##############################################################################################################
#### -------------- for year 2020 -----------------  #########
# df of boundary points
df.bd.2020 <- df %>% 
  filter(year == 2020, pnt_pst == 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst) %>% rename(pnt_pst0 = pnt_pst, first0 = first)
# df of non-boundary points
df.nbd.2020 <- df %>% 
  filter(year == 2020, pnt_pst != 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst)
# calculating diff gradient
df2020 <- df.nbd.2020 %>% left_join(df.bd.2020, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID")) %>% 
  select(- pnt_pst0) %>% mutate (gradient_diff = first0 - first) #gradient difference is boundary point - non-boundary point 
#If at least one pair of inter-outer points has a lower gradient than the boundary point, classify transect as an ‘edge’
df.inner <- df2020 %>% filter(pnt_pst <0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>% #select inner points with boundary gradient
  filter(sum > 0)
df.outer <- df2020 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>% #select outer points with boundary gradient
  filter(sum > 0)
df.inner.outer <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct() #select pairs with inner and outer points gradient lower than boundary (4 possible boundary pairs)

summary.2020 <- df.inner.outer %>% group_by(WDPA_PID) %>% summarise(ntrsct_2020_edge = n())
summary.df <- summary.df %>% left_join(summary.2020, by = "WDPA_PID")  %>% 
  mutate (perc_evaluated_2020 = ntrsct_2020_eva/ntrsct,        # to make sure what we CAN measure is representative of each PA
          edge_level_2020 = ntrsct_2020_edge/ntrsct_2020_eva)  # edge level represents edge presence among what we CAN measure

#### -------------- for year 2001 -----------------  #########
# df of boundary points
df.bd.2001 <- df %>% 
  filter(year == 2001, pnt_pst == 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst) %>% rename(pnt_pst0 = pnt_pst, first0 = first)
# df of non-boundary points
df.nbd.2001 <- df %>% 
  filter(year == 2001, pnt_pst != 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst)
# calculating diff gradient
df2001 <- df.nbd.2001 %>% left_join(df.bd.2001, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID")) %>% 
  select(- pnt_pst0) %>% mutate (gradient_diff = first0 - first) 
# yes edge if at least one pair of inner-outer points has lower gradient than boundary points   #<<< --- any good way to justify this?
df.inner <- df2001 %>% filter(pnt_pst <0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>%
  filter(sum > 0)
df.outer <- df2001 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>%
  filter(sum > 0)
df.inner.outer <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct()

summary.2001 <- df.inner.outer %>% group_by(WDPA_PID) %>% summarise(ntrsct_2001_edge = n()) 
summary.df <- summary.df %>% left_join(summary.2001, by = "WDPA_PID")  %>% 
  mutate (perc_evaluated_2001 = ntrsct_2001_eva/ntrsct, 
          edge_level_2001 = ntrsct_2001_edge/ntrsct_2001_eva)

# ---- merging the calculation with PA info and generate output ---- # 

df.summary.w.info <- left_join(summary.df, (info.df %>% select (-trnscID, -point_position, - X, -Y) %>% distinct()), by = "WDPA_PID")
summary.df[is.na(summary.df)] <- 0
write_csv(df.summary.w.info, "result/sensitivity_analysis/GLOBAL_edge_level_sens_15km_w_info.csv")
rm(df.inner, df.outer, df.inner.outer, df2001, df2020, summary.2020, summary.2001, df.summary.w.info)


#######################################
########## for 5 km ##################
#######################################

### step 1: calculating transects numbers at each previous filtering step ------------
df <- read_csv( "result/sensitivity_analysis/GLOBAL_sim_sens_5km_landOnly.csv", col_types = cols(WDPA_PI = col_character())) %>% 
  rename (WDPA_PID = WDPA_PI)
nrow(df %>% filter(pnt_pst == 0, year == 2020)) # # of transects remained after removing water transects -1,882,390
length(unique(df$WDPA_PID)) # # of PA remained after removing water transects - 5770

# Read transect file with PA info 
info.df <- read_csv("result/sensitivity_analysis/gee_transects_5km_w_info.csv", col_types = cols(WDPA_PID = col_character())) 
nrow(info.df %>% filter(point_position == 0)) # 2,193,378, # of transects evaluated before removing water transects, feb 2023
IDs <- unique(info.df$WDPA_PID) 
length(IDs) #5810 # of PAs evaluated before removing water transects 

## summarize # of transects for each WDPA_PID per step   
summary.df1 <- read_csv("result/GLOBAL_ori_transects_count.csv") %>% filter(WDPA_PID %in% IDs) # ORIGINAL TRANSECTS
#1 TO COUNT TRANSECTS AFTER FILTERING BASED ON LOCATION (SCRIPT 2), BASED ON DF INPUT OF GEE
summary.df2 <- info.df %>% filter(point_position == 0) %>%
  group_by(WDPA_PID) %>% summarise(ntrsct_2 = n())      
#2.1 TO COUNT TRANSECTS AFTER DELETING PONINTS THAT FALL IN WATER(2020), BASED ON DF OUTPUT OF GEE
summary.df3 <- df %>% filter(pnt_pst == 0, year == 2020) %>% group_by(WDPA_PID) %>% summarise(ntrsct_2020_eva = n()) 
summary.df <- summary.df1 %>% left_join(summary.df2) %>% left_join(summary.df3) 
summary.df[is.na(summary.df)] <- 0
#2.2 TO COUNT TRANSECTS AFTER DELETING PONINTS THAT FALL IN WATER (2001)
summary.df4 <- df %>% filter(pnt_pst == 0, year == 2001) %>% group_by(WDPA_PID) %>% summarise(ntrsct_2001_eva = n()) 
summary.df <- summary.df %>% left_join(summary.df4)
summary.df[is.na(summary.df)] <- 0
rm(summary.df1, summary.df2, summary.df3, summary.df4) 


##############################################################################################################
####################### ---calculating percentage boundary edge for 2020 and 2001--- #########################
##############################################################################################################
#### -------------- for year 2020 -----------------  #########
# df of boundary points
df.bd.2020 <- df %>% 
  filter(year == 2020, pnt_pst == 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst) %>% rename(pnt_pst0 = pnt_pst, first0 = first)
# df of non-boundary points
df.nbd.2020 <- df %>% 
  filter(year == 2020, pnt_pst != 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst)
# calculating diff gradient
df2020 <- df.nbd.2020 %>% left_join(df.bd.2020, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID")) %>% 
  select(- pnt_pst0) %>% mutate (gradient_diff = first0 - first) #gradient difference is boundary point - non-boundary point 
#If at least one pair of inter-outer points has a lower gradient than the boundary point, classify transect as an ‘edge’
df.inner <- df2020 %>% filter(pnt_pst <0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>% #select inner points with boundary gradient
  filter(sum > 0)
df.outer <- df2020 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>% #select outer points with boundary gradient
  filter(sum > 0)
df.inner.outer <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct() #select pairs with inner and outer points gradient lower than boundary (4 possible boundary pairs)

summary.2020 <- df.inner.outer %>% group_by(WDPA_PID) %>% summarise(ntrsct_2020_edge = n())
summary.df <- summary.df %>% left_join(summary.2020, by = "WDPA_PID")  %>% 
  mutate (perc_evaluated_2020 = ntrsct_2020_eva/ntrsct,        # to make sure what we CAN measure is representative of each PA
          edge_level_2020 = ntrsct_2020_edge/ntrsct_2020_eva)  # edge level represents edge presence among what we CAN measure

#### -------------- for year 2001 -----------------  #########
# df of boundary points
df.bd.2001 <- df %>% 
  filter(year == 2001, pnt_pst == 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst) %>% rename(pnt_pst0 = pnt_pst, first0 = first)
# df of non-boundary points
df.nbd.2001 <- df %>% 
  filter(year == 2001, pnt_pst != 0) %>% 
  select(WDPA_PID, trnscID, pnt_pst, first) %>% 
  arrange(WDPA_PID, trnscID, pnt_pst)
# calculating diff gradient
df2001 <- df.nbd.2001 %>% left_join(df.bd.2001, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID")) %>% 
  select(- pnt_pst0) %>% mutate (gradient_diff = first0 - first) 
# yes edge if at least one pair of inner-outer points has lower gradient than boundary points   #<<< --- any good way to justify this?
df.inner <- df2001 %>% filter(pnt_pst <0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>%
  filter(sum > 0)
df.outer <- df2001 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff > 0)) %>%
  filter(sum > 0)
df.inner.outer <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct()

summary.2001 <- df.inner.outer %>% group_by(WDPA_PID) %>% summarise(ntrsct_2001_edge = n()) 
summary.df <- summary.df %>% left_join(summary.2001, by = "WDPA_PID")  %>% 
  mutate (perc_evaluated_2001 = ntrsct_2001_eva/ntrsct, 
          edge_level_2001 = ntrsct_2001_edge/ntrsct_2001_eva)

# ---- merging the calculation with PA info and generate output ---- # 

df.summary.w.info <- left_join(summary.df, (info.df %>% select (-trnscID, -point_position, - X, -Y) %>% distinct()), by = "WDPA_PID")
summary.df[is.na(summary.df)] <- 0
write_csv(df.summary.w.info, "result/sensitivity_analysis/GLOBAL_edge_level_sens_5km_w_info.csv")
rm(df.inner, df.outer, df.inner.outer, df2001, df2020, summary.2020, summary.2001, df.summary.w.info)

