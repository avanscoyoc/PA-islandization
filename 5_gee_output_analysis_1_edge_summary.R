##############################################################################################################
## this code summarizes edge situation for 2020 and 2001 and calculated percent transects evaluated for the two years.
## this code also creates null model to be compared to calculated results in 2020 and 2001 ###################
############################## generated data for Figure 5, figure S4 ########################################
##############################################################################################################

library(sf)
library(tidyverse)
library(hrbrthemes)

### step 1: calculating transects numbers at each previous filtering step ------------
df <- read_csv("result/GLOBAL_sim_transects_landOnly.csv", col_types = cols(WDPA_PI = col_character())) %>% 
  rename (WDPA_PID = WDPA_PI)
nrow(df %>% filter(pnt_pst == 0, year == 2020)) # # of transects remained after removing water transects - 1,603,395 - feb 2023
length(unique(df$WDPA_PID)) # # of PA remained after removing water transects - 5269 - feb 2023

# Read transect file with PA info 
info.df <- read_csv("result/gee_transects_w_info.csv", col_types = cols(WDPA_PID = col_character())) 
nrow(info.df %>% filter(point_position == 0)) # 1,929,789, # of transects evaluated before removing water transects, feb 2023
IDs <- unique(info.df$WDPA_PID) 
length(IDs) #5330, # of PAs evaluated before removing water transects - feb 2023

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
write_csv(df.summary.w.info, "result/GLOBAL_edge_level_2020_2001_w_info.csv")
rm(df.inner, df.outer, df.inner.outer, df2001, df2020, summary.2020, summary.2001, df.summary.w.info)

##############################################################################################################
####################### -- ------------ COMPARE EDGE LEVEL 2001 AND 2020 ----------- #########################
##############################################################################################################
summary.df.p1 <- summary.df %>% filter(perc_evaluated_2020 >= 0.3) %>%
  select(WDPA_PID, edge_level_2001, edge_level_2020) %>% 
  pivot_longer(cols = c("edge_level_2001", "edge_level_2020"), names_to = "group")

#tiff(filename="visualization/EdgeLevel_2001_2020_70perc_0721.tif",height=11,width=11,units="cm",res=800,compression="lzw")
summary.df.p1 %>% mutate(group = factor(group, levels = c("edge_level_2001", "edge_level_2020"))) %>%
  ggplot( aes(x=value, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', bins = 50) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") +
  theme(legend.position="bottom")
#dev.off()

##############################################################################################################
####################### -- -----------------create null model  ----------------- #############################
##############################################################################################################
##### by randomizing internal-external point pairs by transects 
## (all internal and external will still be on the same transects) 
## within each WDPAPID, for each pnt_pst, randomize trnscID 

#### -------------- for year 2020 -----------------  #########

PID <- unique(df.bd.2020$WDPA_PID)
df.bd.null <- NULL
for (i in PID) {
  df.bd.i <- df.bd.2020 %>% filter(WDPA_PID == i)  # grab the transect of the park i
  n.i <- nrow(df.bd.i)                             # summarize # of transect for each PA
  df.bd.i <- df.bd.i %>% mutate (trnscID_null = sample(n.i)) #sampling randomly from the transect sets (reassign transect ID for boundary points)
  df.bd.null <- rbind(df.bd.null, df.bd.i)
}

df.null.2020 <- df.nbd.2020 %>% left_join(df.bd.null, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID_null")) %>%
  select(- pnt_pst0, -trnscID.y) %>% mutate(gradient_diff_null = first0 - first)

# write_csv(df.null.2020, "./result/GLOBAL_edge_level_2020_NULL.csv")
# df.null.2020 <- read_csv( "./result/GLOBAL_edge_level_2020_NULL.csv")

df.inner <- df.null.2020 %>% filter(pnt_pst < 0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff_null > 0)) %>%
  filter(sum > 0)
df.outer <- df.null.2020 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff_null > 0)) %>%
  filter(sum > 0)
df.inner.outer.null <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct()

summary.df.null.2020 <- df.inner.outer.null %>% group_by(WDPA_PID) %>% summarise(ntrsct_null_2020 = n())
summary.df <- summary.df %>% left_join(summary.df.null.2020, by = "WDPA_PID") 
summary.df[is.na(summary.df)] <- 0
summary.df <- summary.df %>% mutate (edge_level_2020_null2 = ntrsct_null_2020/ntrsct_2020_eva)

summary.df.p2 <- summary.df %>% filter(perc_evaluated_2020 >= 0.3) %>%
  select(WDPA_PID, edge_level_2020, edge_level_2020_null2) %>% 
  pivot_longer(cols = c("edge_level_2020", "edge_level_2020_null2"), names_to = "group")

####################### -- ---------------------- Figure S4 b  ------------------- ############################
#tiff(filename="./visualization/0APPENDIX/Appendix_fig_2020vsNULL.tif",height=11,width=11,units="cm",res=800,compression="lzw")
summary.df.p2 %>% 
  mutate(group = ifelse( group == "edge_level_2020", "measured", "null model")) %>%
  mutate(group = factor(group, levels = c("null model", "measured"))) %>%
  ggplot( aes(x=value, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', bins = 50) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") +
  ylim(0, 500) +
  theme(legend.position="bottom")
#dev.off()

t.test(summary.df.p2[summary.df.p2$group == "edge_level_2020",]$value, summary.df.p2[summary.df.p2$group == "edge_level_2020_null2",]$value, paired = T)
# Welch Two Sample t-test  --- March 2023 --- ##
# data:  summary.df.p2[summary.df.p2$group == "edge_level_2020", ]$value and summary.df.p2[summary.df.p2$group == "edge_level_2020_null2", ]$value
# t = 19.549, df = 8812.4, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.04005416 0.04898214
# sample estimates:
#   mean of x mean of y 
# 0.5144994 0.4699813 

rm(df.null.2020, summary.df.p2, summary.df.null.2020)


#### -------------- for year 2001 -----------------  #########
#within each WDPAPID, for each pnt_pst, randomize trnscID 
PID <- unique(df.bd.2001$WDPA_PID) #n=5264
df.bd.null <- NULL
for (i in PID) {
  df.bd.i <- df.bd.2001 %>% filter(WDPA_PID == i)
  n.i <- nrow(df.bd.i)
  df.bd.i <- df.bd.i %>% mutate (trnscID_null = sample(n.i))
  df.bd.null <- rbind(df.bd.null, df.bd.i)
}
df.null.2001 <- df.nbd.2001 %>% left_join(df.bd.null, by = c("WDPA_PID" = "WDPA_PID", "trnscID" = "trnscID_null")) %>%
 select(- pnt_pst0, -trnscID.y) %>% mutate (gradient_diff_null = first0 - first)
# write_csv(df.null.2001, "./result/GLOBAL_edge_level_2001_NULL.csv")
#df.null.2001 <- read_csv( "./result/GLOBAL_edge_level_2001_NULL.csv")

df.inner <- df.null.2001 %>% filter(pnt_pst <0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff_null > 0)) %>%
  filter(sum > 0)
df.outer <- df.null.2001 %>% filter(pnt_pst >0) %>% 
  group_by(WDPA_PID, trnscID) %>% mutate(sum = sum(gradient_diff_null > 0)) %>%
  filter(sum > 0)
df.inner.outer.null <- inner_join(df.inner, df.outer, by = c("WDPA_PID", "trnscID")) %>% select(WDPA_PID, trnscID) %>% distinct()

summary.df.null.2001 <- df.inner.outer.null %>% group_by(WDPA_PID) %>% summarise(ntrsct_null_2001 = n())
summary.df <- summary.df %>% left_join(summary.df.null.2001, by = "WDPA_PID") 
summary.df[is.na(summary.df)] <- 0
summary.df <- summary.df %>% mutate (edge_level_2001_null2 = ntrsct_null_2001/ntrsct_2001_eva)

summary.df.p2 <- summary.df %>% filter(perc_evaluated_2001 >= 0.3) %>%
  select(WDPA_PID, edge_level_2001, edge_level_2001_null2) %>% 
  pivot_longer(cols = c("edge_level_2001", "edge_level_2001_null2"), names_to = "group")


####################### -- ---------------------- Figure S4 ------------------- ############################

tiff(filename="./visualization/0APPENDIX/Appendix_fig_2001vsNULL.tif",height=11,width=11,units="cm",res=600,compression="lzw")
summary.df.p2 %>% 
  mutate(group = ifelse( group == "edge_level_2001", "measured", "null model")) %>%
  mutate(group = factor(group, levels = c("null model", "measured"))) %>%
  ggplot( aes(x=value, fill=group)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', bins = 50) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") +
  ylim(0,500) +
  theme(legend.position="bottom")
dev.off()

t.test(summary.df.p2[summary.df.p2$group == "edge_level_2001",]$value, summary.df.p2[summary.df.p2$group == "edge_level_2001_null2",]$value, paired = T)
rm(df.null.2001, summary.df.p2, summary.df.null.2001)

# ---- below can be deleted for simplification ----- #
# ##############################################################################################################
# ################### -- ---------------- -create and write result shps  ----------------- #####################
# ##############################################################################################################
# wdpa_pid_list <- summary.df$WDPA_PID #5330
# wdpa_pg <- read_sf("data/GLOBAL/Global_wdpa_wInfo_June2021.shp") %>% filter(WDPA_PID %in% wdpa_pid_list) #5330 polygons
# df.sp2020 <- summary.df %>% select(WDPA_PID, perc_evaluated_2020, edge_level_2020)
# df.sp2001 <- summary.df %>% select(WDPA_PID, perc_evaluated_2001, edge_level_2001)
# wdpa_pg2020 <- wdpa_pg %>% left_join(df.sp2020, by = "WDPA_PID")
# wdpa_pg2001 <- wdpa_pg %>% left_join(df.sp2001, by = "WDPA_PID")
# 
# write_sf(wdpa_pg2020, "result/GLOBAL_edge_level_2020_w__info.shp")    # <- used for creating figure 5
# # write_sf(wdpa_pg2001, "result/GLOBAL_edge_level_2001_w__info.shp")  # DID NOT USE, could be simplified.
