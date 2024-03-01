#################################################################################################################################
####################### --- calculating PA level trend, generating data for figure 2 and figure 3b --- #########################
#################################################################################################################################

library(sf)
library(tidyverse)
library(hrbrthemes)
library(lme4)
library(lmerTest)

###### run linear regression to examine edge trend from 2001 to 2020 -------------- ##############
df.g <- read_csv("result/GLOBAL_sim_transects_landOnly.csv", col_types = cols(WDPA_PI = col_character())) %>% 
  filter(pnt_pst == 0) %>%  # taking only the boundary points 
  dplyr::select(-n, -pnt_pst) 
IDs <- unique(df.g$WDPA_PI)  # PA remained after removing water transects, n = 5264

trends <- data.frame()
for (ID in IDs) {
  df.g1 <- df.g %>% filter(WDPA_PI == ID) # %>% mutate (first = log(first))  ## ugh why did i do that. FEB 16 2023.
    m1 <- lm (first ~ year, df.g1)
    ceof1 <- data.frame(t(coef(summary(m1))["year",]))
    trends <- rbind(trends, cbind(WDPA_PI = ID, n_transects = length(unique(df.g1$trnscID)),ceof1[,1:2], df = NA, ceof1[,3:4]))
}

trends <- trends %>% mutate(Estimate = as.numeric(Estimate), Std.Err = as.numeric(Std..Error),
                            t.value = as.numeric(t.value), p.value = as.numeric(Pr...t..), WDPA_PID = WDPA_PI) %>%
  select(WDPA_PID, n_transects, Estimate, Std.Err, t.value, p.value)   # nrow = 5264 
# write_csv(trends,  "result/backup/GLOBAL_trends_AllSamples.csv")   # JUST FOR BACKUP, CAN BE SIMPLIFIED

wdpa_sum <- read_csv("result/GLOBAL_edge_level_2020_2001_w_info.csv")  # nrow = 5330
wdpa_sum <- wdpa_sum %>% left_join(trends, by = "WDPA_PID") # nrow = 5330. some PA no trends. 
write_csv(wdpa_sum, "result/GLOBAL_edge_all_results.csv")   # <----- this result combines with all earlier results, including edge presence and PA info.

### -------  create trend shapefiles for mapping -----###
wdpa_pg <- read_sf("data/GLOBAL/Global_wdpa_wInfo_June2021.shp") %>% filter(WDPA_PID %in% IDs) #"555697533" has been eliminated. n = 5268

wdpa <- wdpa_sum %>% 
  select(WDPA_PID, perc_evaluated_2020, edge_level_2020, 
         perc_evaluated_2001,edge_level_2001, Estimate, p.value) %>%
  rename(trend = Estimate)

trends.sp <- wdpa_pg %>% left_join(wdpa, by = "WDPA_PID") %>% filter(WDPA_PID %in% IDs)
write_sf(trends.sp, "result/GLOBAL_edge_all_results.shp")

# --- some quick stats to look at trends ---- ##
mu = data.frame(mean = mean(as.numeric(trends$Estimate), na.rm = T)) # 0.0154032. Feb 2023.
t.test(as.numeric(trends$Estimate)) # p<2.2e-16  # overall trend is significantly different from 0.

# EVERYTHING BELOW CAN BE DELETED #
# ######################################################################   
# ## -------------- simple plots showing slope distribution ---------- #
# ######################################################################
# #tiff(filename="visualization/Appendix_fig_slope.tif",height=8,width=11,units="cm",res=600,compression="lzw")
# ggplot(trends, aes(x = Estimate)) + 
#   geom_histogram(bins = 200, position = "dodge") + 
#   #geom_density() +
#   geom_vline(data = mu, aes(xintercept = mean, color = "red"), linetype = "dashed", size = 1) +
#   theme_ipsum() +
#   labs(fill="") +
#   xlab("Slope") +
#   xlim(-0.5, 0.5) +
#   theme(legend.position="none")
# #dev.off()
# 
# trends.summary <- trends %>% 
#   filter(!is.na(Estimate),p.value < 0.05) %>% 
#   select(WDPA_PID, Estimate) %>% 
#   mutate (group = "sig_only")
# 
# trends.summary <- rbind(trends.summary,(trends %>% 
#                                           filter(!is.na(Estimate)) %>% 
#                                           select(WDPA_PID, Estimate) %>% 
#                                           mutate(group = "all")))
# trends.summary  <- trends.summary  %>% mutate(group = replace(group, group == "sig_only", "significant estimate"))
# 
# #tiff(filename="visualization/Appendix_fig_slope_sigVSall.tif",height=8,width=11,units="cm",res=600,compression="lzw")
# ggplot(trends.summary , aes(x= as.numeric(Estimate), fill = group)) + 
#   geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', bins = 150, size = 0.1) +
#   scale_fill_manual(values=c("#69b3a2", "#404080")) +
#   theme_ipsum() +
#   labs(fill="") +
#   xlim(-0.5, 0.5) +
#   xlab("Slope Estimate") +
#   ylab("Counts") +
#   theme(legend.position="bottom", legend.title = element_blank())
# #dev.off()
