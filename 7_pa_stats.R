library(sf)
library(tidyverse)
library(viridis)

##--------------------------------------##
# Import edge data 
results <- read_csv("./result/GLOBAL_edge_all_results.csv", 
                    col_types = cols(WDPA_PID = col_character())) %>% 
           filter(perc_evaluated_2020 >= 0.3) #remove PAs with <30% of transects evaluated

# nrow(results)
# [1] 4471

# Import country data
country <- read_csv("https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv") %>% 
  select(ISO3 = Three_Letter_Country_Code)

##--------------------------------------##
# Append country, biome, and edge trend to dataset

results <- results %>% 
  #add country 
  left_join(., country, by = "ISO3") %>% 
  #add biome categories
  mutate(biome = case_when( 
    BIOME == "Mangroves" ~ "Mangrove", #rename Mangrove
    BIOME == "N/A" ~ "Rock & Ice", #rename Rock & Ice
    BIOME %in% c("Deserts & Xeric Shrublands") ~ "Desert", #Desert
    BIOME %in% c("Tropical & Subtropical Coniferous Forests",
                 "Tropical & Subtropical Coniferous Forests",
                 "Tropical & Subtropical Moist Broadleaf Forests",
                 "Tropical & Subtropical Dry Broadleaf Forests") ~ "Tropical-Forests", #Tropical Forest
    BIOME %in% c("Mediterranean Forests, Woodlands & Scrub",
                 "Temperate Conifer Forests",
                 "Temperate Broadleaf & Mixed Forests") ~ "Temperate-Forests", #Temperate Forest
    BIOME %in% c("Boreal Forests/Taiga") ~ "Boreal-Forests", #Boreal Forest
    BIOME %in% c("Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                 "Temperate Grasslands, Savannas & Shrublands",
                 "Montane Grasslands & Shrublands",
                 "Flooded Grasslands & Savannas") ~ "Grassland-Shrubland", #Grassland-Shrubland
    TRUE ~ BIOME)) %>%
  #add edge trend significance categories (for some PAs, if %evaluated was low, p-value not calculated)
  mutate(edge_trend = case_when((Estimate > 0 & p.value < 0.05) ~ "Significantly Increase", 
                                 (Estimate > 0 & p.value >= 0.05) ~ "Not Significant Increase",  
                                 (Estimate == 0) ~ "Stable", #no PAs have zero estimate
                                 (Estimate < 0 & p.value < 0.05) ~ "Significantly Decrease", 
                                 (Estimate < 0 & p.value >= 0.05) ~ "Not Significant Decrease",
                                 TRUE ~ "unknown")) %>% 
  distinct(WDPA_PID, .keep_all = TRUE) #remove duplicates created by cross-boundary ISO3s
  
##--------------------------------------##
# Descriptive results 

nrow(results) # Number of PAs                       #4471
n_distinct(results$ISO3) - 3 # Number of Countries  #145 
unique(results$biome) # Number of Biomes            #8
range(results$GIS_AREA) #range of PA area (km^2)    #200.4 km to 961,673.2 km
median(results$GIS_AREA) #median area (km^2)        #862.2 km2 
sd(results$GIS_AREA) #standard deviation area(km^2) #15793.39 km2
median(results$ntrsct_2020_eva) #median transects          #224
range(results$ntrsct_2020_eva) #range n_transects          #7 to 6328 
sd(results$ntrsct_2020_eva) #transect standard deviation   #391.6641 

##--------------------------------------##
# PA Islandization by Year

nrow(filter(results, Estimate > 0 & p.value < 0.05)) / nrow(results) * 100 #43.14471
# Nearly half of the protected areas (43.14%) showed a positive trend over the past 20-years.

##--------------------------------------##
# PA Islandization by biome

biome.trend <- aov(Estimate ~ biome, data = results)
summary(biome.trend)  #trends varied by biome
plot(biome.trend, 2)  #residuals are normally distributed
TukeyHSD(biome.trend) #pairwise comparison 

#            Df Sum Sq Mean Sq  F value  Pr(>F)    
#biome        7  0.87  0.12428  10.17   1.07e-12 ***
#Residuals 4463  54.52 0.01222 

# biome                                      diff          lwr          upr     p adj
# Desert-Boreal-Forests                  0.032033105  0.008466103  0.055600108 0.0009986
# Grassland-Shrubland-Boreal-Forests     0.045790264  0.026770442  0.064810087 0.0000000
# Tropical-Forests-Boreal-Forests        0.042057729  0.022809261  0.061306197 0.0000000
# Temperate-Forests-Grassland-Shrubland -0.023240799 -0.037270424 -0.009211173 0.0000146
# Tropical-Forests-Temperate-Forests     0.019508264  0.005170194  0.033846334 0.0009814

##--------------------------------------##
# PA Islandization by IUCN Category

IUCN <- results %>% #only 3,063 PAs have IUCN data
  filter(!IUCN_CAT %in% c("Not Reported","Not Assigned","Not Applicable"))

IUCN_trend <- aov(Estimate ~ IUCN_CAT, data = IUCN) 
summary(IUCN_trend)  #trends varied by IUCN Category
plot(IUCN_trend, 2)  #residuals are normally distributed
TukeyHSD(IUCN_trend) #pairwise comparison 

#             Df  Sum Sq Mean Sq F value Pr(>F)    
#IUCN_CAT       6   0.34 0.05607   3.886 0.000719 ***
#Residuals   3056  44.09 0.01443  

#IUCN_CAT     diff          lwr          upr     p adj
#VI-IV   0.0224718096  0.002915810  0.042027809 0.0125311

##--------------------------------------##
# PA Islandization by PA Size
size.trend <- lm(Estimate ~ log(GIS_AREA), data = results) # 
summary(size.trend) #trends varied with PA size

#               Estimate   Std. Error  t value  Pr(>|t|)  
# (Intercept)   -0.010428   0.010008  -1.042    0.298  
# log(GIS_AREA)  0.003182   0.001409   2.258    0.024 *
  
# Residual standard error: 0.1113 on 4469 degrees of freedom
# Multiple R-squared:  0.00114,	Adjusted R-squared:  0.0009161 
# F-statistic: 5.099 on 1 and 4469 DF, p-value: 0.02399

##--------------------------------------##
# 2020 PA percent edge

nrow(filter(results, edge_level_2020 > 0.5)) / nrow(results) * 100 #54.84232
# 54.84% of the PAs exhibited edges along the majority their PA frontiers in 2020.

##--------------------------------------##
# 2020 PA percent edge by biome

bio.aov <- aov(edge_level_2020 ~ biome, data = results) 
summary(bio.aov)  #edges varied by biome
plot(bio.aov, 2)  #residuals are normally distributed
TukeyHSD(bio.aov) #pairwise comparison 

#               Df Sum Sq Mean Sq F value   Pr(>F)    
# biome          7   0.70 0.09963   9.845 3.07e-12 ***
# Residuals   4463  45.16 0.01012 

# biome                                       diff           lwr          upr     p adj
# Grassland-Shrubland-Boreal-Forests     0.0228570886  0.0055461824  0.040167995 0.0016288
# Rock & Ice-Boreal-Forests             -0.1049717513 -0.2077813824 -0.002162120 0.0413799
# Grassland-Shrubland-Desert             0.0348929428  0.0176313854  0.052154500 0.0000001
# Rock & Ice-Grassland-Shrubland        -0.1278288400 -0.2298469729 -0.025810707 0.0036699
# Temperate-Forests-Grassland-Shrubland -0.0219392136 -0.0347082882 -0.009170139 0.0000055
# Tropical-Forests-Grassland-Shrubland  -0.0177351898 -0.0297682634 -0.005702116 0.0002161
# Tundra-Grassland-Shrubland            -0.0356965946 -0.0703885753 -0.001004614 0.0384782
# Temperate-Forests-Rock & Ice           0.1058896264  0.0037465795  0.208032673 0.0358219
# Tropical-Forests-Rock & Ice            0.1100936501  0.0080399994  0.212147301 0.0239564

##--------------------------------------##
# 2020 PA percent edge by IUCN Category

iucn.aov <- aov(edge_level_2020 ~ IUCN_CAT, data = IUCN) 
summary(iucn.aov)  #edges varied with IUCN category
TukeyHSD(iucn.aov) #pairwise comparison 

#               Df Sum Sq Mean Sq F value Pr(>F)  
# IUCN_CAT       6  0.143 0.02379   2.401 0.0257 *
# Residuals   3056 30.284 0.00991   

# IUCN_CAT     diff           lwr         upr     p adj
# IV-Ib   0.022364774  0.0007977075 0.043931840 0.0362707

##--------------------------------------##
# 2020 PA percent edge by PA Size

size.lm <- lm(edge_level_2020 ~ log(GIS_AREA), data = results) 
summary(size.lm) #edges did not vary with PA size

#               Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)   0.5127322  0.0091120  56.270   <2e-16 ***
# log(GIS_AREA) 0.0002523  0.0012830   0.197    0.844  

# Residual standard error: 0.1013 on 4469 degrees of freedom
# Multiple R-squared:  8.656e-06,	Adjusted R-squared:  -0.0002151 
# F-statistic: 0.03868 on 1 and 4469 DF,  p-value: 0.8441

##--------------------------------------##
# 2020 PA percent edge by Establishment Year

statusyr <- results %>% filter(!STATUS_YR == 0) #only 4,050 PAs have Establishment data

statusyr.lm <- lm(edge_level_2020 ~ STATUS_YR, data = statusyr) 
summary(statusyr.lm) #edges varied with Establishment Year

#              Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)  1.074e+00  1.566e-01   6.861  7.86e-12 ***
# STATUS_YR   -2.818e-04  7.869e-05  -3.582  0.000345 ***

# Residual standard error: 0.1014 on 4048 degrees of freedom
# Multiple R-squared:  0.003159,	Adjusted R-squared:  0.002913 
# F-statistic: 12.83 on 1 and 4048 DF,  p-value: 0.0003454

##--------------------------------------##
            ###END OF CODE###
##--------------------------------------##
