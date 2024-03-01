library(tidyverse)
library(plotly)
library(hrbrthemes)
library(htmlwidgets)
library(gridExtra)
library(colorspace)
library(patchwork)

#######################################################################################
############ Figure 3; table S1, and other global-level summary statistics ############
#######################################################################################

## global results analysis 
# get IDs for the set of PAs that will be reported in result 
df.info <- read_csv("result/GLOBAL_edge_all_results.csv", col_types = cols(WDPA_PID = col_character())) %>%  # nrow = 5330 
  mutate(BIOME = case_when(                                                                                  ## reclassify BOIME
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
    TRUE ~ BIOME)) %>%
  mutate(edge_change = case_when((Estimate > 0 & p.value < 0.05) ~ "Significantly Increase", 
                                 (Estimate > 0 & p.value >= 0.05) ~ "Not significantly Increase",   # <- in making the figure, we considered these all as not significant 
                                 (Estimate == 0) ~ "Stable", # 0 PA has 0 slope
                                 (Estimate < 0 & p.value < 0.05) ~ "Significantly Decrease", 
                                 (Estimate < 0 & p.value >= 0.05) ~ "Not significantly Decrease",# <- in making the figure, we considered these all as not significant 
                                 TRUE ~ "unknown"))   ## some PAs when percent evaluated is very low, P value cannot be calculated


IDs <- (df.info %>% filter(perc_evaluated_2020 >= 0.3))$WDPA_PID # n = 4471, PA that evaluated for at least 30%

## --------------------   summarize presence of edge at the global scale --------- ##
df <- read_csv("result/GLOBAL_sim_transects_landOnly.csv", col_types = cols(WDPA_PI = col_character())) %>% filter(WDPA_PI %in% IDs) # takes a long time to load. n PID = 4466.
df <- df %>% dplyr::select(-n) %>% pivot_wider(names_from = pnt_pst, values_from = first) 
names(df) <- c("WDPA_PI", "trnscID", "year", "inner2", "inner1", "bd", "outer1","outer2" )
# if boundary gradient is larger than one of the inners and one of the outers, edge exists.
df <- df  %>% mutate(edge = ifelse((bd > inner2 | bd > inner1) & (bd > outer1 | bd > outer2), 1, 0)) 

## merge PA info 
df.w.info <- df %>% left_join(df.info %>% select(WDPA_PID, BIOME), by = c("WDPA_PI" = "WDPA_PID")) 
#length(unique(df.w.info$WDPA_PI)) # n PID = 4471

rm(df)

## -------------------- some basic stats reported in the manuscript --------- ##
## -------------------- total # of transects  --------- ##
ntrnsc = nrow(df.w.info )/20 #1,516,412

## total # of PA
nPA = length(unique(df.w.info$WDPA_PI)) # 4,471

## -------------------- total # of transects by biome --------- ##
ntrnsc.biome <- df.w.info %>% group_by(year, BIOME) %>% summarise (n = n()) %>% ungroup() %>%
  filter(year == 2020) %>% select(-year)

# BIOME                      n
# <chr>                  <int>
# 1 Boreal-Forests        171101
# 2 Desert                188581
# 3 Grassland & shrubland 425980
# 4 Mangroves                885
# 5 N/A                     5874
# 6 Temperate-Forests     256004
# 7 Tropical-Forests      410630
# 8 Tundra                 57357

## --------------------  total # of PA per biome   --------- ##
nPA.biome <- df.w.info %>% select(WDPA_PI, BIOME) %>% distinct() %>% group_by(BIOME) %>% summarise (n = n()) 
# 1 Boreal-Forests          403
# 2 Desert                  406
# 3 Grassland & shrubland  1353
# 4 Mangroves                 7
# 5 N/A                       9
# 6 Temperate-Forests       987
# 7 Tropical-Forests       1224
# 8 Tundra                   82


## -------------------- % PA significantly increase or decrease; fig 2 caption and manuscript summary stats --------- ##
sig.increase.pa <- df.info %>% filter(perc_evaluated_2020 >= 0.3, edge_change == "Significantly Increase")
nrow(sig.increase.pa)/nPA  # % PA show significantly increase trend 

sig.decrease.pa <- df.info %>% filter(perc_evaluated_2020 >= 0.3, edge_change == "Significantly Decrease")
nrow(sig.decrease.pa)/nPA 

## ----------------------------  area summary   -------------------------------- ##
summary(df.info %>% filter(perc_evaluated_2020 >= 0.3) %>% pull(GIS_AREA))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 200.4    435.8    862.2   2947.4   2266.1 961673.2 

#######################################################################################
####################### ------------ FIGURE 3 ------------ ############################
#######################################################################################

## FIG % edge globally change over 20 years -------
## updated in Jan 2023 with regrouped BIOMES ------

### ------ df for fig 3a --------------- #
df.rate.global <- df.w.info %>% group_by(year) %>% filter(edge == 1) %>% summarise( perc_edge = n()/ntrnsc)

### ---- edge level in 2020 at the global level ---- ##
df.rate.global %>% filter(year == 2020)


## FIG % edge globally change over 20 years by biomes -------
df.rate.biome <- df.w.info %>% group_by(year, BIOME) %>% filter(edge == 1) %>% summarise(n.edge = n()) %>%
  left_join(ntrnsc.biome) %>% mutate(perc_edge = n.edge/n)

### ------ df for fig 3b --------------- #
df.info.stackfig.count <- df.info %>%
  filter(perc_evaluated_2020 >= 0.3) %>%
  dplyr::select(BIOME, edge_change) %>%
  group_by(BIOME, edge_change) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "edge_change", values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(total = `Not significantly Increase` + `Significantly Increase` + `Significantly Decrease` + `Not significantly Decrease`)

df.info.stackfig.perc <- df.info.stackfig.count %>% mutate(
  `Not significantly Increase` = `Not significantly Increase`/total,
  `Significantly Increase` = `Significantly Increase`/total,
  `Significantly Decrease` = `Significantly Decrease` / total,
  `Not significantly Decrease` = `Not significantly Decrease`/total
) %>% select(-total) %>%
  mutate(BIOME = factor(BIOME, levels = c("Mangroves", "Rock & Ice","Tundra","Desert", "Grassland & shrubland", 
                                           "Boreal-Forests", "Temperate-Forests","Tropical-Forests" ))) %>%
  pivot_longer(2:5, names_to = "variable", values_to = "value")  %>%
  mutate(variable = ordered(variable, levels =  c("Significantly Decrease","Not significantly Decrease","Significantly Increase", "Not significantly Increase")) )

View(df.info.stackfig.perc)

####  two plots on top of each other
tiff(filename="visualization/0Main_FIGS/mid-product/FIG3_2023.tif", units="mm", width=500, height=270, res=600)
p1 <- 
  ggplot() +
  geom_point(data = df.rate.biome %>% filter(BIOME != 'Rock & Ice', BIOME != 'Mangroves'),
             aes(x=year, y=perc_edge*100, color = BIOME),
             size = 2.5, alpha = .4) +
  geom_smooth(data = df.rate.biome %>% filter(BIOME != 'Rock & Ice', BIOME != 'Mangroves'),
              aes(x=year, y=perc_edge*100, color = BIOME, linetype = "twodash"),
              method=lm, size = 1.5, alpha = 0.15, show.legend = F) +
  geom_smooth(data = df.rate.global, aes(x=year, y=perc_edge * 100), color = "#f04a4a", method=lm, linewidth = 1.5) +
  geom_point(data = df.rate.global, aes(x=year, y=perc_edge * 100), color = "#f04a4a", size = 3, stroke = 1.5, alpha = 0.7) +
  #scale_color_manual(values = c("#6D93BF", "#D882A9", "#F6AE2D", "#A6DC61", "#59B854", "#a393bf"))  +
  scale_color_manual(values = c("#b3b3b3", "#b3b3b3", "#b3b3b3", "#b3b3b3", "#b3b3b3", "#b3b3b3"))  +
  ylim(49,53) +
  xlim(2000,2021) +
  labs(y = " ", x = "") +
  theme_ipsum(base_size = 18, axis_title_size = 20) +
  theme(legend.position="none",
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank())

p2 <- 
  df.info.stackfig.perc  %>%
  ggplot(aes( x = BIOME, fill = variable)) +
  geom_bar(data = subset(df.info.stackfig.perc %>% filter(!BIOME %in% c("Mangroves", "Rock & Ice")), 
                         variable %in% c("Not Significantly Decrease","Significantly Decrease")),
           aes(y = -value*100), position="stack", stat="identity", width = 0.5) +
  geom_bar(data = subset(df.info.stackfig.perc %>% filter(!BIOME %in% c("Mangroves", "Rock & Ice")), 
                         !variable %in% c("Not Significantly Decrease","Significantly Decrease")), 
           aes(y = value*100), position="stack", stat="identity",width = 0.5) +
  scale_fill_manual(values=c("#cbcbcb", "#cbcbcb","#56B4E9", "#f04a4a")) +
  coord_flip()   +
  ylab("") +
  xlab("") +
  theme_ipsum(base_size = 18, axis_title_size = 20) +
  theme(legend.position="none",
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank())

p1 + p2
dev.off()


#######################################################################################
####################### ------------ Table S1  ------------ ############################
#######################################################################################
library(stargazer)
m.global <- lm(perc_edge~year, data = df.rate.global)
m.d <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME ==  "Desert"  ))
m.g <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Grassland & shrubland"))
m.tr <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Tropical-Forests"))
m.tp <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Temperate-Forests" ))
m.br <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Boreal-Forests" ))
m.t <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Tundra"))
m.m <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Mangroves"))
m.r <- lm(perc_edge~year, data = df.rate.biome %>% filter(BIOME == "Rock & Ice"))

stargazer(m.global, m.d, m.g, m.tr, m.tp, m.br, m.t, m.m, m.r,
          type = "text", title="Results", summary = T, align=TRUE, omit = "Constant",
          column.labels = c("Global", "Desert", "Grassland & Shrubland", "Tropical Forest", "Temperate Forest", 
                            "Boreal forest", "Tundra", "Mongrove", "Rock & Ice"))

###########################################################################################################
################## -----------------------------  END CODE  ------------------------------ ################
###########################################################################################################
