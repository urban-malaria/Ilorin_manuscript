rm(list=ls())

source("load_path.R", echo=FALSE)

read.csv("./datafiles/scoring_data02.csv")

# colour palettes for the map 
palettes_00 <- list(rev(RColorBrewer::brewer.pal(5, "OrRd")))[[1]][5:1]
palettes <- list(rev(RColorBrewer::brewer.pal(5, "RdYlBu")))

#data files 
tpr  = read.csv("./datafiles/ward_levelsmoothed_tpr_u5.csv") %>% 
  select(c(WardName, tpr_u5_new))

scoring_data02 = read.csv("datafiles/scoring_data02.csv") %>% 
  inner_join(tpr)



# model permutations
model <- c("normal_score_tpr + normal_score_stype", 
           "restructured_ds + normal_score_stype", 
           "normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + normal_score_stype", 
           "normal_score_tpr + normal_score_meanEVI + normal_score_stype", 
           "restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + 0.5 * restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + 0.5 * restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype")


# variable names 
new_names = c("enhanced vegetation index",
              "settlement type",
              "distance to water bodies", 
              "test positity rate" )

names(new_names) = c("normal_score_meanEVI",
                     "normal_score_stype",
                     "restructured_ds", 
                     "normal_score_tpr")


# Data normalization, Scoring, and manipulations  

scoring_data <- scoring_data02 %>% 
  mutate(normal_score_meanEVI = (meanEVI - min(meanEVI ))/(max(meanEVI ) - min(meanEVI )),
         normal_score_stype = (settlement_type - min(settlement_type))/
           (max(settlement_type) - min(settlement_type)),
         normal_score_dwater = (min(distance) - distance)/(max(distance) - min(distance)),
         normal_score_tpr = (tpr_u5_new - min(tpr_u5_new))/(max(tpr_u5_new) - min(tpr_u5_new))
         ) %>% 
  mutate(restructured_ds = (normal_score_dwater - min(normal_score_dwater))/(max(normal_score_dwater) - min(normal_score_dwater)))


scoring_data_zet <- scoring_data %>% 
  select(WardName, 
         normal_score_stype,
         normal_score_tpr,
         restructured_ds,
         normal_score_meanEVI) %>% 
  mutate(model01 = normal_score_tpr + normal_score_stype,
         model02 = restructured_ds + normal_score_stype, 
         model03 = normal_score_meanEVI + normal_score_stype, 
         model04 = normal_score_tpr + restructured_ds + normal_score_stype,
         model05 = normal_score_tpr + normal_score_meanEVI + normal_score_stype, 
         model06 = restructured_ds + normal_score_meanEVI + normal_score_stype, 
         model07 = normal_score_tpr + restructured_ds + normal_score_meanEVI + normal_score_stype, 
         model08 = normal_score_tpr + 0.5 * restructured_ds + normal_score_meanEVI + normal_score_stype,
         model09 = normal_score_tpr + restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype, 
         model10 = normal_score_tpr + 0.5 * restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype)


# data reshaping 
plotting_scoring_data <- scoring_data_zet %>%
  select(WardName, 
         normal_score_meanEVI, 
         restructured_ds,
         normal_score_stype, 
         normal_score_tpr) %>% 
  reshape::melt(id.vars = c("WardName")) %>% 
  mutate(class = cut(value, seq(0,1, length.out = 6), include.lowest = T)) %>% 
  inner_join(ilorin_shapefile, by = c("WardName"))



plottingdata <- scoring_data_zet %>% 
  select(WardName, model01, model02,  model03,
         model04,  model05,  model06, model07,
         model08,  model09,   model10) %>% 
  reshape2::melt(id.vars = c("WardName")) %>% 
  inner_join(ilorin_shapefile, by = c("WardName")) %>% 
  group_by(variable) %>% 
  mutate(new_value = (value - min(value))/(max(value) - min(value)),
         class = cut(new_value, seq(0, 1, 0.2), include.lowest = T)) %>%
  arrange(value) %>% 
  mutate(rank = 1:n())



#plot for normalized variables 
ggplot(data = ilorin_shapefile) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = plotting_scoring_data, 
          aes(geometry = geometry, fill = class)) +
  facet_wrap(~variable, labeller = labeller(variable = new_names)) +  
  scale_fill_discrete(drop=FALSE, name="", type = palettes_00)+
  labs(subtitle = '', fill = "") +
  theme(panel.background = element_blank(), size = 20) +
  theme_void()




# Ranking maps for all 10 composite score
palettes <- list(RColorBrewer::brewer.pal(5, "YlOrRd"))

ggplot(data = ilorin_shapefile)+
  geom_sf(color = "black", fill = "white")+
  geom_sf(data = plottingdata, aes(geometry = geometry, fill = class))+
  geom_sf_text(data = plottingdata, aes(geometry = geometry, label =  rank), 
               size = 3 )+ 
  facet_wrap(~variable, labeller = label_parsed, ncol = 2) +
  scale_fill_discrete(drop=FALSE, name="rank", type = palettes)+
  # scale_fill_manual(values = palettes)+
  labs(subtitle='', 
       title='',
       fill = "ranking score")+
  theme(panel.background = element_blank(), size = 20)+
  theme_void()


 
# analyse the ranking output and apply the median function row-wise to your data frame  

newdata <- plottingdata %>%
  select(WardName, variable, rank) %>%
  pivot_wider(
    names_from = variable,
    values_from = rank  
  ) %>%
  sf::st_drop_geometry() %>% 
  mutate(means = rowMeans(across(-WardName)))

selected_columns = 2:11
newdata$medians <- apply(newdata[, selected_columns], 1, median)
newdata$modes <- apply(newdata[, selected_columns], 1, calculate_mode)
 
 
 df_long <- newdata %>%
   select(-c(medians, modes, means)) %>% 
   reshape2::melt(id.vars = c("WardName"))
 
 
# Calculate the median values for ordering
medians <- df_long %>%
   group_by(WardName) %>%
   summarize(median_value = median(value)) %>%
   arrange(median_value) %>%
   .$WardName
 
# Convert the WardName to a factor and 
# specify the levels based on the medians calculated
 
df_long$WardName <- factor(df_long$WardName, levels = medians)
 

# Create the plot with ordered boxplots
ggplot(df_long, aes(x = value, y = WardName)) +
   geom_boxplot() +
   labs(title = "", x = "Rank", y = "Ward") +
   labs(title = "", x = "Rank", y = "Ward") +
   scale_x_continuous(limits = c(0, 36), 
                      breaks = seq(0, 36, 3)) +
   theme_manuscript() 
  
