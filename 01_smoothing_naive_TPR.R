rm(list=ls())

source("load_path.R", echo=FALSE)


newdata <- read.csv( "./datafiles/ward_level_unsmoothed_tpr_u5.csv")

inla_data_sf <- inner_join(ilorin_shapefile, newdata, by = c("WardName" = "Wardname"))


new_data_average <- inla_data_sf %>% 
  group_by(WardName) %>% 
  summarise(tpr_u5 = sum(tested_u5_new, na.rm = T)/sum(conf_u5_new, na.rm = T)) %>%  
  mutate(tpr_u5_new = ifelse(tpr_u5 > 1, 1, tpr_u5))


w <- spdep::poly2nb(new_data_average, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, new_data_average$tpr_u5_new)

missing_indices <- which(is.na(new_data_average$tpr_u5_new))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  new_data_average$tpr_u5_new[polygon] <- mean(new_data_average$tpr_u5_new[neighbor_tprs], na.rm = TRUE)
}


merge_dataset <- new_data_average %>% 
  mutate(class = cut(tpr_u5_new, seq(0,1,0.2), include.lowest = T))



ggplot(data =merge_dataset)+
  geom_sf(aes(fill = tpr_u5_new))+
  # scale_fill_discrete(name="TPR U5", type = discrete_palettes)+
  scale_fill_continuous(low ="pink" , high = "red", name = "test positivity rate") +
  map_theme()





