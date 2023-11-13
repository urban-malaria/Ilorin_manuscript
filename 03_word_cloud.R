rm(list = ls())
source("load_path.R")




palettes <- list(rev(RColorBrewer::brewer.pal(4, "RdYlBu")))

# Sample text data (you can replace this with your own text)
text_data <- read.csv("./datafiles/ilorin_cluster_classification.csv") %>% 
  group_by(classification, Codes) %>% 
  summarise(total = n()) 

deprioritization_data <- read.csv("./datafiles/elite_communities.csv") %>% 
  group_by(classifications, communities) %>% 
  summarise(total = n()) %>% 
  mutate(classifications == "formal ", "formal", classifications)



ggplot(
  text_data,
  aes(
    label = Codes,
    size = total,
    color = classification  # Color by the 'classification' column
  )
) +
  geom_text_wordcloud() +
  scale_color_discrete(drop=FALSE, type = palettes)+
  scale_size_area() +
  theme_minimal()




ggplot(
  deprioritization_data,
  aes(
    label = communities,
    size = total,
    color = classifications  
  )
) +
  geom_text_wordcloud() +
  scale_color_discrete(drop=FALSE, type = palettes)+
  scale_size_area() +
  theme_minimal()



