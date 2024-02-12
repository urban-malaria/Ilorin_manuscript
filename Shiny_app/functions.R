# functions
#rm(list = ls())

# packages <- c("vroom", "stringr", "dplyr", "ggplot2",
#               "sf", "ggiraph", "shiny", "shinydashboard", 
#               "")
library(sf)
library(vroom)
library(stringr)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(ggiraph)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(rlang)
library(gridExtra)
library(shinyjs)


#######################################################################################################################
# Define your patterns and their new names
######################################################################################################################

pattern_list <- list(
  "enhanced vegetation index" = c("meanEVI", "enhanced_vegetation_index", "evi", "enhanced vegetation index"),
  "distance to water bodies" = c("distance to water bodies", "water", "dw", "DW", "distance"),
  "test positivity rate" = c("test positivity rate", "test_positivity_rate", "tpr", "TPR", "tpr_u5_new"),
  "settlement type" = c("settlement_type", "ST", "settlement type", "st"),
  "housing quality" = c("housing_quality", "HQ", "housing quality", "hq", "val_housing_15")
)


map_theme <- function(){
  # map theme 
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 22, color = "black"), 
          axis.text.y = element_text(size = 22, color = "black"),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size =22),
          legend.title=element_text(size=22, colour = 'black'),
          legend.text =element_text(size = 22, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}



rename_columns <- function(df, pattern_list) {
  # Function to rename columns based on pattern matching
  for (pattern in names(pattern_list)) {
    patterns <- pattern_list[[pattern]]
    df <- df %>% 
      rename_with(~ pattern, all_of(intersect(names(df), patterns)))
  }
  df
}


plot_map <- function(variable_names, 
                     shp_data_reactive, 
                     raw_dataframe_reactive) {
  
  plots <- list()
  
  for (variable_name in variable_names) {
    
    if (variable_name == "enhanced vegetation index") {
      
      Fill <- "enhanced vegetation index"
      Low <- "#F6E0b3"
      High <- "darkgreen"
      
    } else if (variable_name == "settlement type") {
      
      Fill <- "settlement type"
      Low <- "#F6E0b3"
      High <- "#A97263"
      
    } else if (variable_name == "test positivity rate") {
      
      Fill <- "test positivity rate"
      Low <- "pink"
      High <- "red"
      
    } else if (variable_name == "housing quality") {
      
      Fill <- "housing quality"
      Low <- "#F6E0b3"
      High <- "#A97263"
      
    } else if (variable_name == "distance to water bodies") {
      
      Fill <- "distance to water bodies"
      Low <- "midnightblue"
      High <- "skyblue"
      
    } else {
      
      stop("Error: ", variable_name, " not found")
    }
    
    plot <- ggplot(data = shp_data_reactive) +
      geom_sf(color = "black", fill = "white") +
      geom_sf(data = raw_dataframe_reactive,
                          aes(geometry = geometry,
                              fill = !!sym(Fill)
                              # ,  
                              # tooltip = WardName
                              )
              ) +
      scale_fill_continuous(name = "", low = Low, high = High) +
      labs(title = variable_name, subtitle = '', 
           fill = "", x = NULL, y = NULL) +
      map_theme()
    
    plots[[variable_name]] <- plot
  
  }
  
  combined_plot <- do.call(grid.arrange, c(plots, ncol = 2))
  
  # dynamic <- girafe(ggobj = combined_plot)
  
  return(combined_plot)
  
}

################################################################################
# 
plot_map_00 <- function(variable_name, shp_data_reactive, raw_dataframe_reactive) {
  
  if (variable_name == "enhanced vegetation index") {
    
    Fill <- "enhanced vegetation index"
    Low <- "#F6E0b3"
    High <- "darkgreen"
    
  } else if (variable_name == "settlement type") {
    
    Fill <- "settlement type"
    Low <- "#F6E0b3"
    High <- "#A97263"
    
  } else if (variable_name == "test positivity rate") {
    
    Fill <- "test positivity rate"
    Low <- "pink"
    High <- "red"
    
  } else if (variable_name == "housing quality") {
    
    Fill <- "housing quality"
    Low <- "#F6E0b3"
    High <- "#A97263"
    
  } else if (variable_name == "distance to water bodies") {
    
    Fill <- "distance to water bodies"
    Low <- "midnightblue"
    High <- "skyblue"
    
  } else {
    
    stop("Error: ", variable_name, " not found")
  }
  
  plot <- ggplot(data = shp_data_reactive) +
    geom_sf_interactive(color = "black", fill = "white") +
    geom_sf_interactive(data = raw_dataframe_reactive,
                        aes(geometry = geometry,
                            fill = !!sym(Fill),  
                            tooltip = paste(WardName, 
                                            "(",round(!!sym(Fill), 3),")"))) +
    scale_fill_continuous(name = "", low = Low, high = High) +
    labs(title = variable_name, subtitle = '', 
         fill = "", x = NULL, y = NULL) +
    map_theme()
  
  girafe(ggobj = plot)
}
################################################################################

plot_dynamic_map <- function(variable_name, 
                     shp_data_reactive, 
                     raw_dataframe_reactive) {
    
    if (variable_name == "enhanced vegetation index") {
      
      Fill <- "enhanced vegetation index"
      Low <- "#F6E0b3"
      High <- "darkgreen"
      
    } else if (variable_name == "settlement type") {
      
      Fill <- "settlement type"
      Low <- "#F6E0b3"
      High <- "#A97263"
      
    } else if (variable_name == "test positivity rate") {
      
      Fill <- "test positivity rate"
      Low <- "pink"
      High <- "red"
      
    } else if (variable_name == "housing quality") {
      
      Fill <- "housing quality"
      Low <- "#F6E0b3"
      High <- "#A97263"
      
    } else if (variable_name == "distance to water bodies") {
      
      Fill <- "distance to water bodies"
      Low <- "midnightblue"
      High <- "skyblue"
      
    } else {
      
      stop("Error: ", variable_name, " not found")
    }
    
    plot <- ggplot(data = shp_data_reactive) +
      geom_sf_interactive(color = "black", fill = "white") +
      geom_sf_interactive(data = raw_dataframe_reactive,
              aes(geometry = geometry, fill = !!sym(Fill),
                  tooltip = paste(WardName, 
                                  "(",round(!!sym(Fill), 3),")")
              )
      ) +
      scale_fill_continuous(name = "", low = Low, high = High) +
      labs(title = variable_name, subtitle = '', 
           fill = "", x = NULL, y = NULL) +
      map_theme()
    
      # plot
    
    girafe(code ={print(plot)}, width_svg = 6, height_svg = 4)
  
}


#######################################################################################################################
# Normalization functions
######################################################################################################################

normalize_data <- function(uploaded_data) {
  # Function to normalize and restructure the data
  scoring_data <- uploaded_data %>%
    mutate(
      normalization_evi = (`enhanced vegetation index` - min(`enhanced vegetation index`)) / (max(`enhanced vegetation index`) - min(`enhanced vegetation index`)),
      normalization_st = (`settlement type` - min(`settlement type`)) / (max(`settlement type`) - min(`settlement type`)),
      normalization_dw = (min(`distance to water bodies`) - `distance to water bodies`) / (max(`distance to water bodies`) - min(`distance to water bodies`)),
      normalization_tpr = (`test positivity rate` - min(`test positivity rate`)) / (max(`test positivity rate`) - min(`test positivity rate`)),
      normalization_hq = (`housing quality` - min(`housing quality`)) / (max(`housing quality`) - min(`housing quality`))
    ) %>%
    mutate(
      restructured_ds = (normalization_dw - min(normalization_dw)) / (max(normalization_dw) - min(normalization_dw))
    )
  
  scoring_data
}


# Trial_data <- normalize_data(rename_columns(risk_variables, pattern_list))

process_data <- function(data, 
                         selected_vars #, 
                         #shp_data_func
                         ) {
  # Ensure that WardName is always included and avoid duplicate
  selected_vars <- unique(c("WardName", selected_vars))
  
  
  # Check if enough variables are selected
  if (length(selected_vars) < 3) {
    
    stop("Error: Enter at least three variables along with WardName.")
  }
  
  # Map selected variable names to their normalized counterparts
  normalized_variables_mapping <- c(
    "enhanced vegetation index" = "normalization_evi",
    "distance to water bodies" = "restructured_ds",
    "test positivity rate" = "normalization_tpr",
    "housing quality" = "normalization_hq",
    "settlement type" = "normalization_st"
  )
  
  # Get the normalized variable names based on selected variables
  normalized_variables <- normalized_variables_mapping[selected_vars[selected_vars != "WardName"]]
  
  # Filter the selected variables from the data
  plotting_scoring_data <- data %>%
    select(all_of(c("WardName", normalized_variables))) %>%
    reshape2::melt(id.vars = "WardName") %>%
    mutate(class = cut(value, seq(0, 1, length.out = 6), include.lowest = TRUE)) 
  
  if (anyNA(plotting_scoring_data)){
    
    stop("Error: Data contains NAs clean the data and continue")
    
  }
  

  plotting_scoring_data
}




plot_normalized_map <- function(shp_data, processed_csv) {
  
  # Use a color palette function
  palette_func <- RColorBrewer::brewer.pal(5, "YlOrRd")
  
  # Create the ggplot object
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") +
    geom_sf_interactive(data = processed_csv, 
                        aes(geometry = geometry, fill = class, tooltip = WardName)) +
    facet_wrap(~variable) +  
    scale_fill_manual(name = "rank", values = palette_func) +
    labs(subtitle = '', fill = "") +
    theme(panel.background = element_blank()) +
    theme_void()
  
  # Create the interactive girafe object
  girafe(ggobj = plot)
}

#######################################################################################################################
# Scoring functions 
######################################################################################################################

# Trial_data <- normalize_data(rename_columns(risk_variables, pattern_list)) %>%
#   select(WardName, matches("normalization|restructured"), -normalization_dw)




composite_score_models <- function(composite_score_data, selected_vars) {
  
  
  
  normalized_variables_mapping <- c(
    "enhanced vegetation index" = "normalization_evi",
    "distance to water bodies" = "restructured_ds",
    "test positivity rate" = "normalization_tpr",
    "housing quality" = "normalization_hq",
    "settlement type" = "normalization_st"
  )
  
  
  actual_vars <- normalized_variables_mapping[selected_vars]
  

  scoring_data <- composite_score_data 
  

  model_combinations <- lapply(seq_len(length(actual_vars)), function(x) {
    
    combn(actual_vars, x, simplify = FALSE)
    
  }) %>% 
    unlist(recursive = FALSE)
  
  valid_model_combinations <- Filter(function(x) length(x) >= 2, model_combinations)
  
  model_formula <- list()


  for (i in seq_along(valid_model_combinations)) {
    
    model_formula[[i]] <- paste(valid_model_combinations[[i]], collapse  = " + ")
    model_name <- paste0("model ", i)
    scoring_data <- scoring_data %>% 
      mutate(!!sym(model_name) := !!parse_expr(model_formula[[i]]))
    

    #model_formulas_data <- rbind(model_formulas_data, valid_model_combinations)
  }

  
  final_data <- scoring_data %>% 
    select(WardName, starts_with("model"))
  
  list(model_formula = valid_model_combinations, 
       final_data = final_data)
}



models_formulas <- function(model_data){
  
  model_formulas_data <- data.frame(model = vector(), 
                                    variables = vector())
  
  for (index in seq_along(model_data)){
    
    model_formula <- data.frame(model = paste0("model ", index), 
                             variables = paste(names(model_data[[index]]),  
                                               collapse = " + "))
    
    model_formulas_data <- rbind(model_formulas_data, model_formula)
  }
  
  return(model_formulas_data)
}



########


process_model_score <- function(data_to_process
                                
                                ){
  
  plottingdata <- data_to_process %>% 
    reshape2::melt(id.vars = c("WardName")) %>% 
    # inner_join(shapefile_files, by = c("WardName")) %>% 
    group_by(variable) %>% 
    mutate(new_value = (value - min(value))/(max(value) - min(value)),
           class = cut(new_value, seq(0, 1, 0.2), include.lowest = T)) %>%
    arrange(value) %>% 
    mutate(rank = 1:n(), 
           wardname_rank = paste(WardName, "(",rank,")"))
  
  plottingdata
  
}




plot_model_score_map <- function(shp_data, processed_csv) {
  
  # selected palette 
  palette_func <- RColorBrewer::brewer.pal(5, "YlOrRd")
  
  # Map object
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") +
    geom_sf_interactive(data = processed_csv, 
                        aes(geometry = geometry, fill = class, tooltip = wardname_rank)) +
    facet_wrap(~variable, ncol = 3) +
    scale_fill_discrete(drop=FALSE, name="score", type = palette_func)+
    labs(subtitle='', title='', fill = "score")+
    theme(panel.background = element_blank(), size = 20)+
    theme_void()
  
  
  # interactive  object
  girafe(ggobj = plot)
}



# plot_model_score_map(shp_data = shapefile_files,
#                      processed_csv = plotting_model_data)


#######################################################################################################################
# box plot functions and
######################################################################################################################


box_plot_function <- function(plottingdata){

  df_long <- plottingdata %>%
    select(WardName, variable, rank)

  medians <- df_long %>%
    group_by(WardName) %>%
    summarize(median_value = median(rank)) %>%
    arrange(median_value) %>%
    .$WardName

  df_long$WardName <- factor(df_long$WardName, levels = medians)

  ggplot(df_long, aes(x = rank, y = WardName)) +
    geom_boxplot() +
    labs(title = "", x = "Rank", y = "Ward") +
    labs(title = "", x = "Rank", y = "Ward") +
    scale_x_continuous(limits = c(0, 36),
                       breaks = seq(0, 36, 3)) +
    theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"), 
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black')
          # ,
          # legend.key.height = unit(1, "cm")
          )
  
}


# box_plot_function(other_data = plottingdata)
