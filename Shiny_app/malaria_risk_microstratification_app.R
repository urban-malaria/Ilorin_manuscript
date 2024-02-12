source("functions.R")


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),  
  titlePanel("Malaria Risk Mapping Tool"),
  
  tags$head(
    tags$style(HTML("
      .modal-content {
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
        background-color: #f7f7f7;
        border-radius: 8px;
        font-family: 'Arial', sans-serif;
      }
      .modal-header, .modal-footer {
        background-color: #e9e9e9;
        border-bottom: 1px solid #ddd;
      }
      .modal-title {
        color: #333;
      }
      .modal-body {
        color: #555;
      }
      .fade {
        animation-name: fadeIn;
        -webkit-animation-name: fadeIn; 
        animation-duration: 0.5s; 
        -webkit-animation-duration: 0.5s;
        animation-timing-function: ease-in-out; 
        -webkit-animation-timing-function: ease-in-out;           
      }
      @keyframes fadeIn {
        0% {opacity: 0;}
        100% {opacity: 1;}
      }
      @-webkit-keyframes fadeIn {
        0% {opacity: 0;}
        100% {opacity: 1;}
      }
    "))
  ),
  
  # actionButton("infoButton", "Open Modal"),
  
  br(),
  
  
  
  tabsetPanel(
    tabPanel("Instructions",
             tags$br(),
             h6("

Malaria remains a significant public health challenge globally, necessitating 
optimal and efficient strategies for risk management and intervention. 
Ozodiegwu et-al's study introduces a novel Malaria Risk Mapping Tool (MRMT), designed to
facilitate the microstratification of malaria risk across diverse geographical 
landscapes. The MRMT integrates environmental, socio-economic, 
and health-related data to generate high-resolution risk maps. Key variables, 
including the Enhanced Vegetation Index (EVI), settlement types, test positivity 
rates, and proximity to water bodies, are analyzed to identify areas of high 
transmission potential.


The tool's innovative approach lies in its ability to combine multiple data layers,
offering a comprehensive visualization of risk factors that contribute to malaria spread. 
This enables health authorities and stakeholders to prioritize resources and interventions 
more effectively, focusing on high-risk zones identified through the tool's analytical 
capabilities. Initial deployment and testing in a pilot region demonstrated the MRMT's efficacy 
in revealing previously unrecognized areas of vulnerability, facilitating targeted mosquito control
measures, health education campaigns, and infrastructure improvements (see Ozodiegwu et al).

Furthermore, the MRMT supports dynamic updating and scalability, allowing for the incorporation 
of new data and adaptation to different geographical regions. Its user-friendly interface ensures 
accessibility to a wide range of users, from public health officials to research institutions.

By providing a granular view of malaria risk, the MRMT represents a significant advancement 
in the field of public health, offering a potent tool for the strategic planning of malaria control 
and elimination efforts. Future developments will focus on integrating real-time data feeds and
expanding the tool's application to other vector-borne diseases, further enhancing its utility in 
global health management."),
             
             fluidRow(column(6,
             tags$br(),tags$br(),
             h3("Instructions to Using the Application"),
             HTML("
        <ol>
          <li>Ensure you have a dataset with the variables of interest. 
          Your dataset should include column names, please download the example.csv file to edit. 
          Click on the varible name you will be redirected to the sites where you can download 
          the respective raster files. Note: Some countries may not be listed on the provided sites.
            <ul>
              <li>Ward name</li>
              <li>Distance to water bodies</li>
              <li>Population density</li>
              <li>Population size</li>
              <li>Test positivity rate</li>
              <li>Housing quality</li>
              <li>Enhanced Vegetation Index</li>
              <li>Surface temperature and humidity</li>
              <li>Rainfall</li>
              <li>Dump sites</li>
              <li>ITN distribution per capita</li>
              <li>Settlement type</li>
            </ul>
          </li>
          <li>Download the shapefile for your region of interest.</li>
          <li>Save the shapefile in the same folder as the variable dataset and compress it into a .zip format.</li>
          <li>Proceed to the next tab to upload the data file and shapefile and visualize the output.
            <ul>
              <li><b>Visualize Tab:</b> Visualize the distribution of each variable across wards. Hover over a polygon to see the ward name and variable measure.</li>
              <li><b>Normalized Tab:</b> Select variables for the composite score. This tab allows for normalization and visualization of each variable's impact on the composite score.</li>
              <li><b>Composite Score Tab:</b> Shows the effect of each variable within the composite score. Select the variables you want to see in the composite function.</li>
              <li><b>Composite Score Summary Tab:</b> Analyzes ward ranks to produce box/whisker plots, highlighting wards with the lowest risk.</li>
            </ul>
          </li>
        </ol>
      ")
    )
    ,
    column( 6,
            tags$br(),tags$br(),tags$br(),tags$br(),
            tags$img(src = "digital_abstract.png", height = "600px", width = "auto"
                     ),
            style='text-align: center',
            tags$h6("The figure above shows a digital abstract first presented in Ozodiegwu et-al (in-press), 
            and it shows how the application was used to develop the malaria risk map in Ilorin")
      
    )
    )), 
    
    tabPanel( "Input variables (data and shapefiles)", 
              tags$br(),tags$br(),
              sidebarLayout(
                sidebarPanel(
                  tags$h3("Upload the shapefile and analysis data:"),
                  fileInput("file_csv", "Choose a CSV File", accept = ".csv"),
                  fileInput("file_shp", "Choose a Zipped Shapefile", 
                            accept = c('application/zip', 'application/x-zip-compressed', 
                                       'multipart/x-zip', 'application/x-compress', 
                                       'application/x-compressed', 'application/gzip')),
                  tags$h4("Select a variable in the dataset to visualise:"),
                  selectInput("visualize", "Select Variable", 
                              choices = c("enhanced vegetation index", 
                                          "distance to water bodies",
                                          "test positivity rate", 
                                          "housing quality", 
                                          "settlement type")),
                  downloadButton("downloadData", "example data"),
                  actionButton("plot_raw_data_button", "Plot Map")
                ),
                mainPanel(
                  girafeOutput("rawdataPlots")
                )
              ),
              style='text-align: center',
              tags$h6("The plot shows the distribution of the varibles selected for
                     evaluation accross the region of interest before they have been 
                     normalized using the min-max method. Each measure is presented on 
                     a continous scale (for the units one should refer to the rasters 
                     where the data was extarcted). The deeper the intensity of the 
                     colour in a given ward the higher the value of that measure is 
                     in the ward.")), 
    
    tabPanel("Composite Score distribution", 
             # "Composite Score distribution", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Select a variable in the dataset to visualise:"),
                 selectInput("variable", "Select Variable", 
                             choices = c("enhanced vegetation index", 
                                         "distance to water bodies",
                                         "test positivity rate", 
                                         "housing quality", 
                                         "settlement type"), 
                             multiple = TRUE),
                 
                 actionButton("plot_button", "calculate")
                 
               ),
               
               mainPanel(
                 
                 girafeOutput("mapPlot"),
                 
                 tableOutput("dataTable") ,
                 style='text-align: center',
                 
                 tags$h6(".")
               )
             )
    ),
    
    # hr(),
    
    tabPanel(" box whisker plot", 
             tags$br(),tags$br(),
             
             plotOutput("boxwhiskerPlots"),
             # rank the plots based of  the mean/median score
             tags$br(),tags$br(), # Adds a single line break (adjust by adding more)
             tags$br(),tags$br(),  
             tags$br(),tags$br(),  
             tags$br(),tags$br(),   
             tags$br(),tags$br(),
             style='text-align: center',
             tags$h6("The box and whisker plot visualizes the distribution of median 
                     vulnerability scores across wards, ranked from least to most 
                     vulnerable based on outcomes from models tailored with combinations 
                     of variables such as enhanced vegetation index, settlement type, 
                     test positivity rate, and distance to water bodies. Each model, 
                     integrating at least two or all mentioned variables, 
                     contributes to deriving the wards' median scores, showcasing 
                     the variance in vulnerability efficiently.") 
    ),
    
    tabPanel("normalization", 
             tags$br(),tags$br(),
             girafeOutput("normalizationplot"),
             style='text-align: center',
             tags$h6("The plot shows the distribution of the varibles selected for
                     evaluation accross the region of interest after they have been 
                     normalized using the min-max method. The values are now all on 
                     the same scale ranging from 0 to 1. This range of values are put 
                     into 5 classes (see legend). The plot highlights which of the
                     variables if used in the algorithm will have more infuence in the
                     composite score.")
    )
    
  ),
  

  tags$br(),  
  tags$br(),  
  tags$br(),  
  tags$br(),
  tags$br(),  
  tags$br(),
  
  hr(),
  
  
  div(
    style='text-align: center',
    'Created by the', 
    shiny::HTML('<a href=\'https://www.urban-malaria.com/\' 
                target=\'_blank\'> Urban Malaria Project Team </a>'),
    '@ Loyola University, Parkinson School of Public Health, Department of Health Informatics and Data Science'
  ),
  
  br()
)
# https://www.urban-malaria.com//

server <- function(input, output) {
  
  # information 
  showModal(modalDialog(
    title = "Information",
    "Welcome to the de-prioritization web application, a powerful tool for 
    visualizing the distribution of variables associated with malaria risk 
    at a granular level. It uniquely focuses on a finer scale, analyzing data 
    down to the ward level rather than the broader Local Government Area (LGA). 
    This detailed approach, combined with geocoding technology, allows for 
    enhanced de-prioritization, enabling stakeholders to make decisions at the 
    smallest scale of settlement type. This specificity ensures that de-prioritization
    of resources like bed nets happens effectively, targeting areas with the highest
    need and optimizing malaria control efforts."
  ))
  
  # Use JavaScript to close the modal after 60 seconds
  shinyjs::runjs("setTimeout(function() { $('.modal').modal('hide'); }, 60000);")
  
  # Example data 
  want_example_data  <- reactiveVal(FALSE)
  
  # Reactive values raw data holders 
  raw_data_reactive <- reactiveVal()
  rawdata_reactive <-  reactiveVal()
  
  
  # Reactive values normalisation data holders
  normalized_data_reactive <- reactiveVal()
  normalizeddata_reactive <-  reactiveVal()
  
  # Reactive values scoring algorithm
  data_reactive <- reactiveVal()
  shp_data_reactive <- reactiveVal()
  csv_data_reactive <- reactiveVal()
  output_data_reactive <- reactiveVal()
  
  #take note of the example data set 
  observeEvent(input$downloadData, {
    want_example_data(TRUE)
  })
  
  
  # Load the CSV data into a reactive expression
  download_data_reactive <- reactive({
    read.csv("src/ward/example.csv")
  })
  
  
  observeEvent(input$plot_raw_data_button, {
    req(input$file_csv, input$file_shp)
    
    # Read the CSV file
    csv_data <- read.csv(input$file_csv$datapath)
    raw_dataframe <- rename_columns(csv_data, pattern_list)
    raw_data_reactive(raw_dataframe) # Store raw data
    
    # Read and process the shapefile
    temp_dir <- tempdir()
    unzip(input$file_shp$datapath, exdir = temp_dir)
    shapefile_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    shp_data <- st_read(shapefile_files[1], quiet = TRUE)
    shp_data_reactive(shp_data) # Store shapefile data
    
    # Join raw data with shapefile data for visualization
    raw_data <- inner_join(raw_data_reactive(), shp_data_reactive())
    rawdata_reactive(raw_data)
  })
  
  
  
  observeEvent(input$plot_button, {
    req(input$file_csv, input$file_shp)
    
    # Assuming CSV and shapefile reading is done above
    # and raw_dataframe, shp_data are available from reactive values
    
    # Normalization and model processing
    normalized_data <- normalize_data(raw_data_reactive())
    processeddata <- process_data(normalized_data, selected_vars = input$variable)
    processed_data <- normalized_data %>% 
      select(WardName, matches("normalization|restructured"), -normalization_dw)
    model_scores <- composite_score_models(processed_data, selected_vars = input$variable)
    refactored_data <- process_model_score(model_scores$final_data)
    model_formulae_table <- models_formulas(model_scores$model_formula)
    
    normalized_data_reactive(processeddata)
    csv_data_reactive(refactored_data) 
    output_data_reactive(model_formulae_table)
    
    # Join processed data with shapefile data for further use
    combined_data <- inner_join(csv_data_reactive(), shp_data_reactive()) 
    data_reactive(combined_data)
    normalizeddata_reactive(inner_join(normalized_data_reactive(), shp_data_reactive()))
  })
  
  
  # example data 
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      "example.csv"
      
    },
    
    
    content = function(file) {
      
      file.copy("example.csv", file)
      
    }
    
  )
  
  output$mapPlot <- renderGirafe({
    req(data_reactive()) 
    
    # Plot the composite score maps  
    plot_model_score_map(shp_data  = shp_data_reactive(),
                         processed_csv  = data_reactive())
  })
  
  # yeilds the key for each map composite score permutations formulas 
  output$dataTable <- renderTable({
    
    output_data_reactive()
    
  })
  
  
  output$normalizationplot <- renderGirafe({
    
    req(normalizeddata_reactive()) 
    
    # all selected variables selected for analysis (normalization map)
    plot_normalized_map(shp_data  = shp_data_reactive(),
                        processed_csv  = normalizeddata_reactive())
  })
  
  
  
  # plot output depends on selected variable(s)
  output$rawdataPlots <- renderGirafe({
    req(rawdata_reactive())  
    selected_variable <- input$visualize
    plot_map_00(variable_name = selected_variable,
                shp_data_reactive = shp_data_reactive(),
                raw_dataframe_reactive = rawdata_reactive())
  })
  
  
  # plot output depends on selected variable(s)
  output$boxwhiskerPlots <- renderPlot({
    req(data_reactive()) 
    # new_data = 
    # print(head(req(data_reactive())))
    
    box_plot_function(plottingdata = data_reactive())
  }, width = 800, height = 600)
  
  
  
}

shinyApp(ui = ui, server = server)