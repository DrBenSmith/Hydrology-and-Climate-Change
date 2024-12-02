library(shiny)
library(leaflet)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".leaflet-container { background: white; }"))
  ),

  # Application title
  titlePanel("Climate Change Impacts"),

  # Create a navigation bar
  navbarPage(
    "Hydrology",

    # First tab: Map and plot
    tabPanel("UK",
             fluidRow(
               column(width = 3,
                      radioButtons( # selectInput(selected = "Drought",
                        inputId = "selected_column",
                        label = "Choose a hazard:",
                        choices = c("Drought" = "LTQ95", "Medium Flows" = "Q50", "Flood" = "X1in10Flood"), # List of shapefile columns
                      ),
                      sliderInput(
                        inputId = "opacity",
                        label = "Adjust map transparency:",
                        min = 0, max = 1, value = 0.8, step = 0.1
                      ),
                      ),
               column(width = 5,
                      leafletOutput("display_map", height = "500px") # Map
                      ),
               column(width = 4) # holder for images
               )
             ),
    
    # Second tab: Information
    tabPanel("Parlimentary Constituencies",
             h1("Hydrology and Climate Change for UK Parlimentary Constituencies"),
             
             tags$div(
               tags$ul(
                 tags$li("In the UK, climate change is having direct and tangible impacts on local communities, causing warmer, wetter winters, hotter, drier summers, and more extreme floods and droughts."),
                 tags$li("The Met Office's high emissions scenarios suggest that the UK could warm to 2°C by the early 2030s unless carbon emissions are drastically reduced."),
                 tags$li("This infographic outlines the increased flood and drought risk in your constituency due to climate change⁺ in the hope that you, as an elected MP will protect our society and natural environment by passing responsible climate policies in government and developing sustainable and resilient practices locally.")
               )
             ),
             
             sidebarLayout(
               sidebarPanel(width =2,
                 radioButtons( # selectInput(selected = "Drought",
                   inputId = "selected_column_MP",
                   label = "Choose a hazard:",
                   choices = c("Drought" = "LTQ95", "Medium Flows" = "Q50", "Flood" = "X1in10Flood"), # List of shapefile columns
                   ),
                 textOutput('selected_MP'),
                 leafletOutput("selection_map", height = "500px")
                 ),
               mainPanel(
                 leafletOutput("MP_drought_map", height = "700px")), 
               position = c("left", "right"), fluid = TRUE)
    ),
    
    
    #     tabPanel("Constituencies",
    #          h1("Hydrology and Climate Change for UK Parlimentary Constituencies"),
    #          fluidRow(column(width = 2,
    #                          plotOutput("MP_inset_map", height = "150px")),
    #                          # leafletOutput("MP_inset_map", height = "200px")),
    #                   column(width = 3,
    #                          p(style="text-align: justify;",
    #                            "In the UK, climate change is having direct and tangible impacts on local communities, causing warmer, wetter winters, hotter, drier summers, and more extreme floods and droughts. The Met Office's high emissions scenarios suggest that the UK could warm to 2°C by the early 2030s unless carbon emissions are drastically reduced.")),
    #                   column(width = 3,
    #                          p(style="text-align: justify;",
    #                            "This infographic outlines the increased flood and drought risk in your constituency due to climate change⁺ in the hope that you, as an elected MP will protect our society and natural environment by passing responsible climate policies in government and developing sustainable and resilient practices locally."),)
    #                   ),
    #          fluidRow(
    #            column(width = 6,
    #                   # plotOutput("MP_drought_map"),
    #                   leafletOutput("MP_drought_map", height = "700px")),
    #            column(width = 6,
    #                   leafletOutput("MP_flood_map", height = "700px"))
    #                   # plotOutput("MP_flood_map"))
    #          ),
    #          
    #          fluidRow(
    #            column(width = 1),
    #            column(width = 5,
    #                   h3(style="color:darkred", "Drought Risk"),
    #                   p(style="text-align: justify; color:darkred",
    #                     "Research⁺ suggests that if the UK warms by 2°C average river flows in your constituency could decrease by [XX]%. Your local rivers are modelled to experience an additional [XX%] days of very low flow per year. Models show that average river flows will decrease across the UK, which could impact water availability national."),
    #                   h5(style="text-align: justify; color:darkred",
    #                      "Reduced river flows could have broad and direct consequences:"),
    #                   p(style="text-align: justify; color:darkred",
    #                     "- Water scarcity, causing shortages for drinking, agriculture, industry, energy production, and other essential uses."),
    #                   p(style="text-align: justify; color:darkred",
    #                     "- Damage to ecosystems, leading to habitat loss, reduced biodiversity, and degradation of natural landscapes.")
    #                   ),
    #            column(width = 5,
    #                   h3(style="color:darkblue",
    #                      "Flood Risk"),
    #                   p(style="text-align: justify; color:darkblue",
    #                     "Models of the rivers in your constituency show that high flows could [XXX] by [XXX]%. And, [% if('Region_nam' IN ('Midlands'), 'in the '||'Region_nam', 'in '||'Region_nam')%] as a whole, that high river flows could [XX] by [XX]%. Storm rainfall in your area could increase by [XX]% by 2050* causing increased rapid-onset surface water flooding."),
    #                   p(style="text-align: justify; color:darkblue",
    #                     "Changing rainfall patterns and increased flood risk are causing additional property and infrastructure damage, economic losses, and disruption to crucial services and agriculture, as well as environmental damage and potential loss of life.")),
    #            column(width = 1)
    #            )
    # ),

    # Third tab: Information
    tabPanel("About",
             h2("Data Information"),
             p("This tab describes how the data was generated."),
             p("Add more details about the dataset, such as the source, processing steps, and purpose.")
             )
    )
  )
