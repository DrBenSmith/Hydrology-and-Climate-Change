library(shiny)
library(leaflet)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".leaflet-container { background: white; }")),
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$style(HTML("fancy_border {border:dotted 5em green}"))
  ),

  # Application title
  titlePanel("Climate Change Impacts"),

  # Create a navigation bar
  navbarPage("Hydrology",

    # # First tab: Map and plot
    # tabPanel("UK",
    #          fluidRow(
    #            column(width = 3,
    #                   radioButtons( # selectInput(selected = "Drought",
    #                     inputId = "selected_column",
    #                     label = "Choose a hazard:",
    #                     choices = c("Drought" = "LTQ95", "Medium Flows" = "Q50", "Flood" = "X1in10Flood"), # List of shapefile columns
    #                   ),
    #                   sliderInput(
    #                     inputId = "opacity",
    #                     label = "Adjust map transparency:",
    #                     min = 0, max = 1, value = 0.8, step = 0.1
    #                   ),
    #                   ),
    #            column(width = 5,
    #                   leafletOutput("display_map", height = "500px") # Map
    #                   ),
    #            column(width = 4) # holder for images
    #            )
    #          ),
    
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
               sidebarPanel(width=2,
                            radioButtons( # selectInput(selected = "Drought",
                              inputId = "selected_column_MP",
                              label = "Choose a hazard:",
                              choices = c("Drought" = "LTQ95", "Medium Flows" = "Q50", "Flood" = "X1in10Flood") # List of shapefile columns
                              ),
                            # h5("Choose a Constituency:"),
                            textOutput('selected_MP'),
                            leafletOutput("map_constituency", height = "500px")
                            ),
               
               mainPanel(
                 
                 column(width=8,
                        div(
                          style = "border: 1px solid black; border-radius: 5px; padding: 5px; margin: 0px;",
                        # div(class = 'fancy_border', ## set CSS class to style border
                            leafletOutput("map_hazard", height = "700px")
                            )
                        ),
                 column(width=4,
                        htmlOutput("hazard_text_title"),
                        htmlOutput("hazard_text_body")
                        
                        # h1(textOutput('hazard_title')),
                        # p(textOutput('hazard_text'))
                        )
                 ),
               
               position = c("left", "right"), 
               fluid = TRUE),
             
             hr(),
             
             # PAGE 2
             
             h2("Additional information"),
             
             column(width=6,
                    h3("My area has increased floods & increased droughts..."),
                    p("This is the case for many catchments around the UK. In the north and west, we see catchments getting wetter, while in the south and east we see catchments getting drier. In general, there is an increase in extremes: higher highs, and lower lows. Furthermore, while river flooding could reduce in some catchments, the intensity of storm rainfall is predicted to increase across the UK."),
                    
                    h3("What are ‘low flow days’?"),
                    p("These are days when the river flow is below a certain threshold, which is the daily flow that is exceeded 95% of the time. Typically, there are 18 low flow days in a year and 346 days when the flow is above this threshold. 'Additional low flow days' count the extra days each year when the flow is below the historical low flow threshold."),
                    
                    h3("How are statistics calculated?"),
                    p("Statistics are calculated from hydrological models of river catchments within your
constituency. These models use rainfall inputs to simulate the flow of water in rivers, along the ground surface, and through soils and aquifers. Models are developed and quality checked using recorded data to ensure that all models are accurate and capable of providing plausible future flow estimates. The graph below shows an accuracy report for one of the catchment models from your area:"),
                    
                    h3("[HYDROGRAPH]"),
                    p("Catchment [XXX]"),
                    
                    p("Historical rainfall data is then swapped with the Met Office’s 12 future climate datasets (UKCP18). Model results are summarised by taking the median (or average) change per river catchment for low (Q95), median (Q50), and high flows (1-in-10-year flood) at 2°C of warming. As this is modelled to occur around 2031, results from approximately 2015-2045 are compared to a baseline of 1980-2010.")
                    ),
             
             column(width=6,
                    h3("[PAGE 2 MAPS]"),
                    
                    h3("How likely are these climate scenarios?"),
                    p("The Met Office's UKCP18 climate scenarios assume a high warming scenario (RCP 8.5), assuming greenhouse gas emissions
continue to grow without reduction. If emissions are rapidly reduced then warming, and the hydrological impacts shown here, will be reduced."),
                    
                    h3("Why are we contacting you?"),
                    p("We are contacting MPs and councils across the UK to disseminate recent research into the affects of climate change on UK river flows. We want to ensure active engagement with government and policy makers as urgent action on climate change is vital for ensuring the safety, well-being and health of our communities, businesses, and countryside."),
                    
                    h3("Can I get more information or provide feedback?"),
                    tags$div(
                      tags$ul(
                        tags$li("For additional information, please contact us directly at Ben.Smith4@ncl.ac.uk (Newcastle University | Water Group)."),
                        tags$li("Feedback and links to infographics for other UK constituencies can be given / accessed via this form."),
                        tags$li("A resource created by UCL detailing changes to temperature and rainfall at a constituency level can be found here: Climate change projections by constituency over Great Britain")
                        )
                      )
                    ),
             h4("While median model results are shown on the maps for simplicity, results show that all areas have potential for increased flood risk.")
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
    #                   # plotOutput("map_hazard"),
    #                   leafletOutput("map_hazard", height = "700px")),
    #            column(width = 6,
    #                   leafletOutput("MP_flood_map", height = "700px"))
    #                   # plotOutput("MP_flood_map"))
    #          ),
    #          
    #          fluidRow(
    #            column(width = 1),
    #            
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
