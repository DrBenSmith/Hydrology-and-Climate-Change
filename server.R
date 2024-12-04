# --- Server --- #

# Preamble ----------------------------------------------------------------

# Load Packages
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
# remotes::install_github("hrbrmstr/leafletCRS") # For changing CRS to BNG

# Set paths:
root = 'C:/Users/nbs65/OneDrive - Newcastle University/OpenCL-IMPACT/'
GIS_folder = paste0(root, 'RShinyApp-Hydrology-and-Climate-Change/GIS_layers/')

# Load shapefiles:
sh_MP <- st_read(paste0(root, "Hexagon Infographic - 2024 Constituencies/02 - Output Data/2024 Constituencies with Statistics.shp"))
sh_hex <- st_read(paste0(root, "Hexagon Infographic - 2024 Constituencies/02 - Output Data/Hexagon Grid with Statistics.shp"))
sh_GB <- st_read(paste0(GIS_folder, "UK_outline_4326.shp"))
sh_IR <- st_read(paste0(GIS_folder, "Ireland_outline_BNG.shp"))

# Load in the hazard statistics for the constituencies:
csv_centroids <- read.csv(paste0(GIS_folder, "Constituency Data 1.csv"), header = TRUE)

# Transform shapefiles to correct CRS:
sh_hex <- st_transform(sh_hex, crs = 4326)
sh_IR <- st_transform(sh_IR, crs = 4326)
sh_MP <- st_transform(sh_MP, crs = 4326)

# # Define custom CRS for EPSG:27700
# crs_27700 <- leafletCRS(
#   crsClass = "L.Proj.CRS",
#   code = "EPSG:27700",
#   proj4def = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs",
#   resolutions = 2^(20:6) # Zoom levels
# )


# Server Function ---------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # Add an ID column for click identification
  sh_hex <- sh_hex %>% mutate(id = row_number())
  
  # Build a map to hold MP constituencies for users to select:
  output$map_constituency <- renderLeaflet({
    leaflet(sh_MP) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = sh_MP, fillColor=NA, color = 'grey', opacity = 1, weight = 1, layerId = ~PCON24NM) %>%
      setView(lng = -2, lat = 55, zoom = 5) %>%
      setMaxBounds(lng1 = -11, lat1 = 50, lng2 = 1, lat2 = 60)
    })
  
  # Render the main hazard map
  output$map_hazard <- renderLeaflet({
    
    # Set the colour pallet:
    map_colour = if(input$selected_column_MP == "LTQ95"){"OrRd"} else {"Blues"} 
    pal_drought = colorNumeric(map_colour, sh_hex[[input$selected_column_MP]])
    
    legend_title = if (input$selected_column_MP == "LTQ95"){
      "Additional 'Low Flow Days' Annually"
      } else if (input$selected_column_MP == "Q50") {
        "% Change in Median Flow"
        } else if (input$selected_column_MP == "X1in10Flood") {
          "% Change in 1 in 10 Year Flow"
          }
    
    # Initialize the map:
    leaflet() %>%
      
      # Add shapefile data:
      addPolygons(data = sh_GB, fillColor='darkgrey', color = NA) %>%
      addPolygons(data = sh_IR, fillColor='lightgrey', color = NA) %>%
      addPolygons(
        data = sh_hex,
        fillColor = ~pal_drought(sh_hex[[input$selected_column_MP]]),
        fillOpacity = 1, # input$opacity,
        color = NA,      # Removes the stroke color
        weight = 0,      # Ensures no stroke line width.
        opacity = 0,     # (Optional) Further ensures stroke invisibility.
        layerId = ~id    # Use IDs to track clicks
      ) %>%
      addLegend(
        pal = pal_drought, values = sh_hex[[input$selected_column_MP]], # Values to map to the legend
        position = "topleft", title = legend_title, opacity = 1,
      ) %>%
      setView(lng = -2, lat = 55, zoom = 5) %>%
      setMaxBounds(lng1 = -11, lat1 = 50, lng2 = 1, lat2 = 60)
  })
  
  # Create reactive value to store the clicked constituency name:
  selected_MP_polygon <- reactiveVal("Click on the map to select a constituency.")
  
  # Create reactive value to store the ID of the selected polygon:
  selected_polygon_id <- reactiveVal(NULL)
  
  # Listen for map clicks
  observeEvent(input$map_constituency_shape_click, {
    # Extract the clicked polygon's name from the input
    selected_MP_polygon_name <- input$map_constituency_shape_click$id
    selected_MP_polygon(selected_MP_polygon_name) # Update the reactive value
    selected_polygon_id(selected_MP_polygon_name) # Store the selected polygon ID
    
    # Create a constituency map and highlight the clicked polygon in red:
    leafletProxy("map_constituency", session) %>%
      clearGroup("highlighted") %>% # Clear previous highlights
      addPolygons(
        data = sh_MP[sh_MP$PCON24NM == selected_MP_polygon_name, ], # Filter for the clicked polygon
        fillColor = "red",    # Highlight color
        color = "black",      # Border color
        weight = 2,           # Border thickness
        fillOpacity = 0.8,    # Less transparency for highlight
        group = "highlighted" # Add to the "highlighted" group
      )
    
    
    output$hazard_text_body <- renderText({
      
      constituency_statistics = csv_centroids[csv_centroids$Name == selected_MP_polygon_name,]
      
      text_body = if (input$selected_column_MP == "LTQ95"){
        paste0(
          "Research⁺ suggests that if the UK warms by 2°C average river flows in your constituency could decrease by  ", round(constituency_statistics$Q50,0), "%.
          <p><p>
          Your local rivers are modelled to experience an additional ", round(constituency_statistics$LTQ95,0), " days of very low flow per year. Models show that average river flows will decrease across the UK, which could impact water availability national.
          <p><p>
          Reduced river flows could have broad and direct consequences:
          <p>
          <ul>", 
          paste0(
            "<li>", 
            c("Water scarcity, causing shortages for drinking, agriculture, industry, energy production, and other essential uses.", 
              "Damage to ecosystems, leading to habitat loss, reduced biodiversity, and degradation of natural landscapes."),
            "</li>", collapse = ""),
          "</ul>")
        
      } else if (input$selected_column_MP == "Q50") {
        "Median Flows - WRITE SOME TEXT"
        
      } else if (input$selected_column_MP == "X1in10Flood") {
        paste0("Models of the rivers in your constituency show that high flows could ",
               constituency_statistics[["1in10D"]],
               " by ", round(constituency_statistics$X1in10yrF,0), "%. And, ",
               if(constituency_statistics$Region_nam == 'Midlands'){
                 paste0('in the ', constituency_statistics$Region_nam)
               } else{
                 paste0('in ', constituency_statistics$Region_nam)
               }, " as a whole, that high river flows could ", round(constituency_statistics$Reg_1in10D,0), "% by ", constituency_statistics$Reg1in10yr, 
               ". Storm rainfall in your area could increase by ", constituency_statistics$RainUplift, "% by 2050* causing increased rapid-onset surface water flooding.
      <p><p>
      Changing rainfall patterns and increased flood risk are causing additional property and infrastructure damage, economic losses, and disruption to crucial services and agriculture, as well as environmental damage and potential loss of life.")
      } else {
        "Select a hazard to display on the map..."
      }
      
      text_colour = if (input$selected_column_MP == "LTQ95"){
        "darkred"
      } else if (input$selected_column_MP == "Q50") {
        "darkblue"
      } else if (input$selected_column_MP == "X1in10Flood") {
        "darkblue"
      }
      
      
      # Return the final text wrapped in <h3> with styling
      paste0('<div style="text-align: justify"><span style="color:', text_colour, '">', text_body, '</span></div>')
    })
    
  }) 
  
  # Output the text for the clicked polygon
  output$selected_MP <- renderText({paste0("Selected: ", selected_MP_polygon())})
  
  
  # Render the text based on the map:
  output$hazard_title <- renderText({input$selected_column_MP})
  
  output$hazard_text_title <- renderText({
    text_title = if (input$selected_column_MP == "LTQ95"){
      "Drought Risk"
      } else if (input$selected_column_MP == "Q50") {
        "Median Flows"
        } else if (input$selected_column_MP == "X1in10Flood") {
          "Flood Risk"
          } else {
            "Select a hazard to display on the map..."
          }
    text_colour = if (input$selected_column_MP == "LTQ95"){
      "darkred"
      } else if (input$selected_column_MP == "Q50") {
        "darkblue"
        } else if (input$selected_column_MP == "X1in10Flood") {
          "darkblue"
          }
    # Return the final text wrapped in <h3> with styling
    paste0('<h3 style="color:', text_colour, '">', text_title, '</h3>')
    })
  
}


# Notes -------------------------------------------------------------------

# # These are tiles that should work BNG but don't really:
# leaflet(sh_hex, options = leafletOptions(crs = crs_27700)) %>%
# addTiles(urlTemplate = "https://tiles.arcgis.com/tiles/{server}/arcgis/rest/services/Open_Zoomstack/{z}/{x}/{y}.png") %>% # BAD
# addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%  # Works slightly with 27700
# addTiles(urlTemplate = "http://tile.stamen.com/toner/{z}/{x}/{y}.png") %>% # BAD
# addTiles(urlTemplate = "http://tile.stamen.com/terrain/{z}/{x}/{y}.png") %>% # BAD
# addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%

# output$MP_inset_map <- renderPlot({ggplot(data = sh_GB) +
#     geom_sf(fill = "grey", color = NA) +  # Add a thin border
#     # geom_sf(data = sh_NI, fill = 'grey', color = NA) +  # Overlay a second shapefile
#     geom_sf(data = sh_IR, fill = 'lightgrey', color = NA) +  # Overlay a second shapefile
#     theme_minimal() +
#     theme(axis.text = element_blank(), 
#           axis.ticks = element_blank(),
#           panel.grid = element_blank())
# })


# output$display_map <- renderLeaflet({
#   
#   # Set the colour palet:
#   pal = colorNumeric("YlOrRd", sh_hex[[input$selected_column_MP]])
#   
#   # Initialise the map:
#   leaflet(sh_hex) %>%
#     
#     # Add a basemap:
#     addProviderTiles(providers$CartoDB.Positron) %>% # Does not work with 27700
#     
#     # Add the hexegon grid for the correct hazard:
#     addPolygons(
#       fillColor = ~pal(sh_hex[[input$selected_column_MP]]),
#       fillOpacity = input$opacity,
#       color = NA,    # Removes the stroke color
#       weight = 0,    # Ensures no stroke line width.
#       opacity = 0,   # (Optional) Further ensures stroke invisibility.
#       layerId = ~id  # Use IDs to track clicks
#     ) %>%
#     addLegend(
#       position = "bottomleft",      # Position of the legend
#       pal = pal,                     # Use the same color palette
#       values = ~sh_hex[[input$selected_column_MP]],                 # Values to map to the legend
#       title = paste("Values of", input$selected_column_MP),  # Title for the legend
#       opacity = 1,                    # Legend opacity
#     )
# })


# # Render the map
# output$MP_flood_map <- renderLeaflet({
#   
#   pal_flood = colorNumeric("Blues", sh_hex[['X1in10Flood']])
#   
#   leaflet(options = leafletOptions(background='white', minZoom = 11)) %>% # options = leafletOptions(crs = crs_27700)
#     
#     addPolygons(data = sh_GB, fillColor='lightgrey', color = NA) %>%
#     # addPolygons(data = sh_NI, fillColor='lightgrey', color = NA) %>%
#     addPolygons(data = sh_IR, fillColor='darkgrey', color = NA) %>%
#     addPolygons(
#       data = sh_hex,
#       fillColor = ~pal_flood(sh_hex[['X1in10Flood']]),
#       # fillOpacity = input$opacity,
#       color = NA,    # Removes the stroke color
#       weight = 0,    # Ensures no stroke line width.
#       opacity = 0,   # (Optional) Further ensures stroke invisibility.
#       layerId = ~id  # Use IDs to track clicks
#     )
# })

# output$MP_flood_map <- renderPlot({ggplot(data = sh_GB) +
#     geom_sf(fill = "grey", color = NA) +  # Add a thin border
#     geom_sf(data = sh_NI, fill = 'grey', color = NA) +  # Overlay a second shapefile
#     geom_sf(data = sh_IR, fill = 'lightgrey', color = NA) +  # Overlay a second shapefile
#     # geom_sf(data = sh_hex, fill = "LTQ95", color = NA) +
#     geom_sf(data = sh_hex, aes(fill = LTQ95), color = "black", size = 0.2)
#     theme_minimal() +
#     theme(axis.text = element_blank(), 
#           axis.ticks = element_blank(),
#           panel.grid = element_blank())
# })

# h3(style="color:darkred", "Drought Risk"),
# p(style="text-align: justify; color:darkred", 