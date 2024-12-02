library(shiny)
library(leaflet)
# remotes::install_github("hrbrmstr/leafletCRS") # For changing CRS to BNG
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

# Load shapefile:
GIS_folder = 'C:/Users/nbs65/OneDrive - Newcastle University/OpenCL-IMPACT/ShinyApp/CLIMpact/GIS_layers/'

sh_hex <- st_read("C:/Users/nbs65/OneDrive - Newcastle University/OpenCL-IMPACT/Hexagon Infographic - 2024 Constituencies/02 - Output Data/Hexagon Grid with Statistics.shp")
sh_GB <- st_read(paste0(GIS_folder, "UK_outline_4326.shp"))
# sh_NI <- st_read("C:/Users/nbs65/OneDrive - Newcastle University/OpenCL-IMPACT/Hexagon Infographic - 2024 Constituencies/03 - Map Layers/UK Polygons/NI polygon BNG.shp")
sh_IR <- st_read(paste0(GIS_folder, "Ireland_outline_BNG.shp"))
sh_MP <- st_read("C:/Users/nbs65/OneDrive - Newcastle University/OpenCL-IMPACT/Hexagon Infographic - 2024 Constituencies/02 - Output Data/2024 Constituencies with Statistics.shp")

sh_hex <- st_transform(sh_hex, crs = 4326)
# sh_GB <- st_transform(sh_GB, crs = 4326)
# sh_NI <- st_transform(sh_NI, crs = 4326)
sh_IR <- st_transform(sh_IR, crs = 4326)
sh_MP <- st_transform(sh_MP, crs = 4326)


# Define custom CRS for EPSG:27700
crs_27700 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:27700",
  proj4def = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs",
  resolutions = 2^(20:6) # Zoom levels
)
  
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # Add an ID column for click identification
  sh_hex <- sh_hex %>% mutate(id = row_number())

  # Render the map
  output$display_map <- renderLeaflet({

    pal = colorNumeric("YlOrRd", sh_hex[[input$selected_column]])

    leaflet(sh_hex) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Does not work with 27700
      
    # leaflet(sh_hex, options = leafletOptions(crs = crs_27700)) %>%
      # addTiles(urlTemplate = "https://tiles.arcgis.com/tiles/{server}/arcgis/rest/services/Open_Zoomstack/{z}/{x}/{y}.png") %>% # BAD
      # addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%  # Works slightly with 27700
      # addTiles(urlTemplate = "http://tile.stamen.com/toner/{z}/{x}/{y}.png") %>% # BAD
      # addTiles(urlTemplate = "http://tile.stamen.com/terrain/{z}/{x}/{y}.png") %>% # BAD
      # addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      
      addPolygons(
        # data = sh_hex,
        fillColor = ~pal(sh_hex[[input$selected_column]]),
        fillOpacity = input$opacity,
        color = NA,    # Removes the stroke color
        weight = 0,    # Ensures no stroke line width.
        opacity = 0,   # (Optional) Further ensures stroke invisibility.
        layerId = ~id  # Use IDs to track clicks
      ) %>%
      addLegend(
        position = "bottomleft",      # Position of the legend
        pal = pal,                     # Use the same color palette
        values = ~sh_hex[[input$selected_column]],                 # Values to map to the legend
        title = paste("Values of", input$selected_column),  # Title for the legend
        opacity = 1,                    # Legend opacity
      )
  })
  
  output$selection_map <- renderLeaflet({
    leaflet(sh_MP) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = sh_MP, fillColor=NA, color = 'grey', opacity = 1, weight = 1, layerId = ~PCON24NM)
    })
  
  # output$MP_inset_map <- renderLeaflet({
  #   leaflet(sh_GB) #%>%
      # addPolygons(
      #   data = sh_GB,
      #   fillColor = 'grey'
      # )
  # })
  
  
  output$MP_inset_map <- renderPlot({ggplot(data = sh_GB) +
      geom_sf(fill = "grey", color = NA) +  # Add a thin border
      # geom_sf(data = sh_NI, fill = 'grey', color = NA) +  # Overlay a second shapefile
      geom_sf(data = sh_IR, fill = 'lightgrey', color = NA) +  # Overlay a second shapefile
      theme_minimal() +
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    })
  
  # Render the map
  output$MP_drought_map <- renderLeaflet({
    
    pal_drought = colorNumeric("OrRd", sh_hex[[input$selected_column_MP]])
    
    leaflet() %>%
      
      addPolygons(data = sh_GB, fillColor='darkgrey', color = NA) %>%
      # addPolygons(data = sh_NI, fillColor='darkgrey', color = NA) %>%
      addPolygons(data = sh_IR, fillColor='lightgrey', color = NA) %>%
      addPolygons(
        data = sh_hex,
        fillColor = ~pal_drought(sh_hex[[input$selected_column_MP]]),
        fillOpacity = 1, # input$opacity,
        color = NA,    # Removes the stroke color
        weight = 0,    # Ensures no stroke line width.
        opacity = 0,   # (Optional) Further ensures stroke invisibility.
        layerId = ~id  # Use IDs to track clicks
      )
  })

  # Render the map
  output$MP_flood_map <- renderLeaflet({
    
    pal_flood = colorNumeric("Blues", sh_hex[['X1in10Flood']])
    
    leaflet(options = leafletOptions(background='white')) %>% # options = leafletOptions(crs = crs_27700)
      
      addPolygons(data = sh_GB, fillColor='lightgrey', color = NA) %>%
      # addPolygons(data = sh_NI, fillColor='lightgrey', color = NA) %>%
      addPolygons(data = sh_IR, fillColor='darkgrey', color = NA) %>%
      addPolygons(
        data = sh_hex,
        fillColor = ~pal_flood(sh_hex[['X1in10Flood']]),
        # fillOpacity = input$opacity,
        color = NA,    # Removes the stroke color
        weight = 0,    # Ensures no stroke line width.
        opacity = 0,   # (Optional) Further ensures stroke invisibility.
        layerId = ~id  # Use IDs to track clicks
      )
  })
  
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
  
  
  # Reactive value to store the clicked polygon name
  selected_MP_polygon <- reactiveVal("Click on the map to select a constituency.")
  
  # Reactive value to store the ID of the selected polygon
  selected_polygon_id <- reactiveVal(NULL)
  
  # Listen for map clicks
  observeEvent(input$selection_map_shape_click, {
    # Extract the clicked polygon's name from the input
    selected_MP_polygon_name <- input$selection_map_shape_click$id
    selected_MP_polygon(selected_MP_polygon_name) # Update the reactive value
    selected_polygon_id(selected_MP_polygon_name) # Store the selected polygon ID
    
    # Highlight the clicked polygon in red
    leafletProxy("selection_map", session) %>%
      clearGroup("highlighted") %>% # Clear previous highlights
      addPolygons(
        data = sh_MP[sh_MP$PCON24NM == selected_MP_polygon_name, ], # Filter for the clicked polygon
        fillColor = "red", # Highlight color
        color = "black",    # Border color
        weight = 2,         # Border thickness
        fillOpacity = 0.8,  # Less transparency for highlight
        group = "highlighted" # Add to the "highlighted" group
      )
  }) 
  
  # Output the text for the clicked polygon
  output$selected_MP <- renderText({
    paste0("Selected: ", selected_MP_polygon())
  })
  
  
}
