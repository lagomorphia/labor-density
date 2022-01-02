library(geojsonio)
library(jsonlite)
library(reshape)
library(rgdal)
library(leaflet)
library(shiny)
library(htmltools)
library(spatialEco)

states <- readOGR(dsn="json/us-states.json")
total <- read.csv("state_union_density.csv", check.names = FALSE)

udata <- melt(total, id=c("state"))

newobj <- merge(states, udata, by.x="name", by.y="state", duplicateGeoms = T)
newobj <- newobj[,-c(2,3)]
newobj$variable <- as.Date(newobj$variable, "%Y")
newobj$variable <- format(newobj$variable, format="%Y")
newobj$variable <- as.numeric(newobj$variable)
newobj <- sp.na.omit(newobj, col.name="value", margin = 1)
bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 100)
pal <- colorBin("inferno", domain = newobj$value, bins = bins, reverse=TRUE)

df <- as.data.frame(newobj)

popup <- paste("<b>State: </b>", 
               newobj$name,
               "<br><b>Year: </b>", 
               newobj$variable,
               "<br><b>Union Density: </b>",
               newobj$value) %>% 
  lapply(htmltools::HTML)

ui <- fluidPage(
  HTML("<h3>United States Labor Union Membership Density (1964-2018)</h3>"),
  mainPanel(
    sliderInput("year", "Year", min = min(newobj$variable),
                max = max(newobj$variable),
                value="1964", sep="", animate = animationOptions(interval = 1000, playButton="Play")),
    uiOutput(outputId = "selected_var"),
    leafletOutput("mymap")
  ))

server <- function(input, output, session) {
  
  filteredData <- reactive({
    newobj[newobj$variable == input$year,]
  })
  
  output$selected_var <- renderText({ 
    HTML(paste("<h3><b>Year: ",input$year))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(newobj) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  observe({    
    leafletProxy("mymap", data=filteredData()) %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = paste("<b>State: </b>", 
                      filteredData()$name,
                      "<br><b>Year: </b>", 
                      filteredData()$variable,
                      "<br><b>Union Density: </b>",
                      filteredData()$value) %>% 
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
  })
  
}

shinyApp(ui, server)


