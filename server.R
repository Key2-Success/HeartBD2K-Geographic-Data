library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(geojsonio)
library(scales)

load("data/allData.RData")
dataGeo <- geojsonio::geojson_read("data/statesAndCountries.js", what = "sp")

server <- function(input, output) {
  dataUpdated <- reactive({
    if(input$disease!="all"){
      return(allData %>% filter_(paste(input$disease,"==",1,sep="")))
    }else{
      return(allData)
    }
  })
  
  paletteData <- reactive({
    diseaseValues <- dataUpdated() %>% group_by(statesAndCountries) %>% summarise(n = n())
    bins <- min(diseaseValues$n):max(diseaseValues$n)
    pal <- colorBin("Blues", domain = diseaseValues$n, bins = bins)
    return(pal)
  })
  
  plotData <- reactive({
    diseaseValues <- dataUpdated() %>% group_by(statesAndCountries) %>% summarise(n = n())
    tmpGeo <- as.data.frame(dataGeo@data$name)
    names(tmpGeo) <- "countries"
    tmpGeo$id <- 1:nrow(tmpGeo) # to keep the sort order after merging
    tmpGeo <- merge(tmpGeo,diseaseValues, by.x="countries", by.y="statesAndCountries", all.x = TRUE)
    tmpGeo <- tmpGeo[order(tmpGeo$id), ]
    dataGeo@data$value <- tmpGeo$n
    return(dataGeo)
  })
  
  output$map <- renderLeaflet(leaflet() %>%
    setView(20, 40, 1) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))
    )
  )

  output$text1 <- renderText({
    paste("You have chosen: ", input$disease)})
  
  output$text2 <- renderText({
    dim <- dim(dataUpdated())[1]
    paste("There are ", dim, " case reports associated with ", input$disease)
  })
  
  
  observe({
    popup2 <- paste(plotData()$name,": ",plotData()$value)
    pal <- paletteData()
    leafletProxy("map") %>% clearShapes() %>% addPolygons(data = plotData(), layerId = ~id, fillColor = ~pal(value), weight = 2, opacity = 5,  color = "grey", fillOpacity = 0.7, popup = popup2)
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    countryName <- dataGeo@data$name[dataGeo@data$id==click$id]
    dataCountry <- dataUpdated() %>% filter(statesAndCountries==countryName) %>% select(Age, Gender)

    ageHist <- ggplot(data = dataCountry, aes(Age)) + 
      geom_histogram(aes(y = ..density..), fill = "lightblue", col = "white") + 
      geom_density(color = "darkgrey") + scale_x_continuous(name = "Age", breaks = seq(0, 120, 10)) + 
      xlim(0, 120) + scale_y_continuous(name = "Frequency") + ggtitle(paste("Distribution of Age in", input$disease)) + 
      theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = median(dataCountry$Age))
    
    # plot bar plot
    genderBar <- ggplot(data = dataCountry, aes(Gender)) + 
      geom_bar(fill = "lightblue", aes(y = (..count..)/sum(..count..))) + 
      xlab("Gender") + ylab("Percent") + ggtitle(paste0("Distribution of Gender in ", input$disease)) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      scale_y_continuous(labels = percent)
    
    
    output$hist <- renderPlot({ageHist})
    output$bar <- renderPlot({genderBar})
  })
}