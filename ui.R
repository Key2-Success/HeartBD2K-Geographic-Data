library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Frequency of Case Reports by Region"), br(), 

  sidebarLayout(
    sidebarPanel(
      selectInput("disease", "Disease System:",
                   c("All" = "all",
                     "Cancer" = "Cancer",
                     "Nervous" = "Nervous.System.Diseases",
                     "Cardiovascular" = "Cardiovascular.Diseases",
                     "Musculoskeletal and Rheumatological" = "Musculoskeletal.Diseases.and.Rheumatological.Diseases",
                     "Digestive" = "Digestive.System.Diseases",
                     "Obstetrical and Gynecological" = "Obstetrical.and.Gynecological.Diseases",
                     "Infectious" = "Infectious.Diseases",
                     "Respiratory Tract" = "Respiratory.Tract.Diseases",
                     "Hematological" = "Hematologic.Diseases",
                     "Kidney and Urological" = "Kidney.Diseases.and.Urologic.Diseases",
                     "Endocrine" = "Endocrine.System.Diseases",
                     "Oral and Maxillofacial" = "Oral.and.Maxillofacial.Diseases",
                     "Ophthalamological" = "Ophthalamological.Diseases",
                     "Otorhinolaryngological" = "Otorhinolaryngologic.Diseases",
                     "Skin" = "Skin.Diseases",
                     "Rare" = "Rare.Diseases"))
    , width = 3),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text1"), 
      textOutput("text2"),
      leafletOutput("map"),
      plotOutput("hist"),
      plotOutput("bar"),
      width = 9
    )
  )
)
