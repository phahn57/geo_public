

library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(lubridate)
library(knitr)
library(plotly)
library(ggpubr)
library(grid)
library(gridExtra)
library(kableExtra)
library(leaflet)


## Funktionen

#source("function.R", local=TRUE)  ## Funktionen einlesen
        
### Lade  Daten und ordne den Variablen zu !
load("y_prev.RData")
load("y_akt.RData")

### Sequenz für die x-Achse 
brk <- seq.int(1,12,1)
## Color Palette für Ar-DB
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#0033FF")



# Define UI ----
ui <- dashboardPage(
                
# App title ----
        dashboardHeader(title= paste("Geo_coding")),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Karte",tabName = "kart",icon=icon("map-o")),
                        radioButtons("yearset", "Jahr auswählen",
                                     c( "Aktuell" = "akt", "Vorjahr" = "prev")
                        )
                )
        ),
        
        dashboardBody(
                tabItems(
                        
                tabItem(tabName = "kart",
                         h4("Einzugsbereich stationärer, AOP Fälle"),
                        fluidRow( 
                        column(width=10,
                          leafletOutput("mapping",height=1000)
                        ),
                        column(width=2,
                               radioButtons("inVar7",
                                 label = "Abteilung:",
                                 choices = c("ORTH", "H_CHI", "S_CHI","F_CHI","Kinder","K_CHI"),inline=FALSE))
                               )
                        )
                
                ) ## closes tabItems
                ))

# Define server logic  ----
server <- function(input, output) {
        ## Wechsel zwischen den Jahren     
        y_data <- reactive({
                if (input$yearset == "akt")
                        y_akt
                else if (input$yearset == "prev")
                        y_prev
        })
        
      
        selectedmap <- reactive({
                 y_data()$map %>% select(plz,geometry,input$inVar7) %>% rename(number=input$inVar7)
        })
        
        
### Tab karte
        output$mapping <- renderLeaflet({m <- leaflet(selectedmap()) %>% setView(9.0807,49.2244, zoom=10) 
        bins = c(1,5,10,15,20,50,100,150,180)
        pal <- colorBin("BrBG", domain=selectedmap()$number, bins = bins)
        m %>% addPolygons(
                fillColor = ~pal(number),
                weight = 2,
                opacity= 1,
                color="white",
                dashArray = "1",
                fillOpacity = 0.4) %>% addTiles() %>% addLegend(pal=pal,values=~density, opacity=0.9, title=NULL) 
        })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)