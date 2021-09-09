library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(leaflet)
library(data.table)
library(fmsb)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #includeCSS(url("https://raw.githubusercontent.com/pratik0510-p/TE10-Zombean-EcoGhar/main/bootstrap.css?token=AVHUGEABT2XKQPOATDLHAO3BIKZ5K")),
    # Application title
    titlePanel("Sustaibility Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("Postcode", "Postcode you live in", value = "3000"),
            sliderInput("Household_size",
                        "Number of members in your household:",
                        min = 1,
                        max = 10,
                        value = 2),
            selectInput("solar_info", "Do you have solar panel installed on your household ?",
                        c("Yes", "No")),
            selectInput("rain_info", "Do you have rain water harvesting tools installed on your household ?",
                        c("Yes", "No")),
            textInput("Electricity", "How much Electricity is used", value = "25"),
            textInput("Gas", "How much Gas is used", value = "150"),
            textInput("Water", "How much Water is used", value = "1"),
            textInput("Waste", "How much Waste is used", value = "2")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot", height = 'auto', width = 'auto'),
            #dataTableOutput("DirMovie_stat")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$DirMovie_stat<-renderDataTable({
        Postcode <- input$Postcode
        H_size <- input$Household_size
        Electricity <- input$Electricity
        Gas <- input$Gas
        Water <- input$Water
        Waste <- input$Waste
        
        GHG_Factors <- read.csv(url("https://raw.githubusercontent.com/pratik0510-p/TE10-Zombean-EcoGhar/main/GHG_filters.csv?token=AVHUGEBD7DPK4UKM7I4ZKGLBILH54"))
        Flora_Fauna <- read.csv(url("https://raw.githubusercontent.com/pratik0510-p/TE10-Zombean-EcoGhar/main/Flora_Fauna_score.csv?token=AVHUGEGGRG4NMO63M3ONYDLBILH76"))
        
        carbon_measure<- H_size*(46.849)
        
        if (H_size > 4) {
            H_size <- "5+"
        }
        
        temp_Data <- GHG_Factors %>% filter(Household_size == H_size)
        temp_Data <- temp_Data[c("Household_size","Type","per_day")]
        temp_Data
    
        
    })
    
    output$distPlot<-renderPlotly({
        Postcode <- input$Postcode
        H_size <- input$Household_size
        Electricity <- input$Electricity
        Gas <- input$Gas
        Water <- input$Water
        Waste <- input$Waste
        solar_info <- input$solar_info
        rain_info <- input$rain_info
        
        if (solar_info == "Yes"){
            solar_info <- 2
        }
        if (solar_info == "No") {
            solar_info <- 0
        }
        
        if (rain_info == "Yes"){
            rain_info <- 2
        }
        if (rain_info == "No") {
            rain_info <- 0
        }
        
        carbon_measure<- as.numeric(H_size)*(46.849)
        
        GHG_Factors <- read.csv(url("https://raw.githubusercontent.com/pratik0510-p/TE10-Zombean-EcoGhar/main/GHG_filters.csv?token=AVHUGEBD7DPK4UKM7I4ZKGLBILH54"))
        Flora_Fauna <- read.csv(url("https://raw.githubusercontent.com/pratik0510-p/TE10-Zombean-EcoGhar/main/Flora_Fauna_score.csv?token=AVHUGEGGRG4NMO63M3ONYDLBILH76"))
        
        if (H_size > 4) {
            H_size <- "5+"
        }
        
        temp_Data <- GHG_Factors %>% filter(Household_size == H_size)
        value <- as.numeric(temp_Data[2,5])
        
        Carbon_footprint = (as.numeric(Electricity) * as.numeric(temp_Data[1,10])) + (as.numeric(Gas) * as.numeric(temp_Data[2,10]))+ (as.numeric(Water) * as.numeric(temp_Data[3,10]))+ (as.numeric(Waste) * as.numeric(temp_Data[4,10]))
        Tree_Carbon_offset = as.numeric(Carbon_footprint/0.06)
        
        if(Carbon_footprint <= (carbon_measure/4)){
            carbon_score = 2
        }
        
        if(between(Carbon_footprint, (carbon_measure/4), (carbon_measure/2))){
            carbon_score = 2
        }
        
        if(between(Carbon_footprint, (carbon_measure/2), ((3*carbon_measure)/4))){
            carbon_score = 1.5
        }
        
        if(between(Carbon_footprint,((3*carbon_measure)/4),carbon_measure)){
            carbon_score = 1
        }
        
        if(Carbon_footprint > (carbon_measure)){
            carbon_score = 0
        }
        
        F_F_Data <- Flora_Fauna %>% filter(POSTCODE == Postcode)
        Fauna_Score_info <- as.numeric(F_F_Data[1,6])
        Flora_Score_info <- as.numeric(F_F_Data[1,7])
        
        total_score <- Fauna_Score_info + Flora_Score_info + rain_info + solar_info + carbon_score
        
        fig <- plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value =  total_score,
            delta = list(reference = 6, increasing = list(color = "green")),
            gauge = list(
                axis = list(range = list(NULL, 10), tickwidth = 1, tickcolor = "blue"),
                bar = list(color = "Green"),
                bgcolor = "white",
                borderwidth = 2,
                bordercolor = "gray",
                steps = list(
                    list(range = c(0, 5), color = "lightgreen"),
                    list(range = c(5, 7), color = "yellow")),
                threshold = list(
                    line = list(color = "red", width = 4),
                    thickness = 0.75,
                    value = 6))) 
        fig <- fig %>%
            layout(
                margin = list(l=20,r=30),
                paper_bgcolor = "lavender",
                font = list(color = "darkblue", family = "Arial"))
        
        fig
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
