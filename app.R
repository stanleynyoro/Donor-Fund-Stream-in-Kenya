library(shiny)
library(rvest)
library(dplyr)
library(tidyverse)
library(scales)

funding.d<-read.csv("https://data.humdata.org/dataset/d0bf93aa-bc16-426b-b53b-c8c90fac98a4/resource/da2fbb4e-126c-4c98-9cdc-f74f953f96d7/download/fts_incoming_funding_ken.csv")
funding.d <- funding.d[-1,]
funding.d$amountUSD<-as.numeric(funding.d$amountUSD)
funding.d<-funding.d %>%
    select(date,description,amountUSD,srcOrganization,destOrganization,destGlobalClusters,status)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme=shinythemes::shinytheme("cerulean"),
    titlePanel("Donor funding to INGOs in Kenya"),
    HTML("<p>Data has been scraped from <a href='https://data.humdata.org/dataset/fts-requirements-and-funding-data-for-kenya'>HDX Ocha services</a> and is regulary updated.</p>"),
    sidebarLayout(
        sidebarPanel(
            selectInput("srcOrganization","Donor:",unique(funding.d$srcOrganization)),
            dateRangeInput("date", "Date range:",
                           start  = min(funding.d$date),
                           end    = max(funding.d$date))
            ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Summary of funding by country",tableOutput("data.t")),
                tabPanel("Amount paid/committed",plotOutput("myplot"))
            )
        )
        )
)
     

# Define server logic required to draw a histogram
server <- function(input, output) {
    filtered.d<-reactive({
        subset(funding.d,date>=input$date[1] & date<=input$date[2])
    })
    paid_commit<-reactive({
        filtered.d() %>%
            filter(srcOrganization==input$srcOrganization) %>%
            group_by(status) %>%
            summarise(total_USD=sum(amountUSD)) %>%
            as.data.frame()
    })
    output$myplot<-renderPlot({
        data.p<-paid_commit() 
        options(scipen = 999)
        ggplot(data.p,aes(x=status,y=total_USD)) +
            geom_bar(stat = "identity", fill = "green", 
                     color = "black", width = 0.40)
    }) 

    output$data.t<-renderTable({
        filtered.d() %>%
            filter(srcOrganization==input$srcOrganization)
        
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
