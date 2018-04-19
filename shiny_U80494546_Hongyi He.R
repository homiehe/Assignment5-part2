library(tidyverse)
library(zoo)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(leaflet)

#load the data
data <- read.csv("data.csv")
data <- as.tibble(data)
data_h <- data
data$Time <- as.Date(data$Time)

#load avg_data
data_avg <- read.csv("data_avg.csv")
data_avg <- as.tibble(data_avg)

#load veg data
data_veg <- read.csv("veg_data3.csv")
data_veg <- as.tibble(data_veg)
data_veg <- data_veg %>% filter(label == "RESTRICTED USE CHEMICAL", Unit.of.Measurement==" MEASURED IN LB")
tox <- read_csv("chemical_tox_shiny.csv")
tox <- tox %>% select(-1)
broc <- read_csv("bbroc_no_log.csv")
Caul <- read_csv("Ccaul_no_log.csv")

#prepare the data
tox$`Values for LD50 on rats` <- tox$`Values for LD50 on rats`*88.768027

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Temperature", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Vegetable", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 12,
#####
                  box(
                    title = "Select the range of year",
                    status="success",
                    solidHeader = TRUE,
                    sliderInput("max", 
                              label = "Range of year:",
                              min = 1987, max = 2017, value = c(2016,2017)),
                    hr(),
                    radioButtons("Type", "Type of temperature:",
                               c("Air Temperature" = 'ATMP',
                                 "Water Temperature" = 'WTMP')),
                    hr(),
                    helpText("This is the air and sea temperature from 1987 to 2017 recorded by a buoy in the middle of Berling Sea. Which shows a remarkable increase.")
                      ),
#####

#####
                  tabBox(
                    title = "Berling Sea Temperature",
                  
                    id = "tabset1",
                  
                    side = "left", 
                  
                    height = "250px",
                  
                    selected = "Daily air temperature",
                  
                    tabPanel("Daily",
                           status="success",
                           plotOutput("graph")),
                  
                    tabPanel("Yearly",
                           status="success",
                           plotOutput("graph2")),
                  
                    tabPanel("Regression line",
                           status="success",
                           plotOutput("graph3")
                  )
                )
#####
              ),

                column(width = 6,
                       box(title = "Buoy Location", status = "warning", width = NULL, 
                           leafletOutput("buoy_map")
                       )
              )
      )),
      
tabItem(tabName = "widgets",
        fluidRow(
          box(
            selectInput("a", "Choose a commodity:",choices = c(2006,2010,2014,2016)),
            selectInput("b", "Choose a commodity:",choices = c("BROCCOLI","CAULIFLOWER"))
          ),
          tabBox(
            tabPanel("Chemcials content of vegetable",
                     status = "success",
                     solidHeader = TRUE,
                     plotOutput("graph4"),              
                     hr(),
                     helpText("Amount of toxic chemicals contained in vegetable. Data from EPA"),
                     verbatimTextOutput("dateText")
            ),
            
            tabPanel("Toxicity of Chemcials",
                     status = "success",
                     solidHeader = TRUE,
                     plotOutput("graph5"),
                     hr(),
                     helpText("The LD50(the amount of the substance required to kill 50% of the test population.) level of the chemical that can be detected in vegtables. Data from EPA")
            ),
            
            tabPanel("Chemicals content of vegetable compared to their toxicity",
                     status = "warning",
                     solidHeader = TRUE,
                     plotOutput("graph6"),
                     hr(),
                     helpText("Chemicals content of vegetable compared to their LD50 level.Data from EPA")
            )
          )
        )
)
    )
  )
)

server <- function(input, output) {
  
  a <- reactive({
    filter(data, YYYY>input$max[1]) %>% filter(YYYY<input$max[2]) %>% filter(hh==12)
  })
  
  b <- reactive({
    filter(data_avg, YYYY>input$max[1]) %>% filter(YYYY<input$max[2]) 
  })
  
  c <- reactive({
    filter(data_h, YYYY>input$max[1]) %>% filter(YYYY<input$max[2])
  })
  
  d <- reactive({
    filter(data_veg, Year==input$a) %>% filter(Commodity == input$b)
  })
  
  e <- reactive({
    if(input$b=="CAULIFLOWER"){
      filter(Caul, Year==input$a)
    }else{
      filter(broc, Year==input$a)
    }
  })
  
  output$graph <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(a(), aes(x = Time)) + 
        geom_line(aes(y = ATMP), colour="green", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }else if(input$Type=="WTMP"){
      ggplot(a(), aes(x = Time)) + 
        geom_line(aes(y = WTMP), colour="green", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }
  })
  
  output$graph2 <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(b(), aes(x = YYYY)) + 
        geom_smooth(aes(y = avg_atmp), colour="blue", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }else if(input$Type=="WTMP"){
      ggplot(b(), aes(x = YYYY)) + 
        geom_smooth(aes(y = avg_wtmp), colour="blue", size = 0.5) +
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }
  })
  
  output$graph3 <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(a(),aes(x=Time,y=ATMP))+
        geom_point()+geom_smooth(method="lm")+
        ggtitle("reg ATMP")
    }else if(input$Type=="WTMP"){
      ggplot(a(),aes(x=Time,y=WTMP))+
        geom_point()+
        geom_smooth(method="lm")+
        ggtitle("reg WTMP")
    }
  })
  
  output$graph4 <- renderPlot({
    ggplot(d(), mapping=aes(x= Name, y=Value )) + 
      geom_bar(stat="identity", position="dodge",aes(fill=Name)) + 
      coord_flip()+
      labs(y = "Values(mg) ",x = "Chemical Name")
  })
  
  output$graph5 <- renderPlot({
    data <- as.data.frame(tox)
    rownames(data) <- data[,1]
    ggplot(data,mapping=aes(x = Name, y=`Values for LD50 on rats`))+
      geom_bar(stat="identity", position="dodge",aes(fill=Name))+
      coord_flip()+
      labs(y = "Values(mg/kg)",x = "Chemical Name")
  })
  
  output$graph6 <- renderPlot({
    ggplot(e(), aes(x= Name, y=tox )) + 
      geom_bar(stat="identity",position = "dodge") + 
      labs(y = "Relative Toxicity",x = "Chemical Name") +
      coord_flip()+
      labs(title=paste(" Real content(mg) / LD50(mg/kg) for human of",input$b))
  })
  
  output$buoy_map <- renderLeaflet({
    m <- leaflet() %>% addTiles() %>% setView(177.738, 57.026, zoom = 4) %>% 
      addMarkers(lng=177.738, lat=57.026, popup="The Buoy (ID:46035)") 
    m
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

