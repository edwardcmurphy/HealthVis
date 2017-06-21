library(shiny)
library(shinydashboard)
library(foreign)
library(choroplethr)
library(choroplethrMaps)
library(plyr)
library(ggplot2)
library(stringi)

#Load the data
  health_aging <- read.csv("Healthy_Aging_Data.csv", header = TRUE)

#Keep relevant columns
  health_aging <- health_aging[ , c("YearStart","LocationAbbr","LocationDesc","Topic","Question","Data_Value")]
  
#Drop non-states
  #Includes D.C.
  health_aging <- health_aging[ ! health_aging$LocationAbbr %in% c("GU","MDW","NRE","PR","SOU","US","VI","WEST","AK","HI"), ]
  #List: GU MDW NRE PR SOU US VI WEST

#Attach dataframe
  attach(health_aging)
  
#Prepping for our inputs
  #Year input
    year_list <- unique(YearStart)
    yearmin <- min(year_list)
    yearmax <- max(year_list)
  #Topic input and question display
    topic_list <- unique(Topic)
    question_list <- unique(Question)
    
  #Loading the continental us states vector
    data(continental_us_states)
    
#######
# App #
#######

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "HealthVis", titleWidth = 350),
  dashboardSidebar(
    #Setting up the sidebar
      width = 350,
    #Topic input
      selectInput(inputId = "topic", choices = topic_list, label = "Select a topic", width = 350),
    #Year Input
      sliderInput(inputId = "year", label = "Year", min = yearmin, max = yearmax, value = yearmin, step = 1, width = 350, sep = "")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    box(
      title = textOutput('title'), width = 12, solidHeader = TRUE, status = "success",
      plotOutput('map')
    ),
    infoBoxOutput('max'),
    infoBoxOutput('min'),
    infoBoxOutput('avg')
  )
)
server <- function(input, output){
  
  data_sub <- reactive({
    df <- aggregate(Data_Value~LocationDesc, data = subset(health_aging, Topic==input$topic & YearStart==input$year), mean, na.rm = TRUE)
    df$LocationDesc <- tolower(df$LocationDesc)
    df <- rename(df, c("LocationDesc"="region", "Data_Value"="value"))
    df
  })
  
  ### DYNAMICALLY UPDATE YEARS?
  ### DYNAMICALLY UPDATE STEPS?
  
  output$title <- renderText(paste(question_list[match(input$topic, topic_list)], ".", sep = ""))
  
  output$map <- renderPlot({
    c = StateChoropleth$new(data_sub())
    c$legend = "Value"
    c$set_num_colors(1)
    c$set_zoom(continental_us_states)
    c$show_labels = FALSE
    map_out = c$render() + theme(
      rect = element_rect(fill='transparent', color='transparent')
    )
    map_out
  }, bg = "transparent")
  
  output$max <- renderInfoBox({
    infoBox(
        "Maximum",
        paste(
          max(data_sub()$value),
          " (",
          stri_trans_totitle(data_sub()$region[match(max(data_sub()$value), data_sub()$value)]),
          ")",
          sep = ""
      ),
      icon = icon("arrow-up"), color = "green"
    )
  })

  output$min <- renderInfoBox({
    infoBox(
      "Minimum",
      paste(
        min(data_sub()$value),
        " (",
        stri_trans_totitle(data_sub()$region[match(min(data_sub()$value), data_sub()$value)]),
        ")", sep = ""
      ),
      icon = icon("arrow-down"), color = "yellow"
    )
  })
  
  output$avg <- renderInfoBox({
    infoBox(
      "Average",
      round(mean(data_sub()$value),digits = 1),
      icon = icon("line-chart"), color = "blue"
    )
  })

}

shinyApp(ui=ui, server=server)