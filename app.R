library(teamcolors)
library(Hmisc)
library(SDMTools)
library(MASS)
library(tidyverse)
library(readxl)
library(Hmisc)
library(WMDB)
source("data/Fantasy_Prospect_Finder.R")

## Define UI----
ui <- navbarPage("Fantasy Prospect Report",
                 
                 # Application title
                 tabPanel("Top Prospects 2019",
                          h2("Top Fantasy Prospects of 2019"),
                          h4("Less Than 29 Years Of Age"),
                          DT::dataTableOutput('leaders')
                          ),
                 tabPanel("Top Player Comps",
                           selectInput("player",
                                       "Select Prospect:",
                                       sort(unique(as.character(all_players_2019$Name)))),
                           uiOutput("teamControls"),
                           uiOutput("levelControls"),
                           DT::dataTableOutput('comps')
                  ),
                 tabPanel("Probability Charts",
                           selectInput("player2",
                                       "Select Prospect:",
                                       sort(unique(as.character(all_players_2019$Name)))),
                           uiOutput("teamControls2"),
                           uiOutput("levelControls2"),
                           plotOutput('probchart')
                  )

)

# Define server logic for the tables and charts
server <- function(input, output) {
    output$leaders <- DT::renderDataTable(DT::datatable({
        data <- all_players_2019
    }, options = list(paging = TRUE, searching = TRUE),
    rownames= FALSE))
    
    output$teamControls <- renderUI({
      teams <- unique(as.character(filter(miLB,Name == input$player)$Team))
      selectInput("team",
                  "Select Team:",
                  sort(teams))
    })
    
    output$teamControls2 <- renderUI({
       teams2 <- unique(as.character(filter(miLB,Name == input$player2)$Team))
       selectInput("team2",
                   "Select Team:",
                   sort(teams2))
    })
     
    output$levelControls <- renderUI({
       level <- unique(as.character(filter(miLB,Name == input$player & Team == input$team)$Level))
       selectInput("level",
                   "Select Level:",
                   sort(level))
     })
     
    output$levelControls2 <- renderUI({
       level2 <- unique(as.character(filter(miLB,Name == input$player2 & Team == input$team2)$Level))
       selectInput("level2",
                   "Select Level:",
                   sort(level2))
     })
    
    output$comps <- DT::renderDataTable(DT::datatable({
        data <- Minors_comp(filter(miLB,Name == input$player & Team == input$team & Level == input$level)$Key)
        data <- data[,1:3]
    }, options = list(paging = TRUE, searching = TRUE),
    rownames= FALSE))
    
    output$probchart <- renderPlot({
        Minors_chart(filter(miLB,Name == input$player2 & Team == input$team2 & Level == input$level2)$Key)
    })
    


}

# Run the application 
shinyApp(ui = ui, server = server)