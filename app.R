library(shinydashboard)
library(shiny)
library(dashboardthemes)
library(ggplot2)
library(tidyverse)
source("symVARS.R")
source("plotFunctions.R")

ui <- dashboardPage(
    dashboardHeader(title = "Dyadic Interactions"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("VAR(1)", tabName = "var1", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_light"
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "home",
                    box(includeMarkdown("home.Rmd"))
            ),
            
            # Second tab content
            tabItem(tabName = "var1",
                    h2("First-order vector autoregression"),
                    fluidRow(
                        box(width = 12, collapsible = T,
                            title = "Input",
                            fluidRow(
                                column(4,
                                       h4("Method"),
                                       sliderInput("t", "Measurement occasions", 1, 500, 250), # 1 does not work
                                       sliderInput("burnin", "Burnin", 1, 100, 20),
                                       numericInput("seed", "Seed", 1, 1)
                                 ),
                                column(4,
                                       h4("y"),
                                   sliderInput("alpha_y", "Intercept", -1, 1, 0, .1),
                                   sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                                   sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                                ),
                                column(4,
                                       h4("x"),
                                  sliderInput("alpha_x", "Intercept", -1, 1, 0, .1),
                                  sliderInput("phi_x", "Carryover", -1, 1, .5, .1),
                                  sliderInput("beta_x", "Spillover", -1, 1, .2, .1)
                                )
                            )
                        )
                    ),
                    fluidRow(
                        box(title = "Time series", plotOutput("ts"), width = 12)
                    ),
                    fluidRow(
                        box(title = "Carryover y state space", plotOutput("carryover_y")),
                        box(title = "Carryover x state space", plotOutput("carryover_x"))
                    ),
                    fluidRow(
                      box(title = "Spillover y state space", plotOutput("spillover_y")),
                      box(title = "Spillover x state space", plotOutput( "spillover_x"))
                    ),
                    fluidRow(
                      box(title = "Cross-correlation function", plotOutput("ccf"))
                    )
                    
            )
        )
    )
)

server <- function(input, output) {
    dat <- reactive({
      set.seed(input$seed)
        dat <- symVARS(occasions = input$t, burnin = input$burnin,
                       type = "VAR",
                       params_y = c(input$alpha_y, input$phi_y, input$beta_y),
                       params_x = c(input$alpha_x, input$phi_x, input$beta_x))
        
        dat
    })
    
    # 
    # # dif <- reactive({
    # #     
    # # })
    # 
    output$ts <- renderPlot({
        myTS(dat())
    })

    output$carryover_y <- renderPlot({
        mySSP(dat(), type = "carryover", partner = "y")
    })
    
    output$carryover_x <- renderPlot({
      mySSP(dat(), type = "carryover", partner = "x")
    })
    
    output$spillover_y <- renderPlot({
      mySSP(dat(), type = "spillover", partner = "y")
    })
    
    output$spillover_x <- renderPlot({
      mySSP(dat(), type = "spillover", partner = "x")
    })
    
    output$ccf <- renderPlot({
      myCCF(dat())
    })

}


shinyApp(ui, server)
