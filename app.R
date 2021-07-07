library(shinydashboard)
library(shiny)
library(dashboardthemes)
library(ggplot2)
library(tidyverse)
source("simVARS.R")
source("plotFunctions.R")
source("appModules.R")
options(shiny.autoreload = TRUE)

ui <- dashboardPage(
    dashboardHeader(title = "Dyadic Interactions"),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("VAR(1)", tabName = "var1", icon = icon("chart-line")),
            menuItem("TVAR(1)", tabName = "tvar1", icon = icon("chart-line"))
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
            plotsUI("plots"),
            # Second tab content
            tabItem(tabName = "tvar1",
                    h2("First-order threshold vector autoregression"),
                    fluidRow(
                      box(width = 12, collapsible = T,
                          title = "Input",
                          fluidRow(
                            column(4,
                                   h4("Method"),
                                   sliderInput("TVAR_t", "Measurement occasions", 1, 500, 250), # 1 does not work
                                   sliderInput("TVAR_burnin", "Burnin", 1, 100, 20),
                                   numericInput("TVAR_seed", "Seed", 1, 1)
                            ),
                            column(4,
                                   h4("y"),
                                   sliderInput("TVAR_k_y", "Threshold", -1, 1, 0, .1),
                                   h4("Regime 1"),
                                   sliderInput("TVAR_alpha_y", "Intercept", -1, 1, 0, .1),
                                   sliderInput("TVAR_phi_y", "Carryover", -1, 1, .5, .1),
                                   sliderInput("TVAR_beta_y", "Spillover", -1, 1, .2, .1),
                                   h4("Regime 2"),
                                   sliderInput("TVAR_alpha_y2", "Intercept", -1, 1, 0, .1),
                                   sliderInput("TVAR_phi_y2", "Carryover", -1, 1, .5, .1),
                                   sliderInput("TVAR_beta_y2", "Spillover", -1, 1, .2, .1)
                            ),
                            column(4,
                                   h4("x"),
                                   sliderInput("TVAR_k_x", "Threshold", -1, 1, 0, .1),
                                   h4("Regime 1"),
                                   sliderInput("TVAR_alpha_x", "Intercept", -1, 1, 0, .1),
                                   sliderInput("TVAR_phi_x", "Carryover", -1, 1, .5, .1),
                                   sliderInput("TVAR_beta_x", "Spillover", -1, 1, .2, .1),
                                   h4("Regime 2"),
                                   sliderInput("TVAR_alpha_x2", "Intercept", -1, 1, 0, .1),
                                   sliderInput("TVAR_phi_x2", "Carryover", -1, 1, .5, .1),
                                   sliderInput("TVAR_beta_x2", "Spillover", -1, 1, .2, .1),
                            )
                          )
                      )
                    ),
                    fluidRow(
                      box(title = "Time series", plotOutput("TVAR_ts"), width = 12)
                    ),
                    fluidRow(
                      box(title = "Carryover y state space", plotOutput("TVAR_carryover_y")),
                      box(title = "Carryover x state space", plotOutput("TVAR_carryover_x"))
                    ),
                    fluidRow(
                      box(title = "Spillover y state space", plotOutput("TVAR_spillover_y")),
                      box(title = "Spillover x state space", plotOutput("TVAR_spillover_x"))
                    ),
                    fluidRow(
                      box(title = "Cross-correlation function", plotOutput("TVAR_ccf"))
                    )

            )
        )
    )
)

server <- function(input, output) {

   plotsServer("plots")
    
    TVAR_dat <- reactive({
      dat <- simVARS(occasions = input$TVAR_t, burnin = input$TVAR_burnin,
                     type = "T",
                     params_y = list(c(input$TVAR_alpha_y, input$TVAR_alpha_y2), 
                                     c(input$TVAR_phi_y, input$TVAR_phi_y2), 
                                     c(input$TVAR_beta_y, input$TVAR_beta_y), 
                                     input$TVAR_k_y),
                     params_x = list(c(input$TVAR_alpha_x, input$TVAR_alpha_x2), 
                                     c(input$TVAR_phi_x, input$TVAR_phi_x2), 
                                     c(input$TVAR_beta_x, input$TVAR_beta_x2), 
                                     input$TVAR_k_x),
                     seed = input$seed)
      
      dat
    })

    output$TVAR_ts <- renderPlot({
      myTS(TVAR_dat())
    })
    
    output$TVAR_carryover_y <- renderPlot({
      mySSP(TVAR_dat(), type = "carryover", partner = "y")
    })
    
    output$TVAR_carryover_x <- renderPlot({
      mySSP(TVAR_dat(), type = "carryover", partner = "x")
    })
    
    output$TVAR_spillover_y <- renderPlot({
      mySSP(TVAR_dat(), type = "spillover", partner = "y")
    })
    
    output$TVAR_spillover_x <- renderPlot({
      mySSP(TVAR_dat(), type = "spillover", partner = "x")
    })
    
    output$TVAR_ccf <- renderPlot({
      myCCF(TVAR_dat())
    })

}


shinyApp(ui, server)
