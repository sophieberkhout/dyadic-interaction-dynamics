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
            menuItem("Simulation", tabName = "sim", icon = icon("chart-line")),
            # menuItem("VAR(1)", tabName = "VAR", icon = icon("chart-line")),
            menuItem("TVAR(1)", tabName = "T", icon = icon("chart-line")),
            menuItem("Data", tabName = "dat", icon = icon("table"))
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_light"
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "home",
                    # box(textOutput("text")),
                    box(includeMarkdown("home.Rmd"))
            ),
            inputUITab("test"),
            # plotsUI_VAR("plots"),
            # Second tab content
            tabItem(tabName = "T",
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

            ),
            tabItem(tabName = "dat",
                      fluidRow(
                        column(12,
                          title = "Data",
                          DT::dataTableOutput("table"))
                      )
            )
        )
    )
)

server <- function(input, output, session) {

  # plotsServer("plots")
  
  datServer("test")
  
  # observeEvent(input$tabs, {
  #   output$text <- renderText(input$tabs)
  # })
  
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
    
    TVAR_dat_wide <- reactive({
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
                     seed = input$seed, longformat = F)
      
      dat
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(TVAR_dat_wide()[, c("t", "x", "y")], rownames = F) %>%
        DT::formatRound(columns = c("x", "y"), digits = 3)
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
