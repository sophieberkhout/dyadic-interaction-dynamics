inputUITab <- function(id){
  ns <- NS(id)
  tabItem(  
    tabName = "sim",
    h2("Simulations"),
  fluidRow(
    box(title = "Method", width = 3,
      selectInput(ns("model"), "Data Generating Model", 
                  list("VAR" = "VAR", "LVAR" = "L", "TV-VAR" = "TV", 
                       "TVAR" = "T", "HMM" = "HMM", "MSVAR" = "MS"), selected = 1),
      numericInput(ns("t"), "Measurement occasions", 300, min = 2, step = 50), # 1 does not work
      numericInput(ns("burnin"), "Burnin", 20, min = 0, step = 10),
      uiOutput(ns("test")),
      numericInput(ns("seed"), "Seed", 1, min = 1, max = .Machine$integer.max)
    ),
    uiOutput(ns("yTabs")),
    # tabBox(width = 4,
    #    tabPanel("y",
    #       numericInput(ns("alpha_y"), "Intercept", 0, width = "50%"),
    #       sliderInput(ns("phi_y"), "Carryover", -1, 1, .5, .1),
    #       sliderInput(ns("beta_y"), "Spillover", -1, 1, .2, .1)
    #    )
    #    # uiOutput(ns("yTab2"))
    #    # yInputRegime(ns("yregime"))
    # ),
    tabBox(width = 4,
       tabPanel("x",
           numericInput(ns("alpha_x"), "Intercept", 0, width = "50%"),
           sliderInput(ns("phi_x"), "Carryover", -1, 1, .5, .1),
           sliderInput(ns("beta_x"), "Spillover", -1, 1, .2, .1)
       ),
       xInputRegime(ns("xregime"))
    )
  ),
  fluidRow(
    box(title = "Time series", plotOutput(ns("ts")), width = 12)
  ))
}

# yInputRegime <- function(id){
#   ns <- NS(id)
# 
#   tabPanel("y regime 2",
#            numericInput(ns("alpha_y_2"), "Intercept", 0, width = "50%"),
#            sliderInput(ns("phi_y_2"), "Carryover", -1, 1, .5, .1),
#            sliderInput(ns("beta_y_2"), "Spillover", -1, 1, .2, .1)
#   )
#   
# }

# yServerRegime <- function(id){
#   moduleServer(id,
#                function(input, output, session){
#                  observeEvent(input$model, {
#                  # if(input$model == "T"){
#                  if(T){
#                    output$yTab2 <- renderUI({
#                        tabPanel("y regime 2",
#                               numericInput(ns("alpha_y_2"), "Intercept", 0, width = "50%"),
#                               sliderInput(ns("phi_y_2"), "Carryover", -1, 1, .5, .1),
#                               sliderInput(ns("beta_y_2"), "Spillover", -1, 1, .2, .1)
#                    )
#                    })
#                  }
#                  })
#                })
# }

xInputRegime <- function(id){
  ns <- NS(id)
  
  tabPanel("x regime 2",
           numericInput(ns("alpha_x_2"), "Intercept", 0, width = "50%"),
           sliderInput(ns("phi_x_2"), "Carryover", -1, 1, .5, .1),
           sliderInput(ns("beta_x_2"), "Spillover", -1, 1, .2, .1)
  )
}

datServer <- function(id){
  moduleServer(id,
               function(input, output, session){
                 
                 observeEvent(input$seed, {
                   if(!is.integer(input$seed)){
                     newSeed <- round(input$seed)
                     updateNumericInput(session, "seed", value = newSeed)
                   }
                 })
                 
                 observeEvent(input$burnin, {
                   if(!is.integer(input$burnin)){
                     newBurnin <- round(input$burnin)
                     updateNumericInput(session, "burnin", value = newBurnin)
                   }
                 })
                 
                 observeEvent(input$t, {
                   if(!is.integer(input$t)){
                     newT <- round(input$t)
                     updateNumericInput(session, "t", value = newT)
                   }
                 })
                 
                 output$test <- renderUI({
                   h3("test")
                 })
                 
                 # yInputRegime <- function(){
                 #   tabPanel("y",
                 #            numericInput("alpha_y", "Intercept", 0, width = "50%"),
                 #            sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                 #            sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                 #   )                 
                 # }

                 output$yTabs <- renderUI({
                   # if(input$model == "T"){
                     tabBox(width = 4,
                            tabPanel("y",
                                     numericInput("alpha_y", "Intercept", 0, width = "50%"),
                                     sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                                     sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                            ),
                            if(input$model == "T"){
                            tabPanel("y regime 2",
                                     numericInput("alpha_y_2", "Intercept", 0, width = "50%"),
                                     sliderInput("phi_y_2", "Carryover", -1, 1, .5, .1),
                                     sliderInput("beta_y_2", "Spillover", -1, 1, .2, .1)
                            )
                            }
                     )
                   # } else {
                   #   tabBox(width = 4,
                   #          tabPanel("y",
                   #                   numericInput("alpha_y", "Intercept", 0, width = "50%"),
                   #                   sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                   #                   sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                   #          )
                   #   )
                   # }
                   })

                 # observeEvent(input$model, {
                 #   # if(input$model == "T"){
                 #   if(T){
                     # output$yTab2 <- renderUI({
                     #   tabPanel("y regime 2",
                     #            numericInput(ns("alpha_y_2"), "Intercept", 0, width = "50%"),
                     #            sliderInput(ns("phi_y_2"), "Carryover", -1, 1, .5, .1),
                     #            sliderInput(ns("beta_y_2"), "Spillover", -1, 1, .2, .1)
                     #   )
                     # })
                 #   }
                 # })
                 
                 # yTabsContent <- reactiveValues(
                 #   yTab1 = list(Title = "y", Content = tagList(numericInput("alpha_y", "Intercept", 0, width = "50%"),
                 #                                                   sliderInput("phi_y", "Carryover", -1, 1, .5, .1),
                 #                                                   sliderInput("beta_y", "Spillover", -1, 1, .2, .1)
                 #   )),
                 #   yTab2 = list(Title = "y regme 2", Content = tagList(numericInput("alpha_y_2", "Intercept", 0, width = "50%"),
                 #                                                       sliderInput("phi_y_2", "Carryover", -1, 1, .5, .1),
                 #                                                       sliderInput("beta_y_2", "Spillover", -1, 1, .2, .1)
                 #   )),
                 # )
                 # 
                 # output$yTabs <- renderUI({
                 #   tabs <- lapply(1:length(yTabsContent), function(i) {
                 #     thisTab <- tabs.content[[ paste0("yTab", i) ]]
                 #     tabPanel(thisTab$Title, thisTab$Content())
                 #   })
                 #   args = c(tabs, list(id = "box", selected = input$box))
                 #   do.call(tabBox, args)
                 # })
                 
                 dat <- reactive({
                   dat <- simVARS(occasions = input$t, burnin = input$burnin,
                                  type = input$model,
                                  params_y = c(input$alpha_y, input$phi_y, input$beta_y),
                                  params_x = c(input$alpha_x, input$phi_x, input$beta_x),
                                  seed = input$seed)

                   dat
                 })
                 
                 output$ts <- renderPlot({
                   myTS(dat())
                 })
                 
               })
}


#####################
###### VAR TAB ######
#####################
# inputUI_VAR <- function(id){
#   ns <- NS(id)
#   
#   fluidRow(
#     box(width = 12, collapsible = T,
#         title = "Input",
#         fluidRow(
#           column(4,
#                  h4("Method"),
#                  sliderInput(ns("t"), "Measurement occasions", 1, 500, 250), # 1 does not work
#                  sliderInput(ns("burnin"), "Burnin", 1, 100, 20),
#                  numericInput(ns("seed"), "Seed", 1, 1)
#           ),
#           column(4,
#                  h4("y"),
#                  sliderInput(ns("alpha_y"), "Intercept", -1, 1, 0, .1),
#                  sliderInput(ns("phi_y"), "Carryover", -1, 1, .5, .1),
#                  sliderInput(ns("beta_y"), "Spillover", -1, 1, .2, .1)
#           ),
#           column(4,
#                  h4("x"),
#                  sliderInput(ns("alpha_x"), "Intercept", -1, 1, 0, .1),
#                  sliderInput(ns("phi_x"), "Carryover", -1, 1, .5, .1),
#                  sliderInput(ns("beta_x"), "Spillover", -1, 1, .2, .1)
#           )
#         )
#     )
#   )
#   
# }
# 
# plotsUI_VAR <- function(id){
#   ns <- NS(id)
#   
#   tabItem(  
#     tabName = "VAR",
#     h2("First-order vector autoregression"),
#     inputUI_VAR(ns("input_VAR")),
#     fluidRow(
#       box(title = "Time series", plotOutput(ns("ts")), width = 12)
#     ),
#     fluidRow(
#       box(title = "Carryover y state space", plotOutput(ns("carryover_y"))),
#       box(title = "Carryover x state space", plotOutput(ns("carryover_x")))
#     ),
#     fluidRow(
#       box(title = "Spillover y state space", plotOutput(ns("spillover_y"))),
#       box(title = "Spillover x state space", plotOutput(ns("spillover_x")))
#     ),
#     fluidRow(
#     box(textOutput("text")),
#       box(title = "Cross-correlation function", plotOutput(ns("ccf")))
#     )
#     
#   )
# }
# 
# inputServer <- function(id){
#   moduleServer(id,
#                function(input, output, session){
#                  dat <- reactive({
#                       dat <- simVARS(occasions = input$t, burnin = input$burnin,
#                       type = "VAR",
#                       params_y = c(input$alpha_y, input$phi_y, input$beta_y),
#                       params_x = c(input$alpha_x, input$phi_x, input$beta_x),
#                       seed = input$seed)
#                    
#                       dat
#                   })
#                })
# }
# 
# plotsServer <- function(id){
#   moduleServer(id, function(input, output, session){
#     dat <- inputServer("input_VAR")
#     
#     observeEvent(input$tabs, {
#       output$text <- renderText(input$tabs)
#     })
#     
#     output$ts <- renderPlot({
#       myTS(dat())
#     })
#     
#     output$carryover_y <- renderPlot({
#       mySSP(dat(), type = "carryover", partner = "y")
#     })
#     
#     output$carryover_x <- renderPlot({
#       mySSP(dat(), type = "carryover", partner = "x")
#     })
#     
#     output$spillover_y <- renderPlot({
#       mySSP(dat(), type = "spillover", partner = "y")
#     })
#     
#     output$spillover_x <- renderPlot({
#       mySSP(dat(), type = "spillover", partner = "x")
#     })
#     
#     output$ccf <- renderPlot({
#       myCCF(dat())
#     })
#   }
#   )
# }
