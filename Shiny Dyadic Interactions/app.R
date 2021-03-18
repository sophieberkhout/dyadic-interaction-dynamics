library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(tidyverse)
source("modelfunctions.R")
source("myTheme.R")

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
                            title = "Controls",
                            fluidRow(
                                column(4,
                                  sliderInput("t", "Number of time points:", 2, 100, 50)
                                ),
                                column(4,
                                   sliderInput("u_w", "Intercept Wife", -1, 1, 0, .1),
                                   sliderInput("phi_w", "Autoregression Wife", -1, 1, .5, .1),
                                   sliderInput("phi_hw", "Influence Husband", -1, 1, .2, .1)
                                ),
                                column(4,
                                       sliderInput("u_h", "Intercept Husband", -1, 1, 0, .1),
                                       sliderInput("phi_h", "Autoregression Husband", -1, 1, .5, .1),
                                       sliderInput("phi_wh", "Influence Wife", -1, 1, .2, .1)
                                )
                            )
                        )
                    ),
                    fluidRow(
                        box(plotOutput("plot")),
                        box(plotOutput("plot2"))
                    ),
                    fluidRow(
                        box(plotOutput("plot3")),
                        box(plotOutput("plot4"))
                    ),
                    fluidRow(
                        box(plotOutput("plot6")),
                        box(plotOutput("plot7"))
                    ),
                    fluidRow(
                        box(plotOutput("plot5"), width = 12)
                    ),
                    fluidRow(
                        box(plotOutput("plot8")),
                        box(plotOutput("plot9"))
                    )
            )
        )
    )
)

server <- function(input, output) {
    dat <- reactive({
        dat <- VAR1(t = input$t, 
                    u_w = input$u_w, u_h = input$u_h,
                    phi_w = input$phi_w, phi_h = input$phi_h,
                    phi_wh = input$phi_wh, phi_hw = input$phi_hw)
        
        dat$lagW <- c(dat$W[-1], NA)
        dat$lagH <- c(dat$H[-1], NA)
        dat$difW <- dat$W - dat$lagW
        dat$difH <- dat$H - dat$lagH
        dat
    })
    
    
    # dif <- reactive({
    #     
    # })
    
    output$plot <- renderPlot({
        x <- rep(1:input$t, 2)
        dat <- gather(dat(), "G", "Y", W, H)
        p <- ggplot(dat, aes(x = x, y = Y, colour = G)) + geom_line(size = 1)
        myTheme(p)
    })
    
    output$plot2 <- renderPlot({
        p <- ggplot(dat(), aes(x=W, y=H) ) +
                stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0))
        p <- myTheme(p)
        p <- p + theme(legend.position='none')
        p
    })
    
    output$plot3 <- renderPlot({
        p <- ggplot(dat(), aes(x=W, y=lagW)) + geom_point(shape = 21, size = 3) +
            geom_smooth(method = "lm", se = F)
        myTheme(p)
    })
    
    output$plot4 <- renderPlot({
        p <- ggplot(dat(), aes(x=H, y=lagH)) + geom_point(shape = 21, size = 3) +
            geom_smooth(method = "lm", se = F)
        myTheme(p)
    })
    
    output$plot6 <- renderPlot({
        dat <- dat()
        dat$I_hw <- dat$H * input$phi_hw
        p <- ggplot(dat, aes(x = H, y = I_hw)) + geom_line(size = 1)
        myTheme(p)
    })
    
    output$plot7 <- renderPlot({
        dat <- dat()
        dat$I_wh <- dat$W * input$phi_wh
        p <- ggplot(dat, aes(x = W, y = I_wh)) + geom_line(size = 1)
        myTheme(p)
    })
    
    output$plot5 <- renderPlot({
        dat <- gather(dat(), "G", "dif", difW, difH)
        x <- rep(1:input$t, 2)
        p <- ggplot(dat, aes(x, dif, colour = G)) + 
                geom_segment(aes(xend = x, y = 0, yend = dif), alpha = 0.8, size = 1,
                             na.rm = TRUE, arrow = arrow(length = unit(0.2, "cm")), ) +
                theme(legend.title = element_blank(), legend.position = "bottom") +
                geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0)
        myTheme(p)
    })
    
    output$plot8 <- renderPlot({
        acf(dat()$W)
    })
    output$plot9 <- renderPlot({
        acf(dat()$H)
    })
}


shinyApp(ui, server)