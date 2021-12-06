myTheme <- function(p, x = NULL, y = NULL, legend.position = NULL, shiny = F, cols = c("#FE9F6DFF", "#8C2981FF")){
  if(!shiny & is.null(legend.position)) legend.position <- c(.15, .9)
  p <- p + 
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = "black", size = .75),
          axis.ticks = element_line(colour = "black", size = .75),
          axis.ticks.length = unit(.25, "cm"),
          panel.background = element_blank(),
          text = element_text(size = 14, family = "serif"),
          axis.text = element_text(size = 12),
          legend.position = legend.position,
          legend.key = element_rect(fill = "transparent", size = 2),
          legend.background = element_rect(fill = "transparent"),
          legend.text = element_text(size = 12),
          # legend.text = element_text(size = 12),
          legend.title = element_blank()
  )
  
  if(!is.null(x) && !is.null(y)){
      x_breaks <- pretty(x)
      x_min    <- min(x_breaks)
      x_max    <- max(x_breaks)
      y_breaks <- pretty(y)
      y_min    <- min(y_breaks)
      y_max    <- max(y_breaks)

      p <- p + 
        theme(axis.line = element_blank()) + 
        scale_x_continuous(limits = c(x_min, x_max), breaks = x_breaks) +
        scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks) +
        geom_segment(x = x_min, xend = x_max, y = -Inf, yend = -Inf, size = .75, colour = "black", lineend = "square") +
        geom_segment(x = -Inf, xend = -Inf, y = y_min, yend = y_max, size = .75, colour = "black", lineend = "square")
  }
  
  if(shiny){
    if(is.null(legend.position)) legend.position <- c(.1, .9)
    legendLabels <- p$scales$scales[[1]]$labels
    p <- p + scale_color_manual(values = cols,
                                labels = legendLabels)

    p <- p + theme(text = element_text(size = 20, family = "serif"),
                   axis.text = element_text(size = 18),
                   legend.text = element_text(size = 18),
                   legend.position = legend.position)
  }
  
  return(p)
}

