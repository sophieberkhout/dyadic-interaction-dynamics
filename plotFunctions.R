library("ggplot2")
library("plotly")
source("myTheme.R")

myTS <- function(dat, partner = NULL, regime = F, regimeType = NULL,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL, cols = NULL, shiny = F, legend.position = NULL){
  
  if(is.null(cols)) pCols <- c("grey", "black") else pCols <- cols
  shinyCols <- viridis::viridis(2, begin = .4, end = .8, option = "A")
  
  if(is.null(partner)){
    p <- ggplot(dat, aes(x = t, y = behavior, color = as.factor(partner))) + 
    geom_line(size = 1) +
    labs(x = expression(italic("t")), y = "Measurement") + # measure
    scale_color_manual(values = pCols)
  } else {
    dat <- dat[dat$partner == partner, ]
    
    if(shiny){
      if(partner == "y") pCols <- shinyCols[1]
      if(partner == "x") pCols <- shinyCols[2]
    } else { pCols <- "black" }    
    
    p <- ggplot(dat, aes(x = t, y = behavior)) + 
      labs(x = bquote(italic("t")), y = bquote(italic(.(partner)))) +
      geom_line(col = pCols)
  }
  
  if(regime){
    if(is.null(partner)){
      partners <- unique(dat$partner)
      
      regy <- dat[dat$partner == partners[1], c("t", "regime")]
      regx <- dat[dat$partner == partners[2], c("t", "regime")]
      
      fill <- rep("Both in 1", nrow(regy)) # make NA regimes white
      fill[regy$regime == 2 & regx$regime == 1] <- "y in 2, x in 1"
      fill[regx$regime == 2 & regy$regime == 1] <- "x in 2, y in 1"
      fill[regy$regime == 2 & regx$regime == 2] <- "Both in 2"
      fill[regy$regime == 1 & regx$regime == 1] <- "Both in 1"
      
      colShades <- c("white", "black", shinyCols[2], shinyCols[1])
      
      dfShades <- data.frame(tleft = dat$t - 0.5, tright = dat$t + 0.5, g = fill)
      dat <- cbind(dat, dfShades)
      shades <- geom_rect(aes(xmin = tleft, xmax = tright, fill = g), 
                          ymin = -Inf, ymax = Inf, data = dat, color = NA, alpha = 0.05)
      
      if(!is.null(regimeType)){
        if(regimeType == "shades"){
          p$layers <- c(shades, p$layers)
          p <- p + scale_fill_manual(values = colShades)        
        }        
      } else {
        sameRegime <- all(regy$regime == regx$regime)
        if(!sameRegime){
          p <- p + 
            annotate("point", x = regy$t[regy$regime == 2], y = 0.1, colour = pCols[1]) +
            annotate("point", x = regx$t[regx$regime == 2], y = 0, colour = pCols[2])
        } else {
          p <- p + 
            annotate("point", x = regy$t[regy$regime == 2], y = 0.1, colour = pCols[2])            
          }
      }
    } else {
      dat2 <- dat[dat$regime == 2, ]
      shades <- annotate("rect", xmin = dat2$t - 0.5, xmax = dat2$t + 0.49,
                         ymin = -Inf, ymax = Inf, fill = "grey95")
      p$layers <- c(shades, p$layers)    
    }
  }
  
  if(is.null(xlim)) xlim <- dat$t
  if(is.null(ylim)) ylim <- dat$behavior
  
  p <- myTheme(p, x = xlim, y = ylim, shiny = shiny, legend.position = legend.position)
  
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

mySSP <- function(dat, type, tau, partner = NULL,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL, shiny = F){
  t <- max(dat$t)
  dat$lag1 <- c(NA, dat$behavior[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  dat$spillover <- c(dat$lag1[(t+1):(t*2)], dat$lag1[1:t])
  
  legend.position <- NULL
  
  if(is.null(partner)){
    if(type == "carryover"){
      p <-  ggplot(dat, aes(x = lag1, y = behavior, color = partner)) +
        geom_point(size = 2) + 
        scale_color_manual(values = c("grey", "black")) +
        labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
        geom_smooth(method = "lm", se = F, fullrange = T)
    }
    if(type == "spillover"){
      p <- ggplot(dat, aes(x = spillover, y = behavior, color = partner)) + 
        geom_point(size = 2) +
        scale_color_manual(values = c("grey", "black"), 
                           labels = c(expression(paste("x"[t], " vs ", "y"[t-1])), expression(paste("y"[t], " vs ", "x"[t-1])))) +
        labs(x = expression(italic(t-1)), y = expression(italic(t)), color = "legend") +
        geom_smooth(method = "lm", se = F, fullrange = T) 
    }
  } else {
    dat <- dat[dat$partner == partner, ]
    other <- ifelse(partner == "y", "x", "y")
    
    if(shiny){
      pCols <- viridis::viridis(2, begin = .4, end = .8, option = "A")
      if(partner == "y") pCols <- pCols[1]
      if(partner == "x") pCols <- pCols[2]
    } else { pCols <- "black" }
    
    
    if(is.null(xlim)) xlim <- dat$spillover
    if(is.null(ylim)) ylim <- dat$behavior
    
    if(type == "carryover"){
      p <-  ggplot(dat, aes(x = lag1, y = behavior)) +
        geom_point(size = 2, col = pCols) + 
        labs(x = bquote(italic(.(partner)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = pCols)
    }
    if(type == "spillover"){
      p <- ggplot(dat, aes(x = spillover, y = behavior)) + 
        geom_point(size = 2, col = pCols) +
        labs(x = bquote(italic(.(other)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = pCols) 
    }
    if(type == "spillover_threshold"){
      p <- ggplot(dat, aes(x = spillover, y = behavior, colour = regime)) + geom_point(size = 2) +
        scale_color_manual(values = c("gray50", "gray75")) +
        labs(x = bquote(italic(.(other)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(data = dat[dat$regime == 1, ], method = "lm", se = F, 
                    xseq = seq(min(dat$spillover, na.rm = T), tau, 0.01)) +
        geom_smooth(data = dat[dat$regime == 2, ], method = "lm", se = F, 
                    xseq = seq(tau, max(dat$spillover, na.rm = T), 0.01)) +
        geom_vline(xintercept = tau)
      legend.position <- "none"
    }
  }
  
  if(is.null(xlim)) xlim <- dat$behavior
  if(is.null(ylim)) ylim <- dat$behavior
  
  p <- myTheme(p, x = xlim, y = ylim, legend.position = legend.position, shiny)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myCCF <- function(dat, 
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL, shiny = F){
  cc <- ccf(subset(dat, partner == "y", select = "behavior"), 
            subset(dat, partner == "x", select = "behavior"))
  cc <- data.frame(lag = cc$lag, ccf = cc$acf)
  # cc <- cc[which(cc$lag == -10):which(cc$lag == 10), ]
  p <- ggplot(cc, aes(x = lag, y = ccf)) + 
    geom_linerange(dat = cc, aes(ymin = 0, ymax = ccf)) +
    labs(x = "Lag", y = expression(paste("CCF (", italic("y * x"), ")")))
  if(is.null(xlim)) xlim <- cc$lag
  if(is.null(ylim)) ylim <- cc$ccf
  p <- myTheme(p, x = xlim, y = c(ylim, 0), shiny = shiny)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myCF <- function(dat, type, partner = NULL,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL, shiny = F){
  ptrue <- !is.null(partner)
  if(ptrue) ifelse(partner == "y", other <- "x", other <- "y")
  
  if(type == "ACF"){
    ccy <- acf(subset(dat, partner == "y", select = "behavior"))
    ccx <- acf(subset(dat, partner == "x", select = "behavior"))
    yLabs <- "ACF"
    if(ptrue){
      if(partner == "y"){
        cc <- ccy
        yLabs <- expression(paste("ACF ", italic("y")))
      } else {
        cc <- ccx
        yLabs <- expression(paste("ACF ", italic("x")))
      }
    }
  }
  
  if(type == "CCF"){
    ccy <- ccf(subset(dat, partner == "y", select = "behavior"), 
               subset(dat, partner == "x", select = "behavior"))
    ccx <- ccf(subset(dat, partner == "x", select = "behavior"), 
               subset(dat, partner == "y", select = "behavior"))
    yLabs <- "CCF"
    if(ptrue){
      if(partner == "y"){
        cc <- ccy
        yLabs <- expression(paste("ACF ", italic("y")))
      } else {
        cc <- ccx
        yLabs <- expression(paste("ACF ", italic("x")))
      }
    ifelse(partner == "y",
           yLabs <- expression(paste("CCF (", italic("y * x"), ")")),
           yLabs <- expression(paste("CCF (", italic("x * y"), ")")))
    }
    # cc <- cc[which(cc$lag == -10):which(cc$lag == 10), ]
  }
  
  if(!ptrue){
    cc <- rbind(data.frame(lag = ccx$lag, cf = ccx$acf), data.frame(lag = ccy$lag, cf = ccy$acf))
    cc$partner <- rep(c("x", "y"), each = nrow(cc)/2)
    
    p <- ggplot(cc, aes(x = lag, y = cf, color = partner)) +
      geom_linerange(dat = cc, aes(ymin = 0, ymax = cf), size = 1.2, alpha = .8, position = position_dodge2(.2))
  } else {
    cc <- data.frame(lag = cc$lag, cf = cc$acf)
    p <- ggplot(cc, aes(x = lag, y = cf)) + 
      geom_linerange(dat = cc, aes(ymin = 0, ymax = cf))
  }
  
  p <- p + labs(x = "Lag", y = yLabs)
  if(is.null(xlim)) xlim <- cc$lag
  if(is.null(ylim)) ylim <- cc$cf
  p <- myTheme(p, x = xlim, y = c(ylim, 0), shiny = shiny)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myInf <- function(dat, partner, tau,
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL){
  
  dat <- dat[dat$partner == partner, ]
  other <- ifelse(partner == "y", "x", "y")
  
  if(is.null(xlim)) xlim <- dat$behavior
  if(is.null(ylim)) ylim <- dat$influence
  
  p <- ggplot(dat, aes(x = behavior, y = influence)) + geom_point(size = 2) +
    geom_vline(xintercept = tau) + geom_hline(yintercept = 0) + 
    labs(x = bquote(italic(.(partner)[t-1])), y = bquote(italic(paste(beta[.(other)], .(partner)[t-1]))))
  p <- myTheme(p, x = xlim, y = ylim)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myTSsimple <- function(t, y, xlab = NULL, ylab = NULL,
                       filename = NULL, width = 5, height = 3, shiny = F){
  xlim <- t
  ylim <- y
  if(is.null(xlab)) xlab <- expression(italic("t"))
  if(is.null(ylab)) ylab <- expression(italic("y"))
  dat <- data.frame(x = t, y = y)
  p <- ggplot(dat, aes(x = x, y = y)) +
    geom_line() +
    labs(x = xlab, y = ylab)
  p <- myTheme(p, x = xlim, y = ylim, shiny = shiny)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

my3D <- function(dat, partner = NULL){
  t <- max(dat$t)
  dat$lag1 <- c(NA, dat$behavior[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  pCols <- viridis::viridis(2, begin = .4, end = .8, option = "A")
  if(!is.null(partner)){
    ifelse(partner == "y", pCols <- pCols[1], pCols <- pCols[2])
  }
  
  f <- list(family = "serif",
            size = 14)
  
  if(is.null(partner)){
    p <- plot_ly(dat, x = ~t, y = ~lag1, z = ~behavior, color = dat$partner, colors = pCols,
                 type = "scatter3d", mode = "lines+markers",
                 line = list(width = 4),
                 marker = list(size = 4))
    partner <- "y/x"
  } else {
    dat <- dat[dat$partner == partner, ]
    p <- plot_ly(dat, x = ~t, y = ~lag1, z = ~behavior,
                 type = "scatter3d", mode = "lines+markers",
                 line = list(width = 4, color = pCols),
                 marker = list(size = 4, color = pCols))
  }
  
  p <- layout(p, font = f, 
              scene = list(yaxis = list(title = paste0("<i>", partner, "<sub>t-1</sub></i>"),
                                        showgrid = F, showbackground = F, automargin = T),
                           xaxis = list(title = "<i>t</i>",
                                        showgrid = F, showbackground = F, automargin = T, autorange = "reversed"),
                           zaxis = list(title = paste0("<i>", partner, "<sub>t</sub></i>"),
                                        showgrid = F, showbackground = F, automargin = T),
                           aspectmode = "manual",
                           aspectratio = list(x = 1.7, y = 0.85, z = 0.85),
                           camera = list(eye = list(x = 1.5, y = 1.5, z = .1))))
  p
}

myTVpars <- function(tvFit, partner = NULL){
  # option to suppress plot
  plt <- oddsratio::no_plot(tvFit)
  # plt <- plot(tvFit)
  
  tvpars <- data.frame(x = c(plt[[1]]$x, plt[[2]]$x, plt[[3]]$x), 
                       y = c(plt[[1]]$fit, plt[[2]]$fit, plt[[3]]$fit), 
                       se = c(plt[[1]]$se, plt[[2]]$se, plt[[3]]$se),
                       p = rep(1:3, each = length(plt[[1]]$x)))
  
  labels <- c(bquote(alpha[.(partner)]), bquote(phi[.(partner)]), bquote(beta[.(partner)]))
  
  p <- ggplot(tvpars, aes(x = x, y = y, fill = as.factor(p), color = as.factor(p))) +
    geom_line() +
    geom_ribbon(aes(ymin = y - se,
                    ymax = y + se,
                    fill = as.factor(p),
                    color = as.factor(p)),
                alpha = .5) +
    scale_color_manual(values = c("grey40", "grey80", "black"), labels = labels) +
    scale_fill_manual(values = c("grey40", "grey80", "black"), labels = labels) +
    labs(x = expression(italic("t")), y = "Parameter Value")
  
  
  p <- myTheme(p, x = tvpars$x, y = c(tvpars$y + tvpars$se, tvpars$y - tvpars$se))
  return(p)
}
