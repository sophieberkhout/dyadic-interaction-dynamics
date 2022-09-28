library("ggplot2")
library("plotly")
source("myTheme.R")

myTS <- function(dat, partner = NULL, regime = F, regimeType = NULL, predicted = F,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL, cols = NULL, shiny = F, 
                 legend.position = NULL, textSize = c(14, 12)){
  
  if(is.null(xlim)) xlim <- dat$t
  if(is.null(ylim)) ylim <- dat$value
  
  if(regime){
    # x_breaks <- pretty(xlim)
    y_breaks <- pretty(ylim)
    bottom <- min(as.numeric(y_breaks))
    # bottom <- min(as.numeric(c(x_breaks, y_breaks)))
    dat$secLab <- ifelse(dat$partner == "y", "2nd regime y", "2nd regime x")
    dat$segY <- ifelse(dat$partner == "y", bottom, bottom + 0.1)
  }
  
  if(is.null(cols)) pCols <- c("grey", "black") else pCols <- cols
  shinyCols <- viridis::viridis(2, begin = .8, end = .4, option = "A")
  
  if(is.null(partner)){
    if(shiny) pCols <- shinyCols
    # p <- ggplot(dat, aes(x = t, y = value, color = partner)) + 
    #   geom_line(size = 1) +
    p <- ggplot(dat) + 
      geom_line(aes(x = t, y = value, color = partner), size = 1) +
      labs(x = expression(italic("t")), y = "Value") + # measure
      scale_color_manual(values = pCols)
  } else {
    dat <- dat[dat$partner == partner, ]
    
    if(shiny){
      if(partner == "y") pCols <- shinyCols[2]
      if(partner == "x") pCols <- shinyCols[1]
    } else { pCols <- "black" }    
    
    p <- ggplot(dat, aes(x = t, y = value)) + 
      labs(x = bquote(italic("t")), y = bquote(italic(.(partner)))) +
      geom_line(col = pCols, size = 1)
  }
  
  if(regime){
    if(is.null(partner)){
      if(!is.null(regimeType)){
        if(regimeType == "shades"){
          partners <- unique(dat$partner)
          regy <- dat[dat$partner == partners[1], c("t", "regime")]
          regx <- dat[dat$partner == partners[2], c("t", "regime")]
          fill <- rep("Both in 1", nrow(regy)) # make NA regimes white
          fill[regy$regime == 2 & regx$regime == 1] <- "y in 2, x in 1"
          fill[regx$regime == 2 & regy$regime == 1] <- "x in 2, y in 1"
          fill[regy$regime == 2 & regx$regime == 2] <- "Both in 2"
          fill[regy$regime == 1 & regx$regime == 1] <- "Both in 1"

          colShades <- c("white", "black", shinyCols[1], shinyCols[2])

          dfShades <- data.frame(tleft = dat$t - 0.5, tright = dat$t + 0.5, g = fill)
          dat <- cbind(dat, dfShades)
          shades <- geom_rect(aes(xmin = tleft, xmax = tright, fill = g),
                              ymin = -Inf, ymax = Inf, data = dat, color = NA, alpha = 0.05)

          p$layers <- c(shades, p$layers)
          p <- p + scale_fill_manual(values = colShades)
        } else {
          secReg <- which(dat$regime == 2)
          dat$seg <- NA
          dat$seg[secReg] <- dat$t[secReg]
          sameRegime <- all(dat$regime[dat$partner == "y"] == dat$regime[dat$partner == "x"])
          if(!sameRegime){
            pCols <- rep(pCols, 2)
            p <- p + 
              geom_point(aes(x = seg, y = segY, color = secLab), data = dat) +
              scale_color_manual(values = pCols) +
              guides(color = guide_legend(override.aes = list(shape = c(19, 19, NA, NA),
                                                              linetype = c(0, 0, 1, 1)),
                                          ncol = 2))
          } else {
            pCols <- c("black", pCols)
            p <- p + 
              geom_point(aes(x = seg, y = bottom, color = "2nd regime"), data = dat) +
              scale_color_manual(values = pCols) +
              guides(color = guide_legend(override.aes = list(shape = c(19, NA, NA),
                                                              linetype = c(0, 1, 1))))
          }
        }
      }
    } else {
      dat2 <- dat[dat$regime == 2, ]
      shades <- annotate("rect", xmin = dat2$t - 0.5, xmax = dat2$t + 0.49,
                         ymin = -Inf, ymax = Inf, fill = "grey95")
      p$layers <- c(shades, p$layers)
      p <- p + geom_line(aes(alpha = "2nd regime")) +
        guides(alpha = guide_legend(override.aes = list(size = 5, fill = "grey95")))
    }
  }
  if(predicted){
    if(!regime) pCols <- rep(pCols, 2)
    
    ncol <- 2
    
    datPred <- data.frame(x = dat$t, y = dat$predicted, 
                          color = rep(c("predicted x","predicted y"), each = nrow(dat)/2))
    # alpha <- rep(c("predicted x", "predicted y"), nrow(dat)/2)
    shape <- c(17, 17, NA, NA)
    linetype <- c(0, 0, 1, 1)
    if(regime) {
      if(sameRegime) pCols <- c(pCols, pCols[2:3])
      shape <- c(19, shape)
      linetype <- c(0, linetype)
      if(!sameRegime) {
        ncol <- 3
        pCols <- c(pCols[1:2], pCols)
        shape <- c(19, shape)
        linetype <- c(0, linetype)
      }
    }
    p <- p + geom_point(aes(x = x, y = y, color = color), data = datPred, pch = 17) +
      scale_color_manual(values = pCols) +
      guides(color = guide_legend(override.aes = list(shape = shape,
                                                      linetype = linetype),
                                  ncol = ncol))
  }
  p <- myTheme(p, x = xlim, y = ylim, shiny = shiny, legend.position = legend.position, 
               textSize = textSize, cols = pCols)
  
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

mySSP <- function(dat, type, tau, partner = NULL,
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL, shiny = F, cor = F,
                  legend.position = NULL, textSize = c(14, 12)){
  
  t <- nrow(dat)/2
  dat$lag1 <- c(NA, dat$value[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  dat$spillover <- c(dat$lag1[(t+1):(t*2)], dat$lag1[1:t])
  
  # legend.position <- NULL
  corxx <- round(cor(subset(dat, partner == "x", "value"), 
                     subset(dat, partner == "x", "lag1"),  
                     use = "complete.obs"), 3)
  coryy <- round(cor(subset(dat, partner == "y", "value"), 
                     subset(dat, partner == "y", "lag1"),  
                     use = "complete.obs"), 3)
  
  corxy <- round(cor(subset(dat, partner == "x", "value"), 
                     subset(dat, partner == "x", "spillover"),  
                     use = "complete.obs"), 3)
  coryx <- round(cor(subset(dat, partner == "y", "value"), 
                     subset(dat, partner == "y", "spillover"),  
                     use = "complete.obs"), 3)
  
  if(is.null(partner)){
    if(type == "carryover"){
      if(cor) {
        labs <- c(bquote(paste(italic("x"), 
                               "," ~italic("r")~"="~.(corxx))),
                  bquote(paste(italic("y"), 
                               "," ~italic("r")~"="~.(coryy))))
      } else {
        labs <- c("x", "y")
      }

      p <-  ggplot(dat, aes(x = lag1, y = value, color = partner)) +
        geom_point(size = 2) + 
        scale_color_manual(values = c("grey", "black"), labels = labs) +
        labs(x = bquote(italic("t")*"-1"), y = expression(italic("t"))) +
        geom_smooth(method = "lm", se = F, fullrange = T)
    }
    if(type == "spillover"){
      if(cor) {
      labs <- c(bquote(paste(italic("x"[t]) ~"vs" ~italic("y"[t])[-1], 
                             "," ~italic("r")~"="~.(corxy))),
                bquote(paste(italic("y"[t]) ~"vs" ~italic("x"[t])[-1], 
                             "," ~italic("r")~"="~.(coryx))))
      } else {
        labs <- c(expression(paste("x"[t], " vs ", "y"[t-1])), 
                  expression(paste("y"[t], " vs ", "x"[t-1])))
      }
      p <- ggplot(dat, aes(x = spillover, y = value, color = partner)) + 
        geom_point(size = 2) +
        scale_color_manual(values = c("grey", "black"), 
                           labels = labs) +
        labs(x = bquote(italic(t)*"-1"), y = expression(italic(t)), color = "legend") +
        geom_smooth(method = "lm", se = F, fullrange = T)
    }
  } else {
    dat <- dat[dat$partner == partner, ]
    other <- ifelse(partner == "y", "x", "y")
    corDat <- data.frame(x = -Inf, y = Inf, hjust = -0.25, vjust = 2,
                         corAuto = ifelse(partner == "y", coryy, corxx),
                         corCross = ifelse(partner == "y", coryx, corxy))

    if(shiny){
      pCols <- viridis::viridis(2, begin = .8, end = .4, option = "A")
      if(partner == "y") pCols <- pCols[2]
      if(partner == "x") pCols <- pCols[1]
      rSize <- 14 * 0.352777778
    } else { 
      pCols <- "black"
      rSize <- 12 * 0.352777778
    }
    
    
    if(is.null(xlim)) xlim <- dat$spillover
    if(is.null(ylim)) ylim <- dat$value
    
    if(type == "carryover"){
      p <-  ggplot(dat, aes(x = lag1, y = value)) +
        geom_point(size = 2, col = pCols) + 
        labs(x = bquote(italic(.(partner)["t"])[-1]), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = pCols)
      if(cor){
        p <- p + geom_text(data = corDat, size = rSize, parse = T,
                           aes(x = x, y = y, hjust = hjust, vjust = vjust,
                               label = list(bquote(paste(italic("r")~"="~.(corAuto))))))
      }
    }
    if(type == "spillover"){
      corPartner <- ifelse(partner == "y", coryx, corxy)
      
      p <- ggplot(dat, aes(x = spillover, y = value)) + 
        geom_point(size = 2, col = pCols) +
        labs(x = bquote(italic(.(other)["t"])[-1]), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = pCols)
      if(cor){
        p <- p + geom_text(data = corDat, size = rSize, parse = T, 
                           aes(x = x, y = y, hjust = hjust, vjust = vjust,
                               label = list(bquote(paste(italic("r")~"="~.(corCross))))))
      }
    }
    if(type == "spillover_threshold"){
      corxy1 <- round(cor(subset(dat, regime == 1, "value"), 
                          subset(dat, regime == 1, "spillover"),  
                          use = "complete.obs"), 3)
      corxy2 <- round(cor(subset(dat, regime == 2, "value"), 
                          subset(dat, regime == 2, "spillover"),  
                          use = "complete.obs"), 3)

      corDat <- data.frame(x = c(tau, tau), y = c(Inf, Inf), hjust = c(1.25, -0.25), vjust = c(1.5, 1.5),
                           corCross = c(corxy1, corxy2))
      
      p <- ggplot(dat, aes(x = spillover, y = value)) + geom_point(size = 2, col = pCols) +
        # scale_color_manual(values = c("black", "gray75")) +
        labs(x = bquote(italic(.(other)["t"])[-1]), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(data = dat[dat$regime == 1, ], method = "lm", se = F, 
                    xseq = seq(min(dat$spillover, na.rm = T), tau, 0.01), 
                    color = pCols) +
        geom_smooth(data = dat[dat$regime == 2, ], method = "lm", se = F, 
                    xseq = seq(tau, max(dat$spillover, na.rm = T), 0.01),
                    color = pCols) +
        geom_vline(xintercept = tau)
      if(cor){
        p <- p + geom_text(data = corDat, size = rSize, parse = T, 
                           aes(x = x, y = y, hjust = hjust, vjust = vjust,
                               label = list(bquote(paste(italic("r")~"="~.(corCross[1]))),
                                            bquote(paste(italic("r")~"="~.(corCross[2]))))))
      }
      legend.position <- "none"
    }
  }
  
  if(is.null(xlim)) xlim <- dat$value
  if(is.null(ylim)) ylim <- dat$value
  
  p <- myTheme(p, x = xlim, y = ylim, legend.position = legend.position, textSize = textSize, shiny = shiny)
  
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height, device = "pdf")
  return(p)
}

myCCF <- function(dat, 
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL, shiny = F){
  cc <- ccf(subset(dat, partner == "y", select = "value"), 
            subset(dat, partner == "x", select = "value"))
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
                 xlim = NULL, ylim = NULL, shiny = F, legend.position = NULL){
  # dat$partner <- ifelse(dat$partner == dat$partner[1], "x", "y")
  ptrue <- !is.null(partner)
  if(ptrue) ifelse(partner == "y", other <- "x", other <- "y")
  
  datY <- subset(dat, partner == "y", select = "value")
  datX <- subset(dat, partner == "x", select = "value")
  
  if(type == "ACF"){
    ccy <- acf(datY, plot = F, na.action = na.pass)
    ccx <- acf(datX, plot = F, na.action = na.pass)
    # remove lag zero since that is always one
    ccy$lag <- ccy$lag[-1]
    ccy$acf <- ccy$acf[-1]
    ccx$lag <- ccx$lag[-1]
    ccx$acf <- ccx$acf[-1]
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
    ccy <- ccf(datY, datX, plot = F, na.action = na.pass)
    ccx <- ccf(datX, datY, plot = F, na.action = na.pass)
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
    cc <- rbind(
      data.frame(lag = ccx$lag, cf = ccx$acf),
      data.frame(lag = ccy$lag, cf = ccy$acf)
    )
    cc$partner <- rep(c("x", "y"), each = nrow(cc)/2)
    
    p <- ggplot(cc, aes(x = lag, y = cf, color = partner)) +
      geom_linerange(
        dat = cc, aes(ymin = 0, ymax = cf),
        size = 1.2, alpha = .8, position = position_dodge2(.2)
      )
  } else {
    cc <- data.frame(lag = cc$lag, cf = cc$acf)
    pCols <- "black"
    if(shiny) {
      pCols <- viridis::viridis(2, begin = .8, end = .4, option = "A")
      if(partner == "y") pCols <- pCols[2]
      if(partner == "x") pCols <- pCols[1]
    }    
    
    p <- ggplot(cc, aes(x = lag, y = cf)) + 
      geom_linerange(dat = cc, aes(ymin = 0, ymax = cf), size = 1.2, color = pCols)
  }
  
  p <- p + labs(x = "Lag", y = yLabs)
  if(is.null(xlim)) xlim <- cc$lag
  if(is.null(ylim)) ylim <- cc$cf
  p <- myTheme(p,
    x = xlim, y = c(ylim, 0), shiny = shiny,
    legend.position = legend.position
  )
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myInf <- function(dat, partner, tau,
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL){
  
  dat <- dat[dat$partner == partner, ]
  other <- ifelse(partner == "y", "x", "y")
  
  if(is.null(xlim)) xlim <- dat$value
  if(is.null(ylim)) ylim <- dat$influence
  
  p <- ggplot(dat, aes(x = value, y = influence)) + geom_point(size = 2) +
    geom_vline(xintercept = tau) + geom_hline(yintercept = 0) + 
    labs(x = bquote(italic(.(partner)[t])[-1]), y = bquote(paste(italic(beta[.(other)]), italic(.(partner)[t])[-1])))
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
    geom_line(size = 1) +
    labs(x = xlab, y = ylab)
  p <- myTheme(p, x = xlim, y = ylim, shiny = shiny)
  p <- p + theme(text = element_text(size = 16),
                 axis.text = element_text(size = 12),
                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

my3D <- function(dat, partner = NULL){
  t <- max(dat$t)
  dat$lag1 <- c(NA, dat$value[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  pCols <- viridis::viridis(2, begin = .8, end = .4, option = "A")
  if(!is.null(partner)){
    ifelse(partner == "y", pCols <- pCols[2], pCols <- pCols[1])
  }
  
  f <- list(family = "serif",
            size = 14)
  
  if(is.null(partner)){
    p <- plot_ly(dat, x = ~t, y = ~lag1, z = ~value, color = dat$partner, colors = pCols,
                 type = "scatter3d", mode = "lines+markers",
                 line = list(width = 4),
                 marker = list(size = 4))
    partner <- "y/x"
  } else {
    dat <- dat[dat$partner == partner, ]
    p <- plot_ly(dat, x = ~t, y = ~lag1, z = ~value,
                 type = "scatter3d", mode = "lines+markers",
                 line = list(width = 4, color = pCols),
                 marker = list(size = 4, color = pCols))
  }
  
  p <- layout(p, font = f, 
              scene = list(yaxis = list(title = paste0("<i>", partner, "<sub>t</i>-1</sub>"),
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

myTVpars <- function(tvFit, partner = NULL, shiny = F){
  # option to suppress plot
  # plt <- plot(tvFit)
  
  if(!shiny) {
    plt <- oddsratio::no_plot(tvFit)
    shift <- tvFit$coefficients[1]
    tvpars <- data.frame(x = c(plt[[1]]$x, plt[[2]]$x, plt[[3]]$x), 
                         y = c(plt[[1]]$fit + shift, plt[[2]]$fit, plt[[3]]$fit), 
                         se = c(plt[[1]]$se, plt[[2]]$se, plt[[3]]$se),
                         p = rep(1:3, each = length(plt[[1]]$x)))
  } else {
    tvpars <- data.frame(x = c(tvFit$alpha, tvFit$phi, tvFit$beta),
                         y = rep(1:length(tvFit$alpha), 3),
                         p = rep(1:3, each = length(tvFit$alpha)))
  }
  
  labels <- c(bquote(alpha[.(partner), t]), bquote(phi[.(partner), t]), bquote(beta[.(partner), t]))
  
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
  
  if(shiny) {
    p <- ggplot(tvpars, aes(x = x, y = y, color = as.factor(p))) +
      geom_line() +
      scale_color_manual(values = c("grey40", "grey80", "black"), labels = labels) +
      labs(x = expression(italic("t")), y = "Parameter Value")
  }
  
  if(!shiny) p <- myTheme(p, x = tvpars$x, y = c(tvpars$y + tvpars$se, tvpars$y - tvpars$se))
  if(shiny)  p <- myTheme(p, x = tvpars$x, y = tvpars$y)
  return(p)
}

myTVfit <- function(fit1, fit2, par) {
  plt1 <- oddsratio::no_plot(fit1)
  plt2 <- oddsratio::no_plot(fit2)
  
  shift1 <- shift2 <- 0
  if(par == "alpha") {
    shift1 <- fit1$coefficients[1]
    shift2 <- fit2$coefficients[1]
    labels <- c(bquote(alpha[y, t]), bquote(alpha[x, t]))
    i <- 1
  }
  if(par == "phi") {
    labels <- c(bquote(phi[y, t]), bquote(phi[x, t]))
    i <- 2
  }
  if(par == "beta") {
    labels <- c(bquote(beta[y, t]), bquote(beta[x, t]))
    i <- 3
  }
  
  
  dat <- data.frame(x = c(plt2[[i]]$x, plt1[[i]]$x),
                    y = c(plt2[[i]]$fit + shift2, plt1[[i]]$fit + shift1),
                    se = c(plt2[[i]]$se, plt1[[i]]$se),
                    p = rep(1:2, each = length(plt1[[1]]$x)))
  
  p <- ggplot(dat, aes(x = x, y = y, fill = as.factor(p), color = as.factor(p))) +
    geom_segment(aes(x = min(dat$x), xend = max(dat$x), 
                     y = 0, yend = 0), linetype = "dashed", color = "grey75") +
    geom_line() +
    geom_ribbon(aes(ymin = y - se,
                    ymax = y + se,
                    fill = as.factor(p),
                    color = as.factor(p)),
                alpha = .5) +
    scale_color_manual(values = c("black", "grey"), labels = labels) +
    scale_fill_manual(values = c("black", "grey"), labels = labels) +
    labs(x = expression(italic("t")), y = "Parameter Value")
  
  p <- myTheme(p, x = dat$x, y = c(dat$y + dat$se, dat$y - dat$se))
  
  return(p)
}

myDistribution <- function(dat, partner = NULL, shiny = F, legend.position = NULL){ 
  pCols <- c("grey", "black")
  if(shiny) pCols <- viridis::viridis(2, begin = .8, end = .4, option = "A")
  xLab <- "Value"
  
  p <- ggplot(dat) + 
    geom_histogram(aes(x = value, y = ..density.., fill = partner), 
                   alpha = 0.5, position = "identity") + 
    stat_density(aes(x = value, colour = partner), 
                 geom = "line", size = 1, position = "identity") +
    scale_fill_manual(values = pCols) +
      scale_color_manual(values = pCols) +
      labs(x = xLab, y = "Density")

  ylim <- c(0, ggplot_build(p)$layout$panel_params[[1]]$y.range[2])

  if (!is.null(partner)) {
    dat <- dat[dat$partner == partner, ]
    if (partner == "y") pCols <- pCols[2]
    if (partner == "x") pCols <- pCols[1]
    xLab <- bquote(italic(.(partner)))

    p <- ggplot(dat) + 
    geom_histogram(aes(x = value, y = ..density..), 
                   alpha = 0.5, position = "identity", fill = pCols) + 
    stat_density(aes(x = value), geom = "line", 
                 size = 1, position = "identity", color = pCols) +
    labs(x = xLab, y = "Density")

    ylim <- c(0, ggplot_build(p)$layout$panel_params[[1]]$y.range[2])
  }
  
  p <- myTheme(p, x = dat$value, y = ylim,
               shiny = shiny, legend.position = legend.position)

  return(p)
}
