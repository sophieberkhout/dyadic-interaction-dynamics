library("ggplot2")
source("myTheme.R")

myTS <- function(dat, partner = NULL, regime = F,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL){
  
  if(is.null(partner)){
    p <- ggplot(dat, aes(x = t, y = behavior, color = partner)) + 
    geom_line() +
    labs(x = expression(italic("t")), y = "Behavior") +
    scale_color_manual(values = c("grey50", "black"))
  } else {
    dat <- dat[dat$partner == partner, ]
    p <- ggplot(dat, aes(x = t, y = behavior)) + 
      geom_line() +
      labs(x = bquote(italic("t")), y = bquote(italic(.(partner)))) 
  }
  
  if(regime){
    fill <- ifelse(dat$regime == 1, "white", "grey95")
    # p <- p + geom_rect(aes(xmin = t - 0.5, xmax = t + 0.5,
    #                        ymin = -Inf, ymax = Inf,
    #                        fill = regime), alpha = 0.5) +
    #   scale_fill_manual(values = c("white", "grey"), guide = "none")
    shades <- annotate("rect", xmin = dat$t - 0.5, xmax = dat$t + 0.5,
                       ymin = -Inf, ymax = Inf, fill = fill)
    p$layers <- c(shades, p$layers)
  }
  
  if(is.null(xlim)) xlim <- dat$t
  if(is.null(ylim)) ylim <- dat$behavior
  
  p <- myTheme(p, x = xlim, y = ylim)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

mySSP <- function(dat, type, tau, partner = NULL,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL){
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
    
    if(is.null(xlim)) xlim <- dat$spillover
    if(is.null(ylim)) ylim <- dat$behavior
    
    if(type == "carryover"){
      p <-  ggplot(dat, aes(x = lag1, y = behavior)) +
        geom_point(size = 2) + 
        labs(x = bquote(italic(.(partner)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = "black")
    }
    if(type == "spillover"){
      p <- ggplot(dat, aes(x = spillover, y = behavior)) + 
        geom_point(size = 2) +
        labs(x = bquote(italic(.(other)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = "black") 
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
  
  p <- myTheme(p, x = xlim, y = ylim, legend.position = legend.position)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

myCCF <- function(dat, 
                  filename = NULL, width = 5, height = 3,
                  xlim = NULL, ylim = NULL){
  cc <- ccf(subset(dat, partner == "y", select = "behavior"), 
            subset(dat, partner == "x", select = "behavior"))
  cc <- data.frame(lag = cc$lag, ccf = cc$acf)
  cc <- cc[which(cc$lag == -10):which(cc$lag == 10), ]
  p <- ggplot(cc, aes(x = lag, y = ccf)) + 
    geom_linerange(dat = cc, aes(ymin = 0, ymax = ccf)) +
    labs(x = "Lag", y = "CCF")
  if(is.null(xlim)) xlim <- cc$lag
  if(is.null(ylim)) ylim <- cc$ccf
  p <- myTheme(p, x = xlim, y = c(ylim, 0))
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
                       filename = NULL, width = 5, height = 3){
  xlim <- t
  ylim <- y
  if(is.null(xlab)) xlab <- expression(italic("t"))
  if(is.null(ylab)) ylab <- expression(italic("y"))
  dat <- data.frame(x = t, y = y)
  p <- ggplot(dat, aes(x = x, y = y)) +
    geom_line() +
    labs(x = xlab, y = ylab)
  p <- myTheme(p, x = xlim, y = ylim)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}
