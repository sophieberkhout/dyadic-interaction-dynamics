library("ggplot2")
source("myTheme.R")

myTS <- function(dat, 
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL){
  p <- ggplot(dat, aes(x = t, y = behavior, color = partner)) + 
    geom_line() +
    labs(x = expression(italic("t")), y = "Behavior") +
    scale_color_manual(values = c("grey", "black"))
  
  if(is.null(xlim)) xlim <- dat$t
  if(is.null(ylim)) ylim <- dat$behavior
  
  p <- myTheme(p, x = xlim, y = ylim)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}

mySSP <- function(dat, type, partner = NULL,
                 filename = NULL, width = 5, height = 3,
                 xlim = NULL, ylim = NULL){
  t <- max(dat$t)
  dat$lag1 <- c(NA, dat$behavior[-nrow(dat)])
  dat$lag1[t+1] <- NA
  
  dat$spillover <- c(dat$lag1[(t+1):(t*2)], dat$lag1[1:t])
  
  if(is.null(xlim)) xlim <- dat$behavior
  if(is.null(ylim)) ylim <- dat$behavior
  
  
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
    dathalf <- dat[dat$partner == partner, ]
    other <- ifelse(partner == "y", "x", "y")
    if(type == "carryover"){
      p <-  ggplot(dathalf, aes(x = lag1, y = behavior)) +
        geom_point(size = 2) + 
        labs(x = bquote(italic(.(partner)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = "black")
    }
    if(type == "spillover"){
      p <- ggplot(dathalf, aes(x = spillover, y = behavior)) + 
        geom_point(size = 2) +
        labs(x = bquote(italic(.(other)["t-1"])), y = bquote(italic(.(partner)["t"]))) +
        geom_smooth(method = "lm", se = F, fullrange = T, color = "black") 
    }
  }
  p <- myTheme(p, x = xlim, y = ylim)
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
  p <- myTheme(p, x = xlim, y = ylim)
  if(!is.null(filename))  ggsave(filename, p, width = width, height = height)
  return(p)
}
