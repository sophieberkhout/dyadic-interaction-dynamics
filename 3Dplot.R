##### 3D
library(plotly)
library(tidyverse)
dat <- gather(dat.2, "Partner", "Behavior")
dat$test <- c(dat.2$H, dat.2$W)
dat$Partner <- as.factor(dat$Partner)
dat$t <- rep(1:100, 2)

dat.2$lag1 <- c(NA, dat.2$behavior[-nrow(dat.2)])
dat.2$lag1[300+1] <- NA
fig <- plot_ly(dat.2, x = ~lag1, y = ~t, z = ~lag1, color = ~partner, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_trace(mode = 'lines+markers')
fig <- fig %>% layout(scene = list(xaxis = list(title = 't'),
                                   yaxis = list(title = 'Behavior'),
                                   zaxis = list(title = 'Lag 1')))
fig


fig <- plot_ly(dat.2[1:20,], x = ~lag1, y = ~t, z = ~behavior, color = "red")
fig <- fig %>% add_trace(mode = 'lines+markers', type = "scatter3d")
# fig <- fig %>% layout(scene = list(xaxis = list(title = 't'),
#                                    yaxis = list(title = 'Behavior'),
#                                    zaxis = list(title = 'Lag 1')))
fig
