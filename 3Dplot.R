##### 3D
library(plotly)
library(tidyverse)
dat <- gather(dat.2, "Partner", "Behavior")
dat$test <- c(dat.2$H, dat.2$W)
dat$Partner <- as.factor(dat$Partner)
dat$t <- rep(1:100, 2)
fig <- plot_ly(dat, x = ~t, y = ~Behavior, z = ~test, color = ~Partner, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_trace(mode = 'lines+markers')
fig <- fig %>% layout(scene = list(xaxis = list(title = 't'),
                                   yaxis = list(title = 'Behavior'),
                                   zaxis = list(title = 'test')))
fig
