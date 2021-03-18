library(ggplot2)
library(tidyverse)
source("dat.R")

dat.2 <- VAR1(t = 300,
              u_w = 0, u_h = 0,
              phi_w = 0.5, phi_h = 0.5,
              phi_wh = 0.2, phi_hw = 0.2)

set.seed(1)
dat.8 <- VAR1(t = 300,
              u_w = 0, u_h = 0,
              phi_w = 0.5, phi_h = 0.5,
              phi_wh = 0.2, phi_hw = 0.8)

##### VAR #####
## high influence
# time-series
phigh_ts <- ggplot(dat.8, aes(x = t, y = Behavior, color = Partner)) + 
  geom_line() +
  labs(x = expression(italic("t")), y = "Behavior") +
  scale_color_manual(values = c("grey", "black"))
phigh_ts <- myTheme(phigh_ts, dat.8$t, dat.8$Behavior)
ggsave("VARhigh_ts.pdf", phigh_ts , device = "pdf", width = 5, height = 3)

# scatter plot autoregression
dat.8$lag1 <- c(NA, dat.8$Behavior[-nrow(dat.8)])
dat.8$lag1[max(dat.8$t)+1] <- NA

phigh_ar <-  ggplot(dat.8, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
  geom_smooth(method = "lm", se = F, fullrange = T)
phigh_ar <- myTheme(phigh_ar, x = dat.8$Behavior, y = dat.8$lag1)
ggsave("VARhigh_ss.pdf", phigh_ar, device = "pdf", width = 5, height = 3)

# scatter plot spill-over h-w
phigh_sohw <- ggplot(dat.8, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
  scale_color_manual(values = c("grey", "black")) +
  labs(x = expression(italic("y/x"[t-1])), y = expression(italic("x/y"[t])), color = "legend") +
  geom_smooth(method = "lm", se = F, fullrange = T)
phigh_sohw <- myTheme(phigh_sohw, x = dat.8$Behavior, y = dat.8$lag1)
ggsave("VARhigh_sohw.pdf", phigh_sohw, device = "pdf", width = 5, height = 3)

# ccf
cc.8 <- ccf(dat.8[dat.8$Partner == "y", "Behavior"], 
            dat.8[dat.8$Partner == "x", "Behavior"])
cc.8 <- data.frame(Lag = cc.8$lag, CCF = cc.8$acf)
cc.8 <- cc.8[which(cc.8$Lag == -10):which(cc.8$Lag == 10), ]
phigh_ccf <- ggplot(cc.8, aes(x = Lag, y = CCF)) + 
  geom_linerange(dat = cc.8, aes(ymin = 0, ymax = CCF))
phigh_ccf <- myTheme(phigh_ccf, x = cc.8$Lag, y = c(cc.8$CCF, cc.2$CCF, 0))
ggsave("VARhigh_ccf.pdf", phigh_ccf, device = "pdf", width = 5, height = 3)


## low influence
# time-series
plow_ts <- ggplot(dat.2, aes(x = t, y = Behavior, color = Partner)) + 
            geom_line() +
            labs(x = expression(italic("t")), y = "Behavior") +
            scale_color_manual(values = c("grey", "black"))
plow_ts <- myTheme(plow_ts, dat.2$t, dat.8$Behavior)
ggsave("VARlow_ts.pdf", plow_ts, device = "pdf", width = 5, height = 3)

# scatter plot autoregression
dat.2$lag1 <- c(NA, dat.2$Behavior[-nrow(dat.2)])
dat.2$lag1[max(dat.2$t)+1] <- NA

plow_ar <- ggplot(dat.2, aes(x = lag1, y = Behavior, color = Partner)) + geom_point(size = 2) + 
            scale_color_manual(values = c("grey", "black")) +
            labs(x = expression(italic("t-1")), y = expression(italic("t"))) +
            geom_smooth(method = "lm", se = F, fullrange = T)
plow_ar <- myTheme(plow_ar, x = dat.8$Behavior, y = dat.8$lag1)
ggsave("VARlow_ss.pdf", plow_ar, device = "pdf", width = 5, height = 3)

# scatter plot spill-over
plow_sohw <- ggplot(dat.2, aes(x = c(lag1[301:600], lag1[1:300]), y = Behavior, color = Partner)) + geom_point(size = 2) +
              scale_color_manual(values = c("grey", "black")) +
              labs(x = expression(italic("y/x"[t-1])), y = expression(italic("x/y"[t])), color = "legend") +
              geom_smooth(method = "lm", se = F, fullrange = T)
plow_sohw <- myTheme(plow_sohw, x = dat.8$Behavior, y = dat.8$lag1)
ggsave("VARlow_sohw.pdf", plow_sohw, device = "pdf", width = 5, height = 3)

# ccf
cc.2 <- ccf(dat.2[dat.2$Partner == "y", "Behavior"], dat.2[dat.2$Partner == "x", "Behavior"])
cc.2 <- data.frame(Lag = cc.2$lag, CCF = cc.2$acf)
cc.2 <- cc.2[which(cc.2$Lag == -10):which(cc.2$Lag == 10), ]
plow_ccf <- ggplot(cc.2, aes(x = Lag, y = CCF)) + 
  geom_linerange(dat = cc.2, aes(ymin = 0, ymax = CCF))
plow_ccf <- myTheme(plow_ccf, x = cc.8$Lag, y = c(cc.8$CCF, cc.2$CCF, 0))
ggsave("VARlow_ccf.pdf", plow_ccf, device = "pdf", width = 5, height = 3)








##### TV-VAR pars
phi_w1 <- numeric()
for(i in 1:300){
  phi_w1[i] <- change(c(1, 0), i, 300)
  phi_w1[i] <- phi_w1[i] - 0.6
}
phi_w2 <- numeric()
for(i in 1:300){
  phi_w2[i] <- change(c(2, .6), i, 300)
}

phi_1 <- data.frame(phi = phi_w1, t = 1:300)
phi_2 <- data.frame(phi = phi_w2, t = 1:300)

p <- ggplot(dat = phi_2, aes(x = t, y = phi)) + geom_line() +
  labs(x = expression(italic("t")), y = "Parameter") 

p <- myTheme(p, x= 1:300, y = phi_w2)
p
ggsave("TV-VAR sine.pdf", p, device = "pdf", width = 5, height = 3)

p <- ggplot(dat = phi_2, aes(x = t, y = phi)) + geom_line() +
  labs(x = expression(italic("t")), y = "Parameter") 

p <- myTheme(p, x= 1:100, y = phi_w1)
p


#### TAR Influence
dat <- TAR1(t = 100, u_w = 0, u_h = 0, phi_w = 0.5, phi_h = 0.5,
            type_w = "bilinear", type_h = "step",
            phi_hw_p = 0.2, phi_hw_n = 0.4,
            k_w = -0.1, k_h = 0,
            d_h = 0.2)

x <- seq(-.5, .5, length.out = 100)
test_i <- infl(x, type = "bilinear", 
               k = -0.1, 
               phi_Ip = 0.1, phi_In = 0.5, 
               d = NULL)

plot(dat$H, test_i)

dati <- data.frame(x = x,  I = test_i)
p <- ggplot(dati, aes(x = x, y = I)) + geom_line() +
  geom_vline(xintercept = -0.1, alpha = 0.1) + 
  geom_hline(yintercept = 0, alpha = 0.1) +
  labs(x = expression("H"[t-1]), y = expression(paste(phi["hw,s"], "H"[t-1])))
p <- myTheme(p, x = dati$x, y = dati$I)
p


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
