# library(ggplot2)
# source("myTheme.R")
# library(tidyverse)
source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.2 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.5, 0.2),
                 params_x = c(0, 0.5, 0.2),
                 seed = 1
                 )


set.seed(1)
dat.8 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.5, 0.8),
                 params_x = c(0, 0.5, 0.2),
                 seed = 1
                 )


lim <- c(min(c(dat.2$behavior, dat.8$behavior)), max(c(dat.2$behavior, dat.8$behavior)))
myTS(dat.2, ylim = lim, filename = "../Plots/VAR/VARlow_ts.pdf")
mySSP(dat.2, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARlow_ss.pdf")
mySSP(dat.2, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARlow_sohw.pdf")
myCCF(dat.2, ylim = c(0, 1), filename = "../Plots/VAR/VARlow_ccf.pdf")

myTS(dat.8, ylim = lim, filename = "../Plots/VAR/VARhigh_ts.pdf")
mySSP(dat.8, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARhigh_ss.pdf")
mySSP(dat.8, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARhigh_sohw.pdf")
myCCF(dat.8, ylim = c(0, 1), filename = "../Plots/VAR/VARlow_ccf.pdf")



