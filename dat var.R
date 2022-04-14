source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.2 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.2, 0.2),
                 params_x = c(0, 0.2, 0.2),
                 )


set.seed(2)
dat.8 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.2, 0.8),
                 params_x = c(0, 0.2, 0.2),
                 )


lim <- c(min(c(dat.2$value, dat.8$value)), max(c(dat.2$value, dat.8$value)))
myTS(dat.2, ylim = lim, filename = "Plots/VAR/VARlow_ts.pdf")
mySSP(dat.2, type = "carryover", xlim = lim, ylim = lim, filename = "Plots/VAR/VARlow_ss.pdf")
mySSP(dat.2, type = "spillover", xlim = lim, ylim = lim, filename = "Plots/VAR/VARlow_sohw.pdf")
myCF(dat.2, xlim = c(-10, 10), ylim = c(0, 1), 
     type = "CCF", partner = "y", filename = "Plots/VAR/VARlow_ccf.pdf")

myTS(dat.8, ylim = lim, filename = "Plots/VAR/VARhigh_ts.pdf")
mySSP(dat.8, type = "carryover", xlim = lim, ylim = lim, filename = "Plots/VAR/VARhigh_ss.pdf")
mySSP(dat.8, type = "spillover", xlim = lim, ylim = lim, filename = "Plots/VAR/VARhigh_sohw.pdf")
myCF(dat.8, xlim = c(-10, 10), ylim = c(0, 1), 
     type = "CCF", partner = "y", filename = "Plots/VAR/VARhigh_ccf.pdf")
