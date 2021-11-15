source("simVARS.R")
source("plotFunctions.R")

# set.seed(1)
# alpha <- change_sine(amplitude = 2, freq = 3, phase = 1, deviation = 0, 300)
# dat.s <- simVARS(occasions = 300, burnin = 20,
#                     type = "TV",
#                     params_y = list(alpha = alpha,
#                                     phi = 0.5,
#                                     beta = 0.2),
#                     params_x = list(alpha = 0,
#                                     phi = 0.5,
#                                     beta = 0.2)
# )
# 
# set.seed(1)
# alpha <- change_linear(-2, 2, 300)
# dat.l <- simVARS(occasions = 300, burnin = 20,
#                  type = "TV",
#                  params_y = list(alpha = alpha,
#                                  phi = 0.5,
#                                  beta = 0.2),
#                  params_x = list(alpha = 0,
#                                  phi = 0.5,
#                                  beta = 0.2)
# )

beta.s <- change_sine(amplitude = .3, freq = 1, phase = 0, deviation = .5, 300)
set.seed(1)
dat.s <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = 0,
                                 phi = 0.2,
                                 beta = beta.s),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)


beta.l <- change_linear(0.2, .6/300, 300)
set.seed(1)
dat.l <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = 0,
                                 phi = 0.2,
                                 beta = beta.l),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)

lim <- c(min(c(dat.l$value, dat.s$value)), max(c(dat.l$value, dat.s$value)))
myTS(dat.l, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_ts.pdf")
mySSP(dat.l, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_ar.pdf")
mySSP(dat.l, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_so.pdf")
# mySSP(dat.l, type = "spillover", partner = "y", ylim = lim)
# myCCF(dat.l, ylim = c(0, 1))
myTSsimple(dat.l$t, dat.l$beta_y, ylab = "Coefficient", filename = "../Plots/TV-VAR/TV-VAR linear.pdf")

myTS(dat.s, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_ts.pdf")
mySSP(dat.s, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_ar.pdf")
mySSP(dat.s, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_so.pdf")
# mySSP(dat.s, type = "spillover", partner = "y", ylim = lim)
# myCCF(dat.s, ylim = c(0, 1))
myTSsimple(dat.s$t, dat.s$beta_y, ylab = "Coefficient", filename = "../Plots/TV-VAR/TV-VAR sine.pdf")
