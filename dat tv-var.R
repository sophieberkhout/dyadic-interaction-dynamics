source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
alpha <- change_sine(amplitude = 2, freq = 3, phase = 1, deviation = 0, 300)
dat.s <- simVARS(occasions = 300, burnin = 20,
                    type = "TV",
                    params_y = list(alpha = alpha,
                                    phi = 0.5,
                                    beta = 0.2),
                    params_x = list(alpha = 0,
                                    phi = 0.5,
                                    beta = 0.2)
)

set.seed(1)
alpha <- change_linear(-2, 2, 300)
dat.l <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = alpha,
                                 phi = 0.5,
                                 beta = 0.2),
                 params_x = list(alpha = 0,
                                 phi = 0.5,
                                 beta = 0.2)
)

lim <- c(min(c(dat.l$behavior, dat.s$behavior)), max(c(dat.l$behavior, dat.s$behavior)))
myTS(dat.l, ylim = lim)
mySSP(dat.l, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.l, ylim = c(0, 1))
myTSsimple(dat.l$t, dat.l$alpha_y)

myTS(dat.s, ylim = lim)
mySSP(dat.s, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.s, ylim = c(0, 1))
myTSsimple(dat.s$t, dat.s$alpha_y)