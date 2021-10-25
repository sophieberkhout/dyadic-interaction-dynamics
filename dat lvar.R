source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.low <- simVARS(300, 20, 
                   type = "L",
                   params_y = list(alpha = 0, phi = 0.3, beta = 0.2),
                   params_x = list(alpha = 0, phi = 0.3, beta = 0.2),
                   indicators_y = list(means = 0, lambdas = 1),
                   indicators_x = list(means = 0, lambdas = 1),
                   innovations = c(.1, .03, .1),
                   errors = c(.1, .03, .1),
                   longformat = T
)

set.seed(1)
dat.high <- simVARS(300, 20,
                    type = "L",
                    params_y = list(alpha = 0, phi = 0.3, beta = 0.2),
                    params_x = list(alpha = 0, phi = 0.3, beta = 0.2),
                    indicators_y = list(means = 0, lambdas = 1),
                    indicators_x = list(means = 0, lambdas = 1),
                    innovations = c(.1, .03, .1),
                    errors = c(1, .3, 1),
                    longformat = T
)

lim <- c(min(c(dat.low$behavior, dat.high$behavior)), max(c(dat.low$behavior, dat.high$behavior)))
myTS(dat.low, ylim = lim)
mySSP(dat.low, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.low, type = "spillover", xlim = lim, ylim = lim)
myCCF(dat.low, ylim = c(0, 1))

myTS(dat.high, ylim = lim)
mySSP(dat.high, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.high, type = "spillover", xlim = lim, ylim = lim)
myCCF(dat.high, ylim = c(0, 1))
