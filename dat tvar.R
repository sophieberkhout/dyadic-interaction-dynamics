source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.a <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = c(0, 5),
                                 phi = 0.4,
                                 beta = 0.2,
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.4,
                                 beta = 0),
                 innovations = list(one = c(.1, .3, .1), # should be correlation instead of covariance
                                    two = c(.1, .03, .1))
)

set.seed(1)
dat.b <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = 0,
                                 phi = 0.4,
                                 beta = c(0.2, 0.6),
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.4,
                                 beta = 0),
                 innovations = list(one = c(.1, .3, .1), # should be correlation instead of covariance
                                    two = c(.1, .03, .1))
)

lim <- c(min(c(dat.a$value, dat.b$value)), max(c(dat.a$value, dat.b$value)))

myTS(dat.a, ylim = lim)
myTS(dat.a, ylim = lim, regime = T, regimeType = "points")
mySSP(dat.a, type = "carryover", xlim = lim, ylim = lim)
# mySSP(dat.a, type = "spillover", partner = "y")
mySSP(dat.a, type = "spillover_threshold", partner = "y", tau = 0, ylim = lim)

myTS(dat.b)
myTS(dat.b, regime = T, regimeType = "point")
mySSP(dat.b, type = "carryover", xlim = lim, ylim = lim)
# mySSP(dat.b, type = "spillover", partner = "y")
mySSP(dat.b, type = "spillover_threshold", partner = "y", tau = 0, ylim = lim)
myInf(dat.b, partner = "x", tau = 0)