source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.msvar <- simVARS(occasions = 300, burnin = 20,
                     type = "MS", 
                     probs = c(.9, .6),
                     params_y = list(alpha = c(0, 3),
                                     phi = .4,
                                     beta = .2),
                     params_x = list(alpha = c(0, 3),
                                     phi = .4,
                                     beta = 0),
                     innovations = list(z1 = c(.1, .03, .1),
                                        z2 = c(.1, .03, .1)))


myTS(dat.msvar, regime = T)
mySSP(dat.msvar, type = "carryover")
mySSP(dat.msvar, type = "spillover")
myCCF(dat.msvar)