source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.hmm <- simVARS(occasions = 300, burnin = 20,
                   type = "HMM", probs = c(.9, .6),
                   params_y = list(mu = c(0, 3)),
                   params_x = list(mu = c(0, 3))
)

myTS(dat.hmm, regime = T, partner = "y")
myTS(dat.hmm, regime = T, regimeType = "point")
mySSP(dat.hmm, type = "carryover")
mySSP(dat.hmm, type = "spillover")
myCCF(dat.hmm)