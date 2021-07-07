source("simVARS.R")
source("plotFunctions.R")

###### VAR ######
dat.2 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.5, 0.2),
                 params_x = c(0, 0.5, 0.2),
                 seed = 1)

dat.8 <- simVARS(occasions = 300, burnin = 20,
                 type = "VAR", 
                 params_y = c(0, 0.5, 0.8),
                 params_x = c(0, 0.5, 0.2),
                 seed = 1)


lim <- c(min(c(dat.2$behavior, dat.8$behavior)), max(c(dat.2$behavior, dat.8$behavior)))
myTS(dat.2, ylim = lim, filename = "../Plots/VAR/VARlow_ts.pdf")
mySSP(dat.2, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARlow_ss.pdf")
mySSP(dat.2, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARlow_sohw.pdf")
myCCF(dat.2, ylim = c(0, 1), filename = "../Plots/VAR/VARlow_ccf.pdf")

myTS(dat.8, ylim = lim, filename = "../Plots/VAR/VARhigh_ts.pdf")
mySSP(dat.8, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARhigh_ss.pdf")
mySSP(dat.8, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/VAR/VARhigh_sohw.pdf")
myCCF(dat.8, ylim = c(0, 1), filename = "../Plots/VAR/VARlow_ccf.pdf")


##### Time-Varying VAR ######
dat.s <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = list(2, "sine"),
                                 phi = .5,
                                 beta = .2),
                 params_x = list(alpha = 0,
                                 phi = .5,
                                 beta = 0),
                 seed = 1)

dat.l <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = list(2, "linear"),
                                 phi = .5,
                                 beta = .2),
                 params_x = list(alpha = 0,
                                 phi = .5,
                                 beta = 0),
                 seed = 1)

lim <- c(min(c(dat.l$behavior, dat.s$behavior)), max(c(dat.l$behavior, dat.s$behavior)))
myTS(dat.l, ylim = lim)
mySSP(dat.l, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.l, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.l, ylim = c(0, 1))

myTS(dat.s, ylim = lim)
mySSP(dat.s, type = "carryover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", xlim = lim, ylim = lim)
mySSP(dat.s, type = "spillover", partner = "y", ylim = lim)
myCCF(dat.s, ylim = c(0, 1))
myTSsimple(dat.s$t, dat.s$alpha_y)

###### Threshold VAR ######

dat.a <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = c(0, 5),
                                 phi = 0.5,
                                 beta = 0.2,
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.5,
                                 beta = 0),
                 seed = 1)

dat.b <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = 0,
                                 phi = 0.5,
                                 beta = c(0.2, 0.6),
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.5,
                                 beta = 0),
                 seed = 1)

myTS(dat.a)
mySSP(dat.a, type = "carryover")
mySSP(dat.a, type = "spillover_threshold", partner = "y", tau = 0)

myTS(dat.b)
myTS(dat.b, partner = "y", regime = T)
mySSP(dat.b, type = "carryover")
mySSP(dat.b, type = "spillover_threshold", partner = "y", tau = 0)
myInf(dat.b, partner = "x", tau = 0)

###### Hidden Markov Model ######

dat.hmm <- simVARS(occasions = 300, burnin = 20,
                   type = "HMM", probs = c(.9, .6),
                   params_y = list(mu = c(0, 3)),
                   params_x = list(mu = c(0, 3)),
                   seed = 1)

myTS(dat.hmm, regime = T)
mySSP(dat.hmm, type = "carryover")
mySSP(dat.hmm, type = "spillover")
myCCF(dat.hmm)

###### Markov-Switching VAR ######

dat.msvar <- simVARS(occasions = 300, burnin = 20,
                     type = "MS", probs = c(.9, .6),
                     params_y = list(alpha = c(0, 3),
                                     phi = .5,
                                     beta = c(.1, .3)),
                     params_x = list(alpha = c(0, 3),
                                     phi = .5,
                                     beta = 0),
                     innovations = list(z1 = c(.5, .3, .3, .5),
                                        z2 = c(.8, .3, .3, .8)),
                     seed = 1)


myTS(dat.msvar, regime = T)
mySSP(dat.msvar, type = "carryover")
mySSP(dat.msvar, type = "spillover")
myCCF(dat.msvar)