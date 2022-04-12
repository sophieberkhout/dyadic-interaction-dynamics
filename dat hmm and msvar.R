source("simVARS.R")
source("plotFunctions.R")

set.seed(7)
dat.hmm <- simVARS(occasions = 300, burnin = 20,
                   type = "HMM", probs = c(.9, .7),
                   params_y = list(mu = c(0, 1)),
                   params_x = list(mu = c(0, 1))
)

set.seed(8)
dat.msvar <- simVARS(occasions = 300, burnin = 20,
                     type = "MS", 
                     probs = c(.9, .7),
                     params_y = list(alpha = c(0, 1),
                                     phi = .4,
                                     beta = .2),
                     params_x = list(alpha = 0,
                                     phi = .2,
                                     beta = .2),
                     innovations = list(z1 = c(.1, .03, .1),
                                        z2 = c(.1, .03, .1))
)

lim <- c(dat.hmm$value, dat.msvar$value)

p <- myTS(dat.hmm, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.hmm, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("../Plots/HMM/HMM_ts.pdf", p, width = 5, height = 3)
mySSP(dat.hmm, ylim = lim, xlim = lim, type = "carryover", filename = "../Plots/HMM/HMM_ss.pdf")
mySSP(dat.hmm, ylim = lim, xlim = lim, type = "spillover", filename = "../Plots/HMM/HMM_sohw.pdf")
myCCF(dat.hmm, xlim = c(-10, 10), filename = "../Plots/HMM/HMM_ccf.pdf")

p <- myTS(dat.msvar, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.msvar, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("../Plots/MSVAR/MSVAR_ts.pdf", p, width = 5, height = 3)
mySSP(dat.msvar, ylim = lim, xlim = lim, type = "carryover", filename = "../Plots/MSVAR/MSVAR_ss.pdf")
mySSP(dat.msvar, ylim = lim, xlim = lim, type = "spillover", filename = "../Plots/MSVAR/MSVAR_sohw.pdf")
myCCF(dat.msvar, xlim = c(-10, 10), ylim = c(-0.2, 0.8), filename = "../Plots/MSVAR/MSVAR_ccf.pdf")
