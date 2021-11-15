source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
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
                                        z2 = c(.1, .03, .1)))


myTS(dat.msvar, regime = T)
mySSP(dat.msvar, type = "carryover")
mySSP(dat.msvar, type = "spillover")
myCCF(dat.msvar)

lim <- c(dat.hmm$value, dat.msvar$value)
# myTS(dat.hmm, regime = T, partner = "y")
# myTS(dat.hmm, regime = T, regimeType = "shades")
p <- myTS(dat.msvar, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.msvar, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("../Plots/MSVAR/MSVAR_ts.pdf", p, width = 5, height = 3)
mySSP(dat.msvar, ylim = lim, xlim = lim, type = "carryover", filename = "../Plots/MSVAR/MSVAR_ss.pdf")
mySSP(dat.msvar, ylim = lim, xlim = lim, type = "spillover", filename = "../Plots/MSVAR/MSVAR_sohw.pdf")
myCCF(dat.msvar, ylim = c(0, 1), xlim = c(-10, 10), filename = "../Plots/MSVAR/MSVAR_ccf.pdf")
