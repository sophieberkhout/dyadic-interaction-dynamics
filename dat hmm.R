source("simVARS.R")
source("plotFunctions.R")

set.seed(1)
dat.hmm <- simVARS(occasions = 300, burnin = 20,
                   type = "HMM", probs = c(.9, .7),
                   params_y = list(mu = c(0, 1)),
                   params_x = list(mu = c(0, 1))
)

lim <- c(dat.hmm$value, dat.msvar$value)
# myTS(dat.hmm, regime = T, partner = "y")
# myTS(dat.hmm, regime = T, regimeType = "shades")
p <- myTS(dat.hmm, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.hmm, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("../Plots/HMM/HMM_ts.pdf", p, width = 5, height = 3)
mySSP(dat.hmm, ylim = lim, xlim = lim, type = "carryover", filename = "../Plots/HMM/HMM_ss.pdf")
mySSP(dat.hmm, ylim = lim, xlim = lim, type = "spillover", filename = "../Plots/HMM/HMM_sohw.pdf")
myCCF(dat.hmm, ylim = c(0, 1), xlim = c(-10, 10), filename = "../Plots/HMM/HMM_ccf.pdf")
