source("simVARS.R")
source("plotFunctions.R")

set.seed(5)
dat.a <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = c(0, 0.5),
                                 phi = 0.2,
                                 beta = 0.2,
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)

set.seed(6)
dat.b <- simVARS(occasions = 300, burnin = 20,
                 type = "T", 
                 params_y = list(alpha = 0,
                                 phi = 0.2,
                                 beta = c(0.2, 0.8),
                                 tau = 0),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)

lim <- c(min(c(dat.a$value, dat.b$value)), max(c(dat.a$value, dat.b$value, 1.6)))

myTS(dat.a, ylim = lim)
p <- myTS(dat.a, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.a, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("Plots/TVAR/taric_ts.pdf", p, width = 5, height = 3)

mySSP(dat.a, type = "carryover", xlim = lim, ylim = lim, filename = "Plots/TVAR/taric_ar.pdf")
mySSP(dat.a, type = "spillover_threshold", partner = "y", tau = 0, ylim = lim, filename = "Plots/TVAR/taric_so.pdf")
myCF(dat.a, type = "CCF", partner = "y", xlim = c(-10, 10), filename = "Plots/TVAR/taric_ccf.pdf")

p_so <- myTS(dat.b, ylim = lim, regime = T, partner = "y", regimeType = "shades") +
  geom_line(aes(x = t, y = value, color = partner), data = dat.b, size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.box = "horizontal",
        legend.position = c(.25, .9)) + 
  ylab("Value")
ggsave("Plots/TVAR/tarso_ts.pdf", p_so, width = 5, height = 3)
mySSP(dat.b, type = "carryover", xlim = lim, ylim = lim, filename = "Plots/TVAR/tarso_ar.pdf")
mySSP(dat.b, type = "spillover_threshold", partner = "y", tau = 0, ylim = lim, filename = "Plots/TVAR/tarso_so.pdf")
myCF(dat.b, type = "CCF", partner = "y", xlim = c(-10, 10), ylim = c(-0.2, 0.8), filename = "Plots/TVAR/tarso_ccf.pdf")
