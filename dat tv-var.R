source("simVARS.R")
source("plotFunctions.R")

beta.s <- change_sine(amplitude = .3, freq = 1, phase = 0, deviation = .5, 300)
set.seed(3)
dat.s <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = 0,
                                 phi = 0.2,
                                 beta = beta.s),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)


beta.l <- change_linear(0.2, .6/300, 300)
set.seed(4)
dat.l <- simVARS(occasions = 300, burnin = 20,
                 type = "TV",
                 params_y = list(alpha = 0,
                                 phi = 0.2,
                                 beta = beta.l),
                 params_x = list(alpha = 0,
                                 phi = 0.2,
                                 beta = 0.2)
)

lim <- c(min(c(dat.l$value, dat.s$value)), max(c(dat.l$value, dat.s$value)))
myTS(dat.l, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_ts.pdf")
mySSP(dat.l, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_ar.pdf")
mySSP(dat.l, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARl_so.pdf")
myCF(dat.l, type = "CCF", partner = "y", xlim = c(-10, 10), filename = "../Plots/TV-VAR/TV-VARl ccf.pdf")
myTSsimple(dat.l$t, dat.l$beta_y, ylab = "Coefficient", filename = "../Plots/TV-VAR/TV-VAR linear.pdf")

myTS(dat.s, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_ts.pdf")
mySSP(dat.s, type = "carryover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_ar.pdf")
mySSP(dat.s, type = "spillover", xlim = lim, ylim = lim, filename = "../Plots/TV-VAR/TV-VARs_so.pdf")
myCF(dat.s, type = "CCF", partner = "y", xlim = c(-10, 10), filename = "../Plots/TV-VAR/TV-VARs ccf.pdf")
myTSsimple(dat.s$t, dat.s$beta_y, ylab = "Coefficient", filename = "../Plots/TV-VAR/TV-VAR sine.pdf")

# ZOOMED PLOTS
dat.l.l <- dat.l[c(1:50, 301:350),]
dat.l.r <- dat.l[c(251:300, 551:600),]

lim <- c(dat.l.l$value, dat.l.r$value)
tvp_tsl <- myTS(dat.l.l, ylim = lim, partner = "y")
tvp_tsr <- myTS(dat.l.r, ylim = lim, partner = "y")
ggsave("../Plots/TV-VAR/TV-VARl_ts1.pdf", tvp_tsl , device = "pdf", width = 5, height = 3)
ggsave("../Plots/TV-VAR/TV-VARl_ts2.pdf", tvp_tsr , device = "pdf", width = 5, height = 3)

tvp_arl <- mySSP(dat.l.l, xlim = lim, ylim = lim, type = "spillover", partner = "y")
tvp_arr <- mySSP(dat.l.r, xlim = lim, ylim = lim, type = "spillover", partner = "y")
ggsave("../Plots/TV-VAR/TV-VARl_z1.pdf", tvp_arl , device = "pdf", width = 5, height = 3)
ggsave("../Plots/TV-VAR/TV-VARl_z2.pdf", tvp_arr , device = "pdf", width = 5, height = 3)
