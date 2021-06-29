
ggsave("Plots/tarso_ts.pdf", p1$timeseries, device = "pdf", width = 5, height = 3)
ggsave("Plots/tarso_ar.pdf", p1$autoregressive, device = "pdf", width = 5, height = 3)
ggsave("Plots/tarso_inf.pdf", p1$influence, device = "pdf", width = 5, height = 3)
ggsave("Plots/tarso_so.pdf", p1$spillover, device = "pdf", width = 5, height = 3)


ggsave("Plots/taric_ts.pdf", p2$timeseries, device = "pdf", width = 5, height = 3)
ggsave("Plots/taric_ar.pdf", p2$autoregressive, device = "pdf", width = 5, height = 3)
ggsave("Plots/taric_inf.pdf", p2$influence, device = "pdf", width = 5, height = 3)
ggsave("Plots/taric_so.pdf", p2$spillover, device = "pdf", width = 5, height = 3)
