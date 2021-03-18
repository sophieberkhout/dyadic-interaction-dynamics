# Linear plot

x <- -5:5
y <- seq(-10, 10, by  = 2)
plot(x, y)
dat <- data.frame(x, y)
library(ggplot2)

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_segment(x = -10, xend = 10, yend = 0, y = 0, color = "grey") +
  geom_segment(x = 0, xend = 0, yend = -10, y = 10, alpha = 0.1, color = "grey") +
  geom_segment(x = -10, xend = 1, yend = 2, y = 2, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 1, xend = 1, yend = -10, y = 2, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_point(aes(x = 1, y = 2), size = 2) + geom_line()
p
p <- myTheme(p, x = y, y = y)
ggsave("eglinear.pdf", p, device = "pdf", width = 5, height = 5)
