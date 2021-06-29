# Linear plot
source("myTheme.R")

x <- seq(40,80,length.out = 11)
y <- seq(140,200,length.out = 11)
plot(x, y)
dat <- data.frame(x, y)
library(ggplot2)

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_segment(x = 40, xend = 60, yend = 140, y = 140, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 60, xend = 60, yend = 140, y = 170, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_point(aes(x = 60, y = 170), size = 2) + geom_line() +
  geom_point(aes(x = 40, y = 140), size = 2, shape = 21, fill = "white") +
  labs(x = "Weight", y = "Height") 

p <- myTheme(p, x = x, y = y) 

ggsave("eglinear.pdf", p, device = "pdf", width = 5, height = 4)



##### Relationship happiness
p <- ggplot(dat, aes(x = x, y = y)) +
  # geom_segment(x = 50, xend = 80, yend = 180, y = 180, alpha = 0.1, linetype = "dashed", color = "grey") +
  # geom_segment(x = 80, xend = 80, yend = 150, y = 180, alpha = 0.1, linetype = "dashed", color = "grey") +
  # geom_point(aes(x = 80, y = 180), size = 2) + 
  geom_segment(x = 40, xend = 48, yend = y[1], y = y[1], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 48, xend = 48, yend = y[1], y = y[3], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 48, xend = 56, yend = y[3], y = y[3], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 56, xend = 56, yend = y[3], y = y[5], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 56, xend = 64, yend = y[5], y = y[5], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 64, xend = 64, yend = y[5], y = y[7], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 64, xend = 72, yend = y[7], y = y[7], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 72, xend = 72, yend = y[7], y = y[9], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 72, xend = 80, yend = y[9], y = y[9], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = 80, xend = 80, yend = y[9], y = y[11], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_line(size = 1.3) +
  labs(x = "Wife's Happiness", y = "Husband's Happiness") 

p <- myTheme(p, x = x, y = y) 
p <- p +   scale_x_continuous(breaks  = c(45, 75), 
                         labels = c(paste0("Low\n", sprintf("\u2190")), 
                                    paste0("High\n", sprintf("\u2192")))) +
  scale_y_continuous(breaks = c(145, 195), labels = c(paste0(sprintf("\u2193"), "\nLow"), 
                                                      paste0("High\n", sprintf("\u2191")))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_text(hjust = 0.5))
library("Cairo")
ggsave("eglinearwifehusband.pdf", p, device = cairo_pdf, width = 5, height = 4)



##### Relationship happiness nonlinear
# y2 <- c(150, 152, 155, 160, 173, 200)
y <- c(140, 160, 172, 180, 187, 193, 196, 198, 199, 200, 200)
dat <- data.frame(x = x, y = y)
p <- ggplot(dat, aes(x = x, y = y)) +
  geom_segment(x = x[1], xend = x[3], yend = y[1]+1.5, y = y[1]+1.5, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[3], xend = x[3], yend = y[1]+1.5, y = y[3]-1, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[3], xend = x[5], yend = y[3]-1, y = y[3]-1, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[5], xend = x[5], yend = y[3]-1, y = y[5], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[5], xend = x[7], yend = y[5], y = y[5], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[7], xend = x[7], yend = y[5], y = y[7], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[7], xend = x[9], yend = y[7], y = y[7], alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[9], xend = x[9], yend = y[7], y = y[9]+0.5, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[9], xend = x[11], yend = y[9]+0.5, y = y[9]+0.5, alpha = 0.1, linetype = "dashed", color = "grey") +
  geom_segment(x = x[11], xend = x[11], yend = y[9]+0.5, y = y[11], alpha = 0.1, linetype = "dashed", color = "grey") +
  # geom_segment(x = 50, xend = 80, yend = 160, y = 160, alpha = 0.1, linetype = "dashed", color = "grey") +
  # geom_segment(x = 80, xend = 80, yend = 150, y = 160, alpha = 0.1, linetype = "dashed", color = "grey") +
  # geom_point(aes(x = 80, y = 160), size = 2) + 
  geom_smooth(size = 1, se = F, col = "black") +
  labs(x = "Wife's Happiness", y = "Husband's Happiness") 

p <- myTheme(p, x = x, y = y) 
p <- p +   scale_x_continuous(breaks  = c(45, 75), 
                              labels = c(paste0("Low\n", sprintf("\u2190")), 
                                         paste0("High\n", sprintf("\u2192")))) +
  scale_y_continuous(breaks = c(145, 195), labels = c(paste0(sprintf("\u2193"), "\nLow"), 
                                                      paste0("High\n", sprintf("\u2191")))) +
  theme(axis.ticks = element_blank(), axis.text.y = element_text(hjust = 0.5))

p
library("Cairo")
ggsave("egnonlinearwifehusband.pdf", p, device = cairo_pdf, width = 5, height = 4)
