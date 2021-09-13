y <- rnorm(100)

means <- c(1, 3, 5, 10)
lambdas <- c(0.2, 0.3, 0.5, 0.1)
us <- rnorm(4)

means + lambdas %*% y + us


means.m <- matrix(c(1, 3, 5, 10))
lambdas.m <- matrix(c(0, 0.3, 0.5, 10))


us.m <- matrix(rnorm(4))

y.m <- matrix(y)

ys.m <- lambdas.m %*% t(y.m)


# 4 x 1
# 100 x 1



us <- MASS::mvrnorm(100, c(0, -10, 10, 0), diag(4), empirical = T)

t(means + lambdas.m %*% t(y.m) + t(us))
t(means + lambdas.m %*% t(y.m) + t(us))

mys + t(us)
t(mys) + us

lambdas.m %*% t(y.m)

#### test
t <- 10

params_y <- list(a = 0, ph = 0.5, b = 0.2)
params_x <- list(a = 10, ph = 0.3, b = 0)

# indicators <- list(y = list(means = c(0, 0, 0), 
#                             l = c(1, 0.3, 0.8), 
#                             e = c(1, 5, 1)),
#                    x = list(means = c(10, 10, 10), 
#                             l = c(1, 0.3, 0.8), 
#                             e = c(1, 5, 1)))

indicators <- list(y = list(means = rnorm(10), 
                            l = rnorm(10, sd = 0.3), 
                            e = abs(rnorm(10))),
                   x = list(means = rnorm(10), 
                            l = rnorm(10, sd = 0.3), 
                            e = abs(rnorm(10))))


z <- as.data.frame(MASS::mvrnorm(t, rep(0, 2), diag(2)))
colnames(z) <- c("y", "x")

dat <- LVAR1(t, params_y, params_x, indicators, z)
