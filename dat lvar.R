dat <- simVARS(10000, 50, type = "L",
               params_y = list(alpha = 0, phi = 0.5, beta = 0.2),
               params_x = list(alpha = 0, phi = 0.3, beta = 0),
               indicators_y = list(means = 0, lambdas = 1, epsilons = 2),
               indicators_x = list(means = 0, lambdas = 1, epsilons = 3),
               longformat = T
)

dat <- simVARS(10, 20,
               type = "L",
               params_y = list(alpha = 0, phi = 0.5, beta = 0.2),
               params_x = list(alpha = 0, phi = 0.3, beta = 0),
               indicators_y = list(means = 0, lambdas = 1, epsilons = 2),
               indicators_x = list(means = rep(0, 2), lambdas = c(1, .5), epsilons = c(3, 1)),
               longformat = F
)
