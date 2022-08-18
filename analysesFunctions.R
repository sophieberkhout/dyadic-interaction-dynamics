library(dynr)

dynrEstimation <- function(dat, model, dataFormat = "wide") {
  if (dataFormat == "long") {
    df <- data.frame(
      x = dat$value[dat$partner == "x"],
      y = dat$value[dat$partner == "y"]
    )
  } else {
    df <- data.frame(x = dat$x, y = dat$y)
  }

  df_ts <- ts(df)
  rawData <- dynr.data(df_ts)

  modelSpecs <- dynrModel(model, varDat = var(df))

  mod <- dynr.model(
    dynamics = modelSpecs$dynamics,
    measurement = modelSpecs$measurement,
    noise = modelSpecs$noise,
    initial = modelSpecs$initial,
    data = rawData
  )

  res <- dynr.cook(mod, verbose = FALSE)

  return(res)
}

dynrModel <- function(model, varDat) {
  initNoise <- varDat / 2
  values.latent <- initNoise

  values.load <- matrix(c(1, 0, 0, 1), ncol = 2)
  params.load <- matrix(rep("fixed", 4), ncol = 2)
  obs.names <- c("x", "y")
  values.int <- NULL
  params.int <- NULL
  params.latent <- matrix(c(
    "zeta_x", "zeta_xy",
    "zeta_xy", "zeta_y"
  ), ncol = 2, byrow = T)
  values.observed <- diag(rep(0, 2))

  form <- list(
    x ~ alpha_x + phi_x * x + beta_x * y,
    y ~ alpha_y + phi_y * y + beta_y * x
  )

  startval <- c(
    alpha_x = .01, phi_x = .01, beta_x = .01,
    alpha_y = .01, phi_y = .01, beta_y = .01
  )

  if (model == "VAR") {
    state.names <- obs.names

    params.observed <- matrix(rep("fixed", 4), ncol = 2)
  }

  if (model == "L") {
    state.names <- c("xi", "eta")
    values.int <- matrix(c(.01, .01), ncol = 1)
    params.int <- matrix(c("nu_x", "nu_y"), ncol = 1)

    form <- list(
      xi ~ alpha_x + phi_x * xi + beta_x * eta,
      eta ~ alpha_y + phi_y * eta + beta_y * xi
    )

    params.observed <- matrix(c(
      "epsilon_x", "epsilon_xy",
      "epsilon_xy", "epsilon_y"
    ), ncol = 2, byrow = T) # estimate the measurement error

    values.observed <- values.latent
  }


  meas <- prep.measurement(
    values.load = values.load,
    params.load = params.load,
    state.names = state.names,
    obs.names = obs.names,
    values.int = values.int,
    params.int = params.int
  )

  dynm <- prep.formulaDynamics(
    formula          = form,
    startval         = startval,
    isContinuousTime = FALSE # the VAR model uses discrete time
  )

  init <- prep.initial(
    values.inistate = rep(0, 2), # set initial means to zero
    params.inistate = rep("fixed", 2), # fix the initial means so they are not estimated
    values.inicov = matrix(c(1, 0, 0, 1), ncol = 2),
    params.inicov = matrix(rep("fixed", 4), ncol = 2) # fix the covariance matrix
  )

  noise <- prep.noise(
    values.latent = matrix(values.latent, ncol = 2), # initial values dynamic noise
    params.latent = params.latent,
    values.observed = values.observed, # fix measurement noise to zero
    params.observed = params.observed
  )

  return(list(
    dynamics = dynm,
    measurement = meas,
    noise = noise,
    initial = init
  ))
}
