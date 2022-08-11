library(dynr)

estVAR1 <- function(dat, dataFormat = "wide",
                    startval = c(alpha_x = .01, phi_x = .01, beta_x = .01,
                                 alpha_y = .01, phi_y = .01, beta_y = .01),
                    useInitNoise = T,
                    values.latent = c(1, .01, .01, 1)) {

  if(dataFormat == "long") {
    df <- data.frame(x = dat$value[dat$partner == "x"],
                     y = dat$value[dat$partner == "y"])
  } else {
    df <- data.frame(x = dat$x, y = dat$y)
  }

  df_ts   <- ts(df)
  rawData <- dynr.data(df_ts)

  measVAR <- prep.measurement(
    values.load = matrix(c(1, 0,
                           0, 1), ncol = 2), # the loading matrix
    params.load = matrix(rep("fixed", 4), ncol = 2), # fix the loadings so they are not freely estimated
    state.names = c("x", "y"), # state.names identical to obs.names
    obs.names   = c("x", "y")
  )

  formVAR <- list(
    x ~ alpha_x + phi_x * x + beta_x * y, # enter the formulas for both persons
    y ~ alpha_y + phi_y * y + beta_y * x
  )

  dynmVAR <- prep.formulaDynamics(
    formula          = formVAR, # starting values for the parameters in the formulas
    startval         = startval,
    isContinuousTime = FALSE # the VAR model uses discrete time
  )

  initVAR <- prep.initial(
    values.inistate = rep(0, 2), # set initial means to zero
    params.inistate = rep("fixed", 2), # fix the initial means so they are not estimated
    values.inicov   = matrix(c(1, 0,
                               0, 1), ncol = 2), # set the initial variances to 1 en covariances to 0
    params.inicov   = matrix(rep("fixed", 4), ncol = 2) # fix the covariance matrix
  )

  if(useInitNoise) {
    initNoise <- var(df) / 2
    values.latent <- initNoise
  }

  noiseVAR <- prep.noise(
    values.latent   = matrix(values.latent, ncol = 2), # initial values dynamic noise
    params.latent   = matrix(c("z_x","z_xy",
                               "z_xy","z_y"), ncol = 2, byrow = T), # parameter names dynamic noise
    values.observed = diag(rep(0, 2)), # fix measurement noise to zero
    params.observed = matrix(rep("fixed", 4), ncol = 2)
  )

  modVAR <- dynr.model(dynamics    = dynmVAR,
                       measurement = measVAR,
                       noise       = noiseVAR,
                       initial     = initVAR,
                       data        = rawData)

  resVAR <- dynr.cook(modVAR, verbose = FALSE) # verbose = FALSE leaves out output during estimation

  return(resVAR)
}