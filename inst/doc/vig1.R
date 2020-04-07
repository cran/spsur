## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>", 
  echo = TRUE, eval = FALSE, warning = FALSE, 
  message = FALSE)

## ----loadpackages-------------------------------------------------------------
#  library(spsur)
#  library(sf)
#  library(dplyr)

## ----data_set-----------------------------------------------------------------
#  data(NCOVR, package = "spsur")

## ----printdatatable-----------------------------------------------------------
#  NCOVR <- st_drop_geometry(NCOVR.sf)
#  knitr::kable(
#    head((NCOVR[1:3, 1:6])),
#    caption = 'First observations of NCOVR dataset' )

## ----plotgeom, fig.cap = "Geometry of the USA counties", fig.align='center'----
#  plot(st_geometry(NCOVR.sf))

## ----W------------------------------------------------------------------------
#  # Obtain coordinates of centroids
#  co <- sf::st_coordinates(sf::st_centroid(NCOVR.sf))
#  lwncovr <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(co, k = 10,longlat = TRUE)))

## ----quantilemap, fig.cap = "Quantile Map; Variable HR60", fig.align = 'center'----
#  q <- quantile(NCOVR.sf$HR60)
#  NCOVR.sf$qm <- (NCOVR.sf$HR60 > q[2]) + (NCOVR.sf$HR60 > q[3]) +(NCOVR.sf$HR60 >= q[4]) + 1
#  plot(NCOVR.sf["qm"], pal = c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D"), main = "")

## ----ols_60-------------------------------------------------------------------
#  formula_60 <- HR60 ~ RD60 + PS60 + MA60 + DV60 + UE60 + SOUTH
#  lm_60 <- lm(formula = formula_60, data = NCOVR.sf)
#  summary(lm_60)

## ----residualquantilemap, fig.cap = "Quantile Map of LS residuals", fig.align='center'----
#  q <- quantile(lm_60$residuals)
#  NCOVR.sf$qmr <- (lm_60$residuals > q[2]) + (lm_60$residuals > q[3]) + (lm_60$residuals >= q[4]) + 1
#  plot(NCOVR.sf["qmr"], pal = c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D"), main = "")

## -----------------------------------------------------------------------------
#  print(spdep::lm.LMtests(model = lm_60, listw = lwncovr,
#                          test = "all"))

## ----Formula------------------------------------------------------------------
#  formula_sur <- HR60 | HR70 ~ RD60 + PS60 + MA60 + DV60 + UE60 + SOUTH |  RD70 + PS70 + MA70 + DV70 + UE70 + SOUTH

## ----SUR-SIM------------------------------------------------------------------
#  sur.sim <- spsurml(formula = formula_sur, type = "sim", data = NCOVR.sf)
#  summary(sur.sim)

## ----residualsurquantilemap, fig.cap="Quantile Map of LS residuals for SUR-SIM model", fig.align='center'----
#  res <- residuals(sur.sim)
#  q <- quantile(res[[1]])
#  NCOVR.sf$SUR_residuals_1960 <- (res[[1]] > q[2]) + (res[[1]] > q[3]) + (res[[1]] >= q[4]) + 1
#  q <- quantile(res[[2]])
#  NCOVR.sf$SUR_residuals_1970 <- (res[[2]]>q[1]) + (res[[2]]>q[3]) + (res[[2]] >= q[4]) + 1
#  plot(NCOVR.sf[c("SUR_residuals_1960","SUR_residuals_1970")],
#       pal = c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D"))

## ----test_SUR-SIM-------------------------------------------------------------
#  lmtest.sur <- lmtestspsur(formula = formula_sur,
#                            listw = lwncovr, data = NCOVR.sf)
#  print(lmtest.sur)

## ----SUR-SLX------------------------------------------------------------------
#  sur.slx <- spsurml(formula = formula_sur, listw = lwncovr,
#                     type = "slx",data = NCOVR.sf)
#  summary(sur.slx)

## ----SUR-SLX2-----------------------------------------------------------------
#  formulaD <-  ~ DV60 |  MA70 + UE70
#  sur.slx2 <- spsurml(formula = formula_sur, listw = lwncovr,
#                      type = "slx", Durbin = formulaD,
#                      data = NCOVR.sf)
#  summary(sur.slx2)

## ----SUR-SLM-ML---------------------------------------------------------------
#  control <- list(fdHess = TRUE)
#  sur.slm <- spsurml(formula = formula_sur, listw = lwncovr,
#                     method = "LU", type = "slm", control = control,
#                     data = NCOVR.sf)
#  summary(sur.slm)

## ----SUR-SEM-ML---------------------------------------------------------------
#  sur.sem <- spsurml(formula = formula_sur, listw = lwncovr,
#                     method = "LU", type = "sem",
#                     control = control, data = NCOVR.sf)
#  summary(sur.sem)

## ----SUR-SDEM-ML--------------------------------------------------------------
#  formulaD <-  ~ DV60 |  MA70 + UE70
#  sur.sdem <- spsurml(formula = formula_sur, listw = lwncovr,
#                     method = "LU", type = "sdem", Durbin = formulaD,
#                     control = control, data = NCOVR.sf)
#  summary(sur.sdem)

## ----SUR-SDM-ML---------------------------------------------------------------
#  formulaD <-  ~ RD60 + PS60 + MA60 + DV60 |  RD70 + PS70 + DV70
#  sur.sdm <- spsurml(formula = formula_sur, listw = lwncovr,
#                     type = "sdm", method = "LU",
#                     Durbin = formulaD, data = NCOVR.sf)
#  summary(sur.sdm)

## ----SUR-SARAR-ML-------------------------------------------------------------
#  sur.sarar <- spsurml(formula = formula_sur, listw = lwncovr,
#                       type = "sarar", method = "LU",
#                       control= control,
#                       data = NCOVR.sf)
#  summary(sur.sarar)

## ----SUR-SLM-IV---------------------------------------------------------------
#  sur.slm.3sls <- spsur3sls(formula = formula_sur,
#                            listw = lwncovr,
#                            type = "slm", maxlagW = 3,
#                            data = NCOVR.sf)
#  summary(sur.slm.3sls)

## ----SUR-SDM-IV---------------------------------------------------------------
#  formulaD <-  ~ RD60 |PS70 + DV70
#  sur.sdm.3sls <- spsur3sls(formula = formula_sur,
#                            listw = lwncovr, type = "sdm",
#                            Durbin = formulaD, data = NCOVR.sf)
#  summary(sur.sdm.3sls)

## ----BP-----------------------------------------------------------------------
#  print(sur.sim$BP)
#  print(sur.slm$BP)
#  print(sur.slm.3sls$BP)

## ----SUR-SLM-ML1--------------------------------------------------------------
#  control <- list(fdHess = FALSE)
#  sur.slm1 <- spsurml(formula = formula_sur, listw = lwncovr,
#                     method = "LU", type = "slm",
#                     data = NCOVR.sf)
#  summary(sur.slm1)

## ----waldbeta1----------------------------------------------------------------
#  R1 <- matrix(c(0, 0, 0, 0, 0, 0, 1,
#                 0, 0, 0, 0, 0, 0, -1), nrow = 1)
#  b1 <- matrix(0, ncol = 1)
#  waldbeta1 <- wald_betas(sur.slm, R1, b1)
#  print(waldbeta1)

## ----SUR-SLM-R-ML-------------------------------------------------------------
#  R1 <- matrix(c(0,0,0,0,0,0,1,
#                 0,0,0,0,0,0,-1), nrow=1)
#  b1 <- matrix(0, ncol = 1)
#  sur.slm.r <- spsurml(formula = formula_sur, listw = lwncovr,
#                       type = "slm", R = R1, b = b1,
#                       control = control, data = NCOVR.sf)
#  summary(sur.slm.r)

## ----wald2, include=FALSE-----------------------------------------------------
#  # equal SOUTH coefficient
#  R21 <- matrix(c(0, 0, 0, 0, 0, 0, 1,
#                  0, 0, 0, 0, 0, 0, -1), nrow = 1)
#  # equal intercept
#  R22 <- matrix(c(1, 0, 0, 0, 0, 0, 0,
#                 -1, 0, 0, 0, 0, 0, 0), nrow = 1)
#  R2 <- rbind(R21, R22)
#  b2 <- matrix(c(0, 0), ncol = 1)
#  waldbeta2 <- wald_betas(sur.slm, R1, b1)
#  print(waldbeta2)

## ----SUR-SLM-R2-ML, echo=TRUE-------------------------------------------------
#  sur.slm.r2 <- spsurml(formula = formula_sur, listw = lwncovr,
#                        type = "slm", R = R2, b = b2,
#                        control = control, data = NCOVR.sf)
#  summary(sur.slm.r2)

## ----wald-beta3-3sls----------------------------------------------------------
#  R3 <- matrix(c(1, 0, 0, 0, 0, 0, 0,
#                -1, 0, 0, 0, 0, 0, 0), nrow = 1)
#  b3 <- matrix(0, ncol = 1)
#  waldbeta3 <- wald_betas(sur.slm.3sls, R = R3, b = b3)
#  print(waldbeta3)

## ----SUR-SLM-R-3SLS, eval=FALSE, include=FALSE--------------------------------
#  sur.slm.3sls.r <- spsur3sls(formula = formula_sur,
#                              listw = lwncovr, type = "slm",
#                              R = R3, b = b3, data = NCOVR.sf)
#  summary(sur.slm.3sls.r)

## ----walddelta1---------------------------------------------------------------
#  R1 <- matrix(c(1, -1), nrow = 1)
#  b1 <- matrix(0, ncol = 1)
#  wald_deltas(sur.slm, R = R1, b = b1)

## ----walddelta2---------------------------------------------------------------
#  wald_deltas(sur.sem, R = R1, b = b1)

## ----walddelta3---------------------------------------------------------------
#  R3 <- matrix(c(1, -1, 0, 0, 0, 0, 1, -1), nrow = 2, ncol = 4,
#               byrow = TRUE)
#  b3 <- matrix(c(0, 0), ncol = 1)
#  wald_deltas(sur.sarar, R = R3, b = b3)

## -----------------------------------------------------------------------------
#  logLik(sur.sim)
#  logLik(sur.slx)
#  logLik(sur.sarar)

## -----------------------------------------------------------------------------
#  lrtestspsur(sur.sim, sur.slx)
#  lrtestspsur(sur.slm, sur.sarar)
#  lrtestspsur(sur.slm.r2, sur.slm)

## ----impactSLM----------------------------------------------------------------
#  Wncovr <- as(lwncovr, "CsparseMatrix")
#  trWncovr <- spatialreg::trW(Wncovr, type = "MC")
#  sur.sdm.impacts <- impactspsur(sur.slm, tr = trWncovr, R = 1000)
#  class(sur.sdm.impacts[[1]])
#  summary(sur.sdm.impacts[[1]], zstats = TRUE, short = TRUE)
#  summary(sur.sdm.impacts[[2]], zstats = TRUE, short = TRUE)

## ----impacts-slm, echo=TRUE---------------------------------------------------
#  sur.slx.impacts <- impactspsur(sur.slx, listw = lwncovr)
#  print(sur.slx.impacts)

## ----data_time----------------------------------------------------------------
#  N <- nrow(NCOVR.sf)
#  Tm <- 2
#  index_name <- rep(NCOVR.sf$NAME,Tm)
#  index_indiv <- rep(1:N, Tm)
#  index_time <- rep(1:Tm, each = N)
#  HR <- c(NCOVR.sf$HR60, NCOVR.sf$HR70)
#  RD <- c(NCOVR.sf$RD60, NCOVR.sf$RD70)
#  PS <- c(NCOVR.sf$PS60, NCOVR.sf$PS70)
#  MA <- c(NCOVR.sf$MA60, NCOVR.sf$MA70)
#  DV <- c(NCOVR.sf$DV60, NCOVR.sf$DV70)
#  UE <- c(NCOVR.sf$UE60, NCOVR.sf$UE70)
#  SOUTH <- c(NCOVR.sf$SOUTH, NCOVR.sf$SOUTH)
#  NCOVR.df <- data.frame(index_name, index_indiv, index_time, HR,
#                         RD, PS, MA, DV, UE, SOUTH)

## ----SUR-SLM-TIME-ML----------------------------------------------------------
#  formula_sur_time <- HR ~ RD + PS + MA + DV + UE + SOUTH
#  sur.slm.time <- spsurtime(formula = formula_sur_time,
#                            data = NCOVR.df,
#                            listw = lwncovr,
#                            time =  NCOVR.df$index_time,
#                            type = "slm", fit_method = "ml")
#  summary(sur.slm.time)

## ----sur-slm-panel-demean-----------------------------------------------------
#  formula.panel <- HR | RD ~ PS + MA + SOUTH | UE + DV
#  sur.slm.panel.demean <- spsurml(formula = formula.panel,
#                                  listw = lwncovr, type = "slm",
#                                  method ="LU", Tm = 2,
#                                  data = NCOVR.df,
#                                  control = list(fdHess = TRUE))
#  summary(sur.slm.panel.demean)

## ----sur-slm-panel------------------------------------------------------------
#  formula.panel <- HR | RD ~ PS + MA + SOUTH | UE + DV
#  sur.slm.panel <- spsurml(formula = formula.panel,
#                           listw = lwncovr, type = "slm",
#                           method = "LU",
#                           Tm = 2,
#                           data = NCOVR.df,
#                           control = list(fdHess = TRUE))
#  summary(sur.slm.panel)

## ----DGP----------------------------------------------------------------------
#  Tm <- 1 # Number of time periods
#  G <- 3 # Number of equations
#  N <- 500 # Number of spatial elements
#  p <- 3 # Number of independent variables
#  Sigma <- matrix(0.3, ncol = G, nrow = G) # Matrix error correlation
#  diag(Sigma) <- 1
#  Betas <- c(1, 2, 3, 1, -1, 0.5, 1, -0.5, 2) # Beta coefficients
#  rho <- 0.5 # Level of sustantive spatial dependence
#  lambda <- 0.0 # spatial autocorrelation error term = 0
#  # random coordinates
#  co <- cbind(runif(N,0,1),runif(N,0,1))
#  lwdgp <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(co, k = 5, longlat = FALSE)))
#  sur.dgp <- dgp_spsur(Sigma = Sigma, Betas = Betas, rho = rho,
#                       lambda = lambda, Tm = Tm, G = G, N = N,
#                       p = p, pdfU = "nvrnorm", listw = lwdgp)

## ----DGP2---------------------------------------------------------------------
#  sdm_dgp <- spsurml(Y = sur.dgp$Y, X = sur.dgp$X, type = "sdm", G = G,
#                     Tm = Tm, N = N, p = 3, listw = lwdgp,
#                     control = list(tol = 0.01, maxit = 200,
#                                    trace = FALSE))
#  summary(sdm_dgp)

