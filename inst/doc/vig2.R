## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  warning = FALSE, message = FALSE, 
  echo = TRUE, eval = FALSE)

## ----loadpackages-------------------------------------------------------------
#  library(spsur)
#  library(spdep)
#  library(spatialreg)
#  library(sf)

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
#  listw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(co, k = 10,longlat = TRUE)))

## ----ols_60sr-----------------------------------------------------------------
#  formula_60 <- HR60 ~ RD60 + PS60 + MA60 + DV60 + UE60 + SOUTH
#  lm_60 <- lm(formula = formula_60, data = NCOVR.sf)
#  summary(lm_60)

## ----sur-sim------------------------------------------------------------------
#  ols.spsur <- spsurml(formula = formula_60, type = "sim",
#                       data = NCOVR.sf)
#  summary(ols.spsur)

## -----------------------------------------------------------------------------
#  lmtest.spdep <- spdep::lm.LMtests(lm_60, listw, test = "all")
#  print(lmtest.spdep)

## ----test_SUR-SIM-------------------------------------------------------------
#  lmtest.spsur <- lmtestspsur(formula = formula_60, listw = listw,
#                              data = NCOVR.sf)
#  print(lmtest.spsur)

## ----SUR-SLM------------------------------------------------------------------
#  slm.spsur <- spsur::spsurml(formula = formula_60, type = "slm",
#                              listw = listw, data = NCOVR.sf)
#  summary(slm.spsur)

## ----SLM----------------------------------------------------------------------
#  slm.spatialreg <- spatialreg::lagsarlm(formula = formula_60,
#                                         listw = listw, type = "lag",
#                                         data = NCOVR.sf)
#  summary(slm.spatialreg)

## ----logLik-------------------------------------------------------------------
#  logLik(slm.spsur)

## ----LR-test------------------------------------------------------------------
#  lrtestspsur(ols.spsur, slm.spsur)

## ----IV-spsur-----------------------------------------------------------------
#  slm.3sls.spsur <- spsur3sls(formula = formula_60, type = "slm",
#                              listw = listw, data = NCOVR.sf)
#  summary(slm.3sls.spsur)

## ----IV-spatialreg, collapse=FALSE--------------------------------------------
#  slm.3sls.spatialreg <- spatialreg::stsls(formula = formula_60, listw = listw,
#                               data = NCOVR.sf)
#  summary(slm.3sls.spatialreg)

## ----SEM----------------------------------------------------------------------
#  sem.spatialreg <- spatialreg::errorsarlm(formula = formula_60,
#                               listw = listw, data = NCOVR.sf)
#  summary(sem.spatialreg)

## ----sem spsur----------------------------------------------------------------
#  sem.spsur <- spsurml(formula = formula_60, type = "sem",
#                       listw = listw, data = NCOVR.sf)
#  summary(sem.spsur)

## ----LR-test-OLS-SEM----------------------------------------------------------
#  lrtestspsur(ols.spsur, sem.spsur)

## -----------------------------------------------------------------------------
#  print(sem.spsur$LMM)

## ----SUR-SARAR----------------------------------------------------------------
#  sarar.spsur <- spsurml(formula = formula_60, listw = listw,
#                         type ="sarar",data = NCOVR.sf)
#  summary(sarar.spsur)

## ----SARAR--------------------------------------------------------------------
#  sarar.spatialreg <- spatialreg::sacsarlm(formula = formula_60,
#                               listw = listw, data = NCOVR.sf)
#  summary(sarar.spatialreg)

## -----------------------------------------------------------------------------
#  lrtestspsur(slm.spsur,sarar.spsur)
#  lrtestspsur(sem.spsur,sarar.spsur)

