% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spsurml.R
\name{spsurml}
\alias{spsurml}
\title{Maximum likelihood estimation of spatial SUR model.}
\usage{
spsurml(formula = NULL, data = NULL, na.action,
               listw = NULL, type = "sim", Durbin = NULL,
               method = "eigen", zero.policy = NULL, interval = NULL,
               trs = NULL, R = NULL, b = NULL, X = NULL, Y = NULL, 
               G = NULL, N = NULL, Tm = NULL,p = NULL, 
               control = list() )
}
\arguments{
\item{formula}{An object type \code{\link[Formula]{Formula}} 
similar to objects created with the package \pkg{Formula} 
describing the equations to be estimated in the model. 
This model may contain several responses (explained 
variables) and a varying number of regressors in each equation.}

\item{data}{An object of class data.frame or a matrix.}

\item{na.action}{A function (default \code{options("na.action")}),
can also be \code{na.omit} or \code{na.exclude} with consequences 
 for residuals and fitted values. It may be necessary to set 
 \code{zero.policy} to \code{TRUE} because this subsetting may 
 create no-neighbour observations.}

\item{listw}{A \code{listw} object created for example by 
\code{\link[spdep]{nb2listw}} from \pkg{spatialreg} package; if 
\code{\link[spdep]{nb2listw}} not given, set to 
the same spatial weights as the \code{listw} argument. It can
also be a spatial weighting matrix of order \emph{(NxN)} instead of
a \code{listw} object. Default = \code{NULL}.}

\item{type}{Type of spatial model specification: "sim",
"slx", "slm", "sem", "sdm", 
"sdem", "sarar" or "gnm". Default = "sim".}

\item{Durbin}{If a formula object and model is type "sdm", "sdem" 
or "slx" the subset of explanatory variables to lag for each equation.}

\item{method}{Similar to the corresponding parameter of 
\code{\link[spatialreg]{lagsarlm}} function in \pkg{spatialreg} package. 
"eigen" (default) - the Jacobian is computed as the product of 
(1 - rho*eigenvalue) using \code{\link[spatialreg]{eigenw}}, and 
"spam" or "Matrix_J" for strictly symmetric weights lists of 
styles "B" and "C", or made symmetric by similarity 
(Ord, 1975, Appendix C) if possible for styles "W" and "S", 
using code from the spam or Matrix packages to calculate the 
determinant; "Matrix" and "spam_update" provide updating Cholesky 
decomposition methods; "LU" provides an alternative sparse matrix 
decomposition approach. In addition, there are "Chebyshev" and 
Monte Carlo "MC" approximate log-determinant methods; 
the Smirnov/Anselin (2009) trace approximation is available 
as "moments". Three methods: "SE_classic", "SE_whichMin", 
and "SE_interp" are provided experimentally, the first to 
attempt to emulate the behaviour of Spatial Econometrics 
toolbox ML fitting functions. All use grids of log determinant 
values, and the latter two attempt to ameliorate some features 
of "SE_classic".}

\item{zero.policy}{Similar to the corresponding parameter of 
\code{\link[spatialreg]{lagsarlm}} function in \pkg{spatialreg} package. 
If \code{TRUE} assign zero to the lagged value of zones without 
neighbours, if \code{FALSE} assign \code{NA} - causing 
\code{spsurml()} to terminate with an error. Default = \code{NULL}.}

\item{interval}{Search interval for autoregressive parameter.
Default = \code{NULL}.}

\item{trs}{Similar to the corresponding parameter of 
\code{\link[spatialreg]{lagsarlm}} function in \pkg{spatialreg} package.
Default \code{NULL}, if given, a vector of powered spatial weights 
matrix traces output by \code{\link[spdep]{trW}}.}

\item{R}{A row vector of order \emph{(1xpr)} with  the set of 
\emph{r} linear constraints on the \emph{beta} parameters. The 
\emph{first} restriction appears in the first \emph{p} terms,
the second restriction in the next \emph{p} terms and so on. 
Default = \code{NULL}.}

\item{b}{A column vector of order \emph{(rx1)} with the values of 
the linear restrictions on the \emph{beta} parameters. 
Default = \code{NULL}.}

\item{X}{A data matrix of order \emph{(NTmGxp)} with the observations
of the regressors. The number of covariates in the SUR model is 
\emph{p} = \eqn{sum(p_{g})} where \emph{\eqn{p_{g}}} is the number 
of regressors (including the intercept) in the g-th equation, 
\emph{g = 1,...,G}). The specification of "X" is only 
necessary if not available a \code{\link[Formula]{Formula}} and a 
data frame. Default = \code{NULL}.}

\item{Y}{A column vector of order \emph{(NTmGx1)}, with the 
observations of the explained variables. The ordering of the data 
must be (first) equation, (second) time dimension and (third) 
cross-sectional/spatial units. The specification of \emph{Y} is 
only necessary if not available a \code{\link[Formula]{Formula}}
and a data frame. Default = \code{NULL}.}

\item{G}{Number of equations.}

\item{N}{Number of cross-section or spatial units}

\item{Tm}{Number of time periods.}

\item{p}{Number of regressors by equation, including the intercept. 
\emph{p} can be a row vector of order \emph{(1xG)}, if the number 
 of regressors is not the same for all the equations, or a scalar, 
 if the \emph{G} equations have the same number of regressors. The 
specification of \emph{p} is only necessary if not available a 
\code{\link[Formula]{Formula}} and a data frame.}

\item{control}{list of additional arguments.}
}
\value{
Object of \code{spsur} class with the output of the  
 maximum-likelihood estimation of the specified spatial SUR model. 
 A list with:
  \tabular{ll}{
    \code{call} \tab Matched call. \cr
    \code{type} \tab  Type of model specified. \cr
    \code{method} \tab Value of \code{method} argument to compute the 
      Jacobian \cr
    \code{Durbin} \tab Value of \code{Durbin} argument. \cr     
    \code{coefficients} \tab Estimated coefficients for the regressors. \cr
    \code{deltas} \tab Estimated spatial coefficients. \cr
    \code{rest.se} \tab Estimated standard errors for the 
      estimates of \emph{beta}. \cr
    \code{deltas.se} \tab Estimated standard errors for the estimates of 
      the spatial coefficients (\code{deltas}). \cr
    \code{resvar} \tab Estimated covariance matrix for the estimates of 
      \emph{beta's} and spatial coefficients (\code{deltas}).\cr
    \code{LL} \tab Value of the likelihood function at the 
      maximum-likelihood estimates. \cr
    \code{R2} \tab Coefficient of determination for each equation, 
      obtained as the squared of the correlation coefficient between the 
      corresponding explained variable and fitted values. \cr
    \code{R2 pooled} \tab \emph{Global} coefficient of determination 
      obtained for the set of the \emph{G} equations. 
      It is computed in the same way than uniequational \code{R2} but 
      joining the dependent variable and fitted values in single vectors 
      instead of one vector for each equation. \cr
    \code{Sigma} \tab Estimated covariance matrix for the residuals of 
      the \emph{G} equations. \cr
    \code{fdHess} \tab Logical value of \code{fdHess} argument when 
      computing numerical covariances.  \cr  
    \code{residuals} \tab Residuals of the model. \cr
    \code{df.residuals} \tab Degrees of freedom for the residuals. \cr
    \code{fitted.values} \tab Estimated values for the dependent 
      variables. \cr
    \code{BP} \tab Value of the Breusch-Pagan statistic to test the 
      null hypothesis of diagonality among the errors of the \emph{G} 
      equations. \cr
    \code{LMM} \tab Marginal Lagrange Multipliers, 
      LM(\eqn{\rho}|\eqn{\lambda}) and
      LM(\eqn{\lambda}|\eqn{\rho}), to test for omitted spatial effects 
      in the specification. \cr
    \code{G} \tab Number of equations. \cr
    \code{N} \tab Number of cross-sections or spatial units. \cr
    \code{Tm} \tab Number of time periods. \cr
    \code{p} \tab Number of regressors by equation (including intercepts). \cr
    \code{Y} \tab If \emph{data} is \emph{NULL}, vector \emph{Y} of the explained variables of the 
      SUR model. \cr
    \code{X} \tab If \emph{data} is \emph{NULL}, matrix \emph{X} of the regressors of the SUR model. \cr
    \code{W} \tab Spatial weighting matrix. \cr
    \code{zero.policy} \tab Logical value of \code{zero.policy} . \cr
    \code{interval} \tab	Search interval for spatial parameter. \cr
    \code{listw_style} \tab	Style of neighborhood matrix \code{W}. \cr
    \code{trs} \tab Either \code{NULL} or vector of powered spatial weights 
      matrix traces output by \code{trW}. \cr
    \code{insert} \tab Logical value to check if \code{is.null(trs)}. \cr 
 }
}
\description{
This function estimates spatial SUR models using 
  maximum-likelihood methods.The number of equations, time periods 
  and cross-sectional units is not restricted.The user can choose 
  between different spatial specifications as described below.
  The estimation procedure allows for the introduction of linear 
  restrictions on the \eqn{\beta} parameters associated to the 
  regressors.
}
\details{
The list of (spatial) models that can be estimated with the \emph{spsurml} function are:
 \itemize{
    \item "sim": SUR model with no spatial effects
      \deqn{ y_{tg} = X_{tg} \beta_{g} + \epsilon_{tg} }
    \item "slx": SUR model with spatial lags of the regressors
      \deqn{ y_{tg} = X_{tg} \beta_{g} + WX_{tg} \theta_{g} + \epsilon_{tg} }
    \item "slm": SUR model with spatial lags of the explained variables
      \deqn{y_{tg} = \rho_{g} Wy_{tg} + X_{tg} \beta_{g} + \epsilon_{tg} }
    \item "sem": SUR model with spatial errors
      \deqn{ y_{tg} = X_{tg} \beta_{g} + u_{tg} }
      \deqn{ u_{tg} = \lambda_{g} Wu_{tg} + \epsilon_{tg} }
    \item "sdm": SUR model of the Spatial Durbin type
      \deqn{ y_{tg} = \rho_{g} Wy_{tg} + X_{tt} \beta_{g} + WX_{tg} \theta_{g} + \epsilon_{tg} }
    \item "sdem": SUR model with spatial lags of the regressors and spatial errors
      \deqn{ y_{tg} = X_{tg} \beta_{g} + WX_{tg} \theta_{g} + u_{tg} }
      \deqn{ u_{tg} = \lambda_{g} W u_{tg} + \epsilon_{tg} }
    \item "sarar": SUR model with spatial lags of the explained variables and spatial
      errors
      \deqn{ y_{tg} = \rho_{g} Wy_{tg} + X_{tg} \beta_{g} + u_{tg} }
      \deqn{ u_{tg} = \lambda_{g} W u_{tg} + \epsilon_{tg} }
    \item "gnm": SUR model with spatial lags of the explained variables, 
      regressors and spatial errors
      \deqn{ y_{tg} = \rho_{g} Wy_{tg} + X_{tg} \beta_{g} + 
                        WX_{tg} \theta_{g} + u_{tg} }
      \deqn{ u_{tg} = \lambda_{g} W u_{tg} + \epsilon_{tg} }
  }
}
\section{Control arguments}{

  \tabular{ll}{
    \code{tol} \tab Numerical value for the tolerance for the estimation 
      algorithm until convergence. Default = 1e-3. \cr
    \code{maxit} \tab Maximum number of iterations until convergence; 
      it must be an integer value. Default = 200. \cr
    \code{trace} \tab A logical value to show intermediate results during 
      the estimation process. Default = \code{TRUE}. \cr
    \code{fdHess} \tab Compute variance-covariance matrix using the numerical 
      hessian. Suited for large samples. Default = \code{FALSE} \cr
    \code{Imult} \tab default 2; used for preparing the Cholesky 
      decompositions for updating in the Jacobian function \cr
    \code{super} \tab  if \code{NULL} (default), set to \code{FALSE} to use 
      a simplicial decomposition for the sparse Cholesky decomposition and 
      method "Matrix_J", set to as.logical(NA) for method "Matrix", if 
      \code{TRUE}, use a supernodal decomposition \cr
    \code{cheb_q} \tab default 5; highest power of the approximating 
      polynomial for the Chebyshev approximation \cr
    \code{MC_p} \tab default 16; number of random variates \cr
    \code{MC_m} \tab default 30; number of products of random variates 
      matrix and spatial weights matrix \cr
    \code{spamPivot} \tab  default "MMD", alternative "RCM" \cr
    \code{in_coef} \tab default 0.1, coefficient value for initial Cholesky 
      decomposition in "spam_update" \cr
    \code{type} \tab default "MC", used with method "moments"; alternatives 
      "mult" and "moments", for use if trs is missing \cr 
    \code{correct} \tab default \code{TRUE}, used with method "moments" to 
      compute the Smirnov/Anselin correction term \cr
    \code{trunc} \tab default \code{TRUE}, used with method "moments" to 
      truncate the Smirnov/Anselin correction term \cr
    \code{SE_method} \tab default "LU", may be "MC" \cr
    \code{nrho} \tab default 200, as in SE toolbox; the size of the first 
      stage lndet grid; it may be reduced to for example 40 \cr
    \code{interpn} \tab default 2000, as in SE toolbox; the size of the 
      second stage lndet grid \cr
    \code{SElndet} \tab default \code{NULL}, may be used to pass a 
      pre-computed SE toolbox style matrix of coefficients and their lndet 
      values to the "SE_classic" and "SE_whichMin" methods \cr
    \code{LU_order} \tab default \code{FALSE}; used in "LU_prepermutate", 
      note warnings given for lu method \cr
    \code{pre_eig} \tab default \code{NULL}; may be used to pass a 
      pre-computed vector of eigenvalues \cr
}
}

\examples{

#################################################
######## CROSS SECTION DATA (G>1; Tm=1) ########
#################################################

#### Example 1: Spatial Phillips-Curve. Anselin (1988, p. 203)
rm(list = ls()) # Clean memory
data(spc)
Tformula <- WAGE83 | WAGE81 ~ UN83 + NMR83 + SMSA | UN80 + NMR80 + SMSA
spcsur.sim <- spsurml(formula = Tformula, data = spc, type = "sim")
summary(spcsur.sim)
# All the coefficients in a single table.
print(spcsur.sim)
# Plot of the coefficients of each equation in different graphs
plot(spcsur.sim) 

## A SUR-SLX model 
## (listw argument can be either a matrix or a listw object )
spcsur.slx <- spsurml(formula = Tformula, data = spc, type = "slx", 
  listw = Wspc, Durbin = TRUE)
summary(spcsur.slx)
# All the coefficients in a single table.
print(spcsur.slx)
# Plot of the coefficients in a single graph
if (require(gridExtra)) {
  pl <- plot(spcsur.slx, viewplot = FALSE)
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], 
                        nrow = 2)
} 

## VIP: The output of the whole set of the examples can be examined 
## by executing demo(demo_spsurml, package="spsur")
  
\donttest{
### A SUR-SLM model
spcsur.slm <- spsurml(formula = Tformula, data = spc, type = "slm", 
                     listw = Wspc)
summary(spcsur.slm)
if (require(gridExtra)) {
  pl <- plot(spcsur.slm, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], 
               pl$pldeltas, nrow = 3)
}


### A SUR-SDM model
spcsur.sdm <- spsurml(formula = Tformula, data = spc, type = "sdm", 
                     listw = Wspc)
summary(spcsur.sdm)
print(spcsur.sdm)
if (require(gridExtra)) {
  pl <- plot(spcsur.sdm, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], 
               pl$pldeltas, nrow = 3)
}

## A SUR-SDM model with different spatial lags in each equation
TformulaD <- ~ UN83 + NMR83 + SMSA | UN80
spcsur.sdm2 <- spsurml(formula = Tformula, data = spc, type = "sdm", 
                      listw = Wspc, Durbin = TformulaD)
summary(spcsur.sdm2)
if (require(gridExtra)) {
  pl <- plot(spcsur.sdm2, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], 
               pl$pldeltas, nrow = 3)
}
}

##################################################
#########  CLASSIC PANEL DATA G=1; Tm>1   ########
##################################################
#
##### Example 2: Homicides + Socio-Economics (1960-90)
## Homicides and selected socio-economic characteristics for continental
## U.S. counties.
## Data for four decennial census years: 1960, 1970, 1980 and 1990.
## \url{https://geodacenter.github.io/data-and-lab/ncovr/}

\donttest{
### It usually requires 1-2 minutes maximum...
rm(list = ls()) # Clean memory
### Read NCOVR.sf object
data(NCOVR, package = "spsur")
nbncovr <- spdep::poly2nb(NCOVR.sf, queen = TRUE)
### Some regions with no links...
lwncovr <- spdep::nb2listw(nbncovr, style = "W", zero.policy = TRUE)
Tformula <- HR80  | HR90 ~ PS80 + UE80 | PS90 + UE90
### A SUR-SIM model
NCOVRSUR.sim <- spsurml(formula = Tformula, data = NCOVR.sf, type = "sim")
summary(NCOVRSUR.sim)
if (require(gridExtra)) {
  pl <- plot(NCOVRSUR.sim, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], nrow = 3)
}
### A SUR-SLX model
NCOVRSUR.slx <- spsurml(formula = Tformula, data = NCOVR.sf, type = "slx", 
                       listw = lwncovr, zero.policy = TRUE)
print(NCOVRSUR.slx)
if (require(gridExtra)) {
  pl <- plot(NCOVRSUR.slx, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], nrow = 2)
}

### A SUR-SLM model
### method = "Matrix" (Cholesky) instead of "eigen"
### (fdHess = TRUE to compute numerical covariances )
NCOVRSUR.slm <- spsurml(formula = Tformula, data = NCOVR.sf, 
                       type = "slm", listw = lwncovr, method = "Matrix", 
                       zero.policy = TRUE, control = list(fdHess = TRUE))
summary(NCOVRSUR.slm)

if (require(gridExtra)) {
  pl <- plot(NCOVRSUR.slm, viewplot = FALSE) 
  grid.arrange(pl$lplbetas[[1]], pl$lplbetas[[2]], 
               pl$pldeltas, nrow = 3)
}
# LR test for nested models
anova(NCOVRSUR.sim, NCOVRSUR.slm)
}
}
\references{
\itemize{   
    \item Anselin, L. (1988). \emph{Spatial econometrics: methods and models.} 
      Dordrecht: Kluwer
    \item Bivand, R.S. and  Piras G. (2015). Comparing Implementations of 
     Estimation Methods for Spatial Econometrics. \emph{Journal of 
     Statistical Software}, 63(18), 1-36. 
     <doi:	10.18637/jss.v063.i18>
    \item Bivand, R. S., Hauke, J., and Kossowski, T. (2013). 
      Computing the Jacobian in Gaussian spatial autoregressive models: An 
      illustrated comparison of available methods. \emph{ Geographical 
      Analysis}, 45(2), 150-179. <doi:10.1111/gean.12008>
    \item Breusch T., Pagan A. (1980). The Lagrange multiplier test and its
       applications to model specification in econometrics. 
       \emph{Rev Econ Stud} 47: 239-254
    \item Cliff, A.D. and Ord, J.K. (1981). \emph{Spatial processes: Models 
      and applications}, Pion. 
    \item LeSage J and Pace, R.K. (2009). \emph{Introduction to Spatial 
      Econometrics.} CRC Press, Boca Raton.
     \item Lopez, F.A., Mur, J., and Angulo, A. (2014). Spatial model
       selection strategies in a SUR framework. The case of regional
       productivity in EU. \emph{Annals of Regional Science}, 53(1), 197-220.
       <doi:10.1007/s00168-014-0624-2>
   \item Minguez, R., Lopez, F.A. and Mur, J.  (2022).
      spsur: An R Package for Dealing with Spatial 
      Seemingly Unrelated Regression Models. 
      \emph{Journal of Statistical Software}, 
      104(11), 1--43.
      <doi:10.18637/jss.v104.i11>
    \item Mur, J., Lopez, F., and Herrera, M. (2010). Testing for spatial
      effects in seemingly unrelated regressions.
      \emph{Spatial Economic Analysis}, 5(4), 399-440.
      <doi:10.1080/17421772.2010.516443>
    \item Ord, J.K. (1975). Estimation methods for models of spatial 
      interaction, \emph{Journal of the American Statistical Association}, 
      70, 120-126. 
  }
}
\seealso{
\code{\link{spsur3sls}}, \code{\link[spatialreg]{lagsarlm}}, 
\code{\link{lmtestspsur}}, \code{\link{wald_betas}}, 
\code{\link{lr_betas}}
}
\author{
\tabular{ll}{
  Fernando Lopez  \tab \email{fernando.lopez@upct.es} \cr
  Roman Minguez  \tab \email{roman.minguez@uclm.es} \cr
  Jesus Mur  \tab \email{jmur@unizar.es} \cr
  }
}
