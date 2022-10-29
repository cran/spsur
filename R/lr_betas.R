#' @name lr_betas
#' @rdname lr_betas
#'
#' @title Likelihood ratio for testing homogeneity constraints on beta coefficients of the SUR equations.
#'
#' @description Function \code{\link{lr_betas}} obtains a Likelihood Ratio test, LR in what follows,
#'   with the purpose of testing if some of the \eqn{\beta} coefficients in the \emph{G} equations of the
#'   \emph{SUR} model are equal. This function has a straightforward application, especially when \eqn{G=1},
#'   to the case of testing for the existence of structural breaks in the \eqn{\beta} parameters.
#'
#'   The function can test for the homogeneity of only one coefficient, of a few of them
#'   or even the homogeneity of all the slope terms. The testing procedure implies, \emph{first},
#'   the estimation of both a constrained and a unconstrained model and, \emph{second}, the comparison
#'   of the log-likelihoods to compute the LR statistics.
#'   
#'  @usage lr_betas (obj, R, b)
#'   
#' @inheritParams wald_betas
#'
#' @return Object of \code{htest} including the LR
#'   statistic, the corresponding p-value, the degrees of
#'   freedom and the values of the sample estimates.
#'   
#' @author
#'   \tabular{ll}{
#'   Fernando Lopez  \tab \email{fernando.lopez@@upct.es} \cr
#'   Roman Minguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Jesus Mur  \tab \email{jmur@@unizar.es} \cr
#'   }
#'
#' @references
#'   \itemize{
#'     \item Mur, J., Lopez, F., and Herrera, M. (2010). Testing for spatial
#'       effects in seemingly unrelated regressions.
#'       \emph{Spatial Economic Analysis}, 5(4), 399-440.
#'       <doi:10.1080/17421772.2010.516443>
#'     \item Minguez, R., Lopez, F.A. and Mur, J.  (2022).
#'        spsur: An R Package for Dealing with Spatial 
#'        Seemingly Unrelated Regression Models. 
#'        \emph{Journal of Statistical Software}, 
#'        104(11), 1--43. <doi:10.18637/jss.v104.i11>
#'       
#'   }
#'
#' @seealso
#' \code{\link{spsurml}}, \code{\link{spsurtime}}, \code{\link{wald_betas}}
#'
#' @examples
#' 
#' ## VIP: The output of the whole set of the examples can be examined 
#' ## by executing demo(demo_lr_betas, package="spsur")
#'
#' \donttest{
#' #' #################################################
#' ######## CROSS SECTION DATA (G>1; Tm=1)  ########
#' #################################################
#'
#' #### Example 1: Spatial Phillips-Curve. Anselin (1988, p. 203)
#' rm(list = ls()) # Clean memory
#' data(spc)
#' lwspc <- spdep::mat2listw(Wspc, style = "W")
#' Tformula <- WAGE83 | WAGE81 ~ UN83 + NMR83 + SMSA | UN80 + NMR80 + SMSA
#' ### H0: equal beta for SMSA in both equations.
#' R <- matrix(c(0,0,0,1,0,0,0,-1), nrow=1)
#' b <- matrix(0, ncol=1)
#' spcsur.slm <- spsurml(formula = Tformula, data = spc, 
#'                       type = "slm", listw = lwspc)
#' summary(spcsur.slm)
#' lr_betas(spcsur.slm, R = R, b = b)
#'
#' ### Estimate restricted SUR-SLM model
#' spcsur.slmr <- spsurml(formula = Tformula, data = spc, 
#'                       type = "slm", listw = lwspc,
#'                       R = R, b = b)
#' summary(spcsur.slmr)
#' }
#' @export
lr_betas <- function(obj, R, b) {
  modelu <- obj 
  lliku <- logLik(modelu)
  cat("\n Fitting restricted model by ML...\n")
  modelr <- try( stats::update(modelu, R = R, b = b), silent = TRUE )
  if (inherits(modelr, "try-error")) {
    modelr <- spsurml(R = R, b = b, Y = modelu$Y, X = modelu$X,
                      G = modelu$G, N = modelu$N, Tm = modelu$Tm,
                      type = modelu$type, method = modelu$method,
                      listw = modelu$W, interval = modelu$interval,
                      control = list(fdHess = modelu$fdHess))
  }
  llikr <- logLik(modelr)
  statistic <- -2*(llikr-lliku)
  attr(statistic, "names") <- "Likelihood ratio"
  parameter <- abs(attr(llikr, "df") - attr(lliku, "df"))
  if (parameter < 1) 
    stop("non-positive degrees of freedom: no test possible")
  attr(parameter, "names") <- "df"
  p.value <- 1 - pchisq(abs(statistic), parameter)
  estimate <- c(llikr, lliku)
  attr(estimate, "names") <- c("Log likelihood of restricted model", 
                               "Log likelihood of unrestricted model")
  method <- "Likelihood ratio on beta parameters"
  data.name <- modelu$call[[3]]
  res <- list(statistic = statistic, parameter = parameter, 
              p.value = p.value, estimate = estimate, 
              method = method, data.name = data.name)
  class(res) <- "htest"
  res
}
