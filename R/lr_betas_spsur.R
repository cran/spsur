#' @name lr_betas_spsur
#' @rdname lr_betas_spsur
#'
#' @title Likelihood ratio for testing homogeneity constraints on beta coefficients of the SUR equations.
#'
#' @description Function \code{\link{lr_betas_spsur}} obtains a Likelihood Ratio test, LR in what follows,
#'   with the purpose of testing if some of the \eqn{\beta} coefficients in the \emph{G} equations of the
#'   \emph{SUR} model are equal. This function has a straightforward application, especially when \eqn{G=1},
#'   to the case of testing for the existence of structural breaks in the \eqn{\beta} parameters.
#'
#'   The function can test for the homogeneity of only one coefficient, of a few of them
#'   or even the homogeneity of all the slope terms. The testing procedure implies, \emph{first},
#'   the estimation of both a constrained and a unconstrained model and, \emph{second}, the comparison
#'   of the log-likelihoods to compute the LR statistics.
#'
#' @param printmodels Logical value: prints constrained and unconstrained models. Default = \code{FALSE}.
#'   models. Default = \code{FALSE}.
#' @param time Time variable.
#' @param trace Logical value to show intermediate results during estimation. Default = \code{FALSE}.
#' @inheritParams spsurml
#'

#' @return
#'  The function estimates two variants of the same SUR model, namely, a restricted and a unrestricted
#'  version. The purpose, as indicated above, is testing homogeneity constraints between the
#'  \eqn{\beta} parameters of the different equations of the SUR.
#'  The output of \code{\link{lr_betas_spsur}} shows, in first place, the iteration sequence of the
#'  maximum-likelihood algorithm. Then appears the Likelihood  Ratio, LR, test.
#'  The output also includes the maximum-likelihood estimation of the two models.
#'
#'   \tabular{ll}{
#'   \code{statistic} \tab The Values of the LR test. \cr
#'   \code{p_val}    \tab The p-value of the LR test. \cr
#'   \code{df}    \tab Degrees of freedom. \cr
#'   \code{llik_unr}    \tab The Log-likelihood of the unrestricted model. \cr
#'   \code{llik_res} \tab The Log-likelihood of the restricted model. \cr
#'   }
#'
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Jesús Mur  \tab \email{jmur@@unizar.es} \cr
#'   }
#'
#' @references
#'   \itemize{
#'     \item Mur, J., López, F., and Herrera, M. (2010). Testing for spatial
#'       effects in seemingly unrelated regressions.
#'       \emph{Spatial Economic Analysis}, 5(4), 399-440.
#'   }
#'
#' @seealso
#' \code{\link{spsurml}}, \code{\link{spsurtime}}, \code{\link{wald_betas}}
#'
#' @examples
#' #################################################
#' ######## CROSS SECTION DATA (G>1; Tm=1) ########
#' #################################################
#'
#' #### Example 1: Spatial Phillips-Curve. Anselin (1988, p. 203)
#' rm(list = ls()) # Clean memory
#' data(spc)
#' Tformula <- WAGE83 | WAGE81 ~ UN83 + NMR83 + SMSA | UN80 + NMR80 + SMSA
#' ## H0: equal beta for SMSA in both equations.
#' R <- matrix(c(0,0,0,1,0,0,0,-1),nrow=1)
#' b <- matrix(0,ncol=1)
#' LR_SMSA <-  lr_betas_spsur(Form = Tformula, data = spc, W = Wspc,
#'                            type = "sim", R = R, b = b, trace = TRUE,
#'                            printmodels = TRUE)
#'
#' @export
lr_betas_spsur <- function(Form = NULL, data = NULL, R = NULL, b = NULL,
                          W = NULL, time = NULL, X = NULL, Y = NULL,
                          G = NULL, N = NULL, Tm = NULL, p = NULL,
                          type = "sim", printmodels = FALSE,
                          cov = FALSE, trace = FALSE) {

  if (is.null(R) || is.null(b)) stop("R and b must be specified as arguments")
  if (printmodels) cov <- TRUE

  start_fit <- proc.time()[3]
  cat("\n Fitting unrestricted model ... \n")
  if (is.null(time)){
    spcsur_unr <-spsurml(Form = Form, data = data, type = type, W = W,
                         cov = cov, control = list(tol = 0.05,
                                                   maxit = 200,
                                                   trace = trace) )
  } else {
    spcsur_unr <-spsurtime(Form = Form, data =data, type = type, W = W,
                           time = time, method = "ml", cov = cov,
                           trace = trace)
  }
  ll_unr <- spcsur_unr$llsur
  # betas_unr <-  spcsur_unr$betas
  # holg <- R %*% matrix(betas_unr,ncol=1) - b
  end_fit <- proc.time()[3]
  cat("\n Time to fit unrestricted model: ",end_fit-start_fit," seconds\n")

  start_fit <- proc.time()[3]
  cat("\n Fitting restricted model ... \n")
  if (is.null(time)){
    spcsur_res <- spsurml(Form = Form, data = data, R = R, b = b,
                         type = type, W = W, cov = cov,
                         control = list(tol = 0.05, maxit = 200,
                                        trace = trace) )
  } else {
    spcsur_res <- spsurtime(Form = Form, data = data, R = R, b = b,
                           type = type, W = W, time = time,
                           method = "ml", cov = cov, trace = trace)
  }
  ll_res <- spcsur_res$llsur
  end_fit <- proc.time()[3]
  cat("Time to fit restricted model: ",end_fit-start_fit," seconds\n")

  lr_stat <- -2*(ll_res-ll_unr)
  lr_df <- nrow(R)
  lr_pval <- pchisq(lr_stat,df=lr_df,lower.tail=FALSE)

  cat("\n LR-Test \n")
  cat("\n Log-likelihood unrestricted model: ",round(ll_unr,3))
  cat("\n Log-likelihood restricted model: ",round(ll_res,3))
  cat("\n LR statistic: ",round(lr_stat,3)," degrees of freedom: ",round(lr_df,4),
      " p-value: (",lr_pval,")")
  if (printmodels) {
    cat("\n\n UNRESTRICTED MODEL \n")
    print(summary(spcsur_unr))
    cat("\n\n RESTRICTED MODEL \n")
    print(summary(spcsur_res))
  }
  res <- list(statistic=lr_stat,
              p_val=lr_pval,
              df=lr_df,
              llik_unr=ll_unr,
              llik_res=ll_res,
              R=R,
              b=b,
              type=type)
}
