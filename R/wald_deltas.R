#' @name wald_deltas
#' @rdname wald_deltas
#'
#' @title Wald tests for spatial parameters coefficients.
#'
#' @description
#'  Function \code{\link{wald_deltas}} obtains Wald tests for linear 
#'  restrictions on the spatial coefficients of a SUR model that has been 
#'  estimated previously through the function \code{\link{spsurml}}. The 
#'  restrictions can affect to coefficients of the same equation
#'  (i.e., \eqn{\lambda_{g}=\rho_{g} forall g}) or can involve coefficients 
#'  from different equations (i.e., \eqn{\lambda_{g}=\lambda_{h}}). The 
#'  function has great flexibility in this respect. Note that 
#'  \code{\link{wald_deltas}} only works in a maximum-likelihood framework.
#'
#'  In order to work with \code{\link{wald_betas}}, the model on which the 
#'  linear restrictions are to be tested needs to exists as an \emph{spsur} 
#'  object. Using the information contained in the object, 
#'  \code{\link{wald_deltas}} obtains the corresponding Wald statistic for 
#'  the null hypotheses specified by the user through the \emph{R} row vector 
#'  and \emph{b} column vector discussed, used also in \code{\link{spsurml}}. 
#'  The function shows the  resulting Wald test statistics and their 
#'  corresponding p-values.
#'  
#' @usage wald_deltas (obj , R , b)
#'
#' @param obj An \code{spsur} object created by \code{\link{spsurml}},
#'  \code{\link{spsur3sls}} or \code{\link{spsurtime}}.
#' @param R A row vector of order \emph{(1xGr)} or \emph{(1x2Gr)} showing 
#'  the set of \emph{r} linear constraints on the  spatial parameters. The 
#'  last case is reserved to "sarar" models where there appear
#'  \emph{G} parameters \eqn{\lambda_{g}} and \emph{G} parameters 
#'  \eqn{\rho_{g}}, \emph{2G} spatial in total. The \emph{first} 
#'  restriction appears in the first \emph{G} terms in \emph{R} 
#'  (\emph{2G} for the "sarar" case),   the second restriction 
#'  in the next \emph{G} terms (\emph{2G} for the
#'  "sarar" case) and so on. 
#' @param  b A column vector of order \emph{(rx1)} with the values 
#'  of the linear restrictions on the \eqn{\beta} parameters. 
#'
#' @return Object of \code{htest} including the Wald
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
#' @seealso
#'  \code{\link{spsurml}}, \code{\link{spsur3sls}}
#' @examples
#'
#' #################################################
#' ######## CROSS SECTION DATA (G>1; Tm=1) ########
#' #################################################
#' rm(list = ls()) # Clean memory
#' data(spc, package = "spsur")
#' lwspc <- spdep::mat2listw(Wspc, style = "W")
#' Tformula <- WAGE83 | WAGE81 ~ UN83 + NMR83 + SMSA | UN80 + NMR80 + SMSA
#'
#' #################################
#' ## Estimate SUR-SLM model
#' spcsur.slm <-spsurml(formula = Tformula, data = spc, 
#'                        type = "slm", listw = lwspc)
#' summary(spcsur.slm)
#' ## H_0: equality of the lambda parameters of both equations.
#' R1 <- matrix(c(1,-1), nrow=1)
#' b1 <- matrix(0, ncol=1)
#' wald_deltas(spcsur.slm, R = R1, b = b1)
#' @export
wald_deltas <- function(obj , R , b){
  z <- obj 
  deltas <- Matrix::Matrix(matrix(z$deltas, ncol = 1))
  rownames(deltas) <- names(z$deltas)
  cov_deltas <- Matrix::Matrix(z$resvar[rownames(deltas),
                                        rownames(deltas)])
  R <- Matrix::Matrix(R)
  b <- Matrix::Matrix(matrix(b,ncol=1))
  holg <- (R %*% deltas) - b
  parameter <- nrow(as.matrix(R))
  attr(parameter, "names") <- "df"
  statistic <- as.numeric(Matrix::t(holg) %*%
                 Matrix::solve(R %*% cov_deltas %*% 
                                 Matrix::t(R),holg)  )
  attr(statistic, "names") <- "Wald test"
  method <- paste("Wald test on spatial delta parameters")
  p.value <- pchisq(statistic, df = parameter, 
                    lower.tail = FALSE)
  estimate <- as.numeric(deltas)
  names(estimate) <- rownames(deltas)
  data.name <- z$call[[3]]
  res <- list(statistic = statistic, parameter = parameter, 
              p.value = p.value, # estimate = estimate, 
              method = method, data.name = data.name)
  class(res) <- "htest"
  res  
}
