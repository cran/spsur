#' @name spsurtime
#' @rdname spsurtime
#'
#' @title Estimation of SUR models for simple spatial panels (G=1).
#'
#'
#'
#' @description This function estimates SUR models for simple spatial panel datasets.
#'  \code{\link{spsurtime}} is restricted, specifically, to cases where there is only one equation, \emph{G=1},
#'   and a varying number of spatial units, \emph{N}, and time periods, \emph{Tm}. The SUR structure appears
#'   in form of serial dependence among the error terms corresponding to the same spatial unit.
#'   Note that it is assumed that all spatial units share a common pattern of serial dependence.
#'
#'   The user can choose between different types of spatial specifications, as described below,
#'   and the estimation algorithms allow for the introduction of linear restrictions on the \eqn{\beta} parameters
#'   associated to the regressors. The spatial panels with SUR structure can be estimated by maximum-likelihood
#'   methods or three-stages least squares procedures, using spatial instrumental variables.
#'
#' @inheritParams spsurml
#' @param time Time variable.
#' @param method Method of estimation for the spatial panel SUR model, either \emph{ml} or \emph{3sls}.
#' Default = \emph{ml}.
#' @param maxlagW Maximum spatial lag order of the regressors employed to produce spatial
#'  instruments for the spatial lags of the explained variables. Default = 2. Note that in case of
#'  \emph{type="sdm"}, the default value for maxlagW is set to 3 because the first lag of the
#'  regressors, \eqn{WX_{tg}}, can not be used as spatial instruments.
#' @param demean  Logical value to allow for the demeaning of panel data, sustracting the individual mean to
#'  each spatial or cross-sectional unit. Default = \code{FALSE}.
#' @param trace Logical value to show intermediate results.  Default = \code{TRUE}.
#'
#' @details
#'  Function \code{\link{spsurtime}} only admits a formula, created with \code{\link[Formula]{Formula}}
#'  and a dataset of class data.frame or matrix. That is, the data cannot be uploaded using data matrices
#'  \eqn{Y} and \eqn{X} provided for other functions in this package. \cr
#'  The argument \emph{time} selects the variable, in the data.frame, associated to the time
#'  dimension in the panel dataset. Then \code{\link{spsurtime}} operates as in Anselin (1988), that is,
#'  each cross-section is treated as if it were an equation in a SUR model, which now has \emph{Tm}
#'  'equations' and \emph{N} individuals. \cr
#'  The SUR structure appears because there is serial dependence in the errors of each individual in
#'  the panel. The serial dependence in the errors is not parameterized, but estimated non-parametrically
#'  in the \eqn{Sigma} covariance matrix returned by the function. An important constraint to mention
#'  is that the serial dependence assumed to be the same for all individuals in the sample. Serial dependence
#'  among individuals is excluded from Anselin approach.
#'
#'
#' @return
#'  Output of the maximum-likelihood or three-stages least-squares estimation of
#'  the spatial panel SUR model. The final list depends of the estimation method but, typically, you
#'  will find information about:
#'   \tabular{ll}{
#'     \code{call} \tab Matched call. \cr
#'     \code{type} \tab  Type of model specified. \cr
#'     \code{betas} \tab Estimated coefficients for the regressors. \cr
#'     \code{deltas} \tab Estimated spatial coefficients. \cr
#'     \code{se_betas} \tab Estimated standard errors for the estimates of \emph{beta}. \cr
#'     \code{se_deltas} \tab Estimated standard errors for the estimates of the spatial coefficients. \cr
#'     \code{cov} \tab Estimated covariance matrix for the estimates of \emph{beta's} and spatial coefficients.\cr
#'     \code{llsur} \tab Value of the likelihood function at maximum-likelihood estimation.Only if \emph{method} = \emph{ml}. \cr
#'     \code{R2} \tab Global coefficient of determination for the Tm equations, obtained as the squared of the correlation
#'      coefficient between the corresponding explained variable and its estimates. \cr
#'     \code{Sigma} \tab Estimated covariance matrix for the residuals of the \emph{G} equations. \cr
#'     \code{Sigma_corr} \tab stimated correlation matrix for the residuals of the \emph{G} equations. \cr
#'     \code{Sigma_inv} \tab Inverse of \code{Sigma}, the \emph{(GxG)} covariance matrix of
#'      the residuals of the SUR model. \cr
#'     \code{residuals} \tab Residuals of the model. \cr
#'     \code{df.residuals} \tab Degrees of freedom for the residuals. \cr
#'     \code{fitted.values} \tab Estimated values for the dependent variables. \cr
#'     \code{BP} \tab Value of the Breusch-Pagan statistic to test the null hypothesis
#'      of diagonality among the errors of the \emph{G} equations. Only if \emph{method} = \emph{ml}. \cr
#'     \code{LMM} \tab Marginal Lagrange Multipliers, LM(\eqn{\rho}|\eqn{\lambda}) and
#'       LM(\eqn{\lambda}|\eqn{\rho}), to test for omitted spatial effects in the specification.
#'       Only if \emph{method} = \emph{ml}. \cr
#'     \code{N} \tab Number of cross-sections or spatial units. \cr
#'     \code{Tm} \tab Number of time periods. \cr
#'     \code{demean} \tab Logical value used for demeaning. \cr
#'     \code{W} \tab Spatial weighting matrix. \cr
#'  }
#'
#'
#'
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
#'     \item Anselin, L. (1988). Spatial econometrics: methods and models. Dordrecht,
#'      Kluwer Academic Publishers.
#'     \item López, F.A., Mur, J., and Angulo, A. (2014). Spatial model
#'        selection strategies in a SUR framework. The case of regional
#'        productivity in EU. \emph{Annals of Regional Science}, 53(1), 197-220.
#'     \item López, F.A., Martínez-Ortiz, P.J., & Cegarra-Navarro, J.G. (2017).
#'        Spatial spillovers in public expenditure on a municipal level in
#'        Spain. \emph{Annals of Regional Science}, 58(1), 39-65.
#'     \item Mur, J., López, F., and Herrera, M. (2010). Testing for spatial
#'       effects in seemingly unrelated regressions. \emph{Spatial Economic Analysis}, 5(4), 399-440.
#'   }
#'
#' @seealso
#' \code{\link{spsurml}}, \code{\link{spsur3sls}}, \code{\link{wald_betas}},
#' \code{\link{lmtestspsur}}, \code{\link{lrtestspsur}}
#'
#' @examples
#'
#' ####################################
#' ######## PANEL DATA (G=1; Tm>1) ###
#' ####################################
#'
#' ## Example 1:
#' rm(list = ls()) # Clean memory
#' N <- nrow(spc)
#' Tm <- 2
#' index_time <- rep(1:Tm, each = N)
#' index_indiv <- rep(1:N, Tm)
#' WAGE <- c(spc$WAGE83, spc$WAGE81)
#' UN <- c(spc$UN83, spc$UN80)
#' NMR <- c(spc$NMR83, spc$NMR80)
#' SMSA <- c(spc$SMSA, spc$SMSA)
#' pspc <- data.frame(index_indiv,index_time,WAGE,UN,NMR,SMSA)
#' form_pspc <- WAGE ~ UN + NMR + SMSA
#'
#' # SLM by 3SLS
#' pspc_slm <- spsurtime(Form = form_pspc, data = pspc, W = Wspc,
#'                       time = pspc$index_time, type = "slm", method = "3sls")
#'                       summary(pspc_slm)
#' @export
spsurtime <- function(Form, data, time, type = "sim",  method = "ml",
                      maxlagW = 2, W = NULL, cov = TRUE,
                      demean = FALSE, trace = TRUE,
                      R = NULL, b = NULL) {

  #check for row-standardization of W
  if (!is.null(W)){
    if (class(W) != "matrix") W <- as.matrix(W)
    rsumW <- rowSums(W)
    rsumW[rsumW == 0] <- 1
    nW <- dim(W)[1]
    W <- W / matrix(rep(rsumW, each = nW),
                    nrow = nW, ncol = nW, byrow = TRUE)
    W <- Matrix::Matrix(W)
  }

  if (class(time) != "factor") time <- as.factor(time)
  time <- droplevels(time)
  if (length(time) != nrow(data)) stop("time must have same length than the
                                       number of rows in data")
  mt <- terms(Form)
  G <- length(levels(time))
  Ylist <- vector("list",G)
  Xlist <- vector("list",G)
  p <- NULL
  namesX <- NULL
  levels_time <- levels(time)
  for (i in 1:G) {
    data_i <- model.frame(mt,data=data[time==levels_time[i],])
    Ylist[[i]] <- data_i[,1]
    Xlist[[i]] <- model.matrix(mt,data=data[time==levels_time[i],])
    p <- c(p,ncol(Xlist[[i]]))
    namesX <- c(namesX,paste(colnames(Xlist[[i]]),i,sep="_"))
  }
  Y <- matrix(unlist(Ylist),ncol=1)
  X <- as.matrix(Matrix::bdiag(Xlist))
  colnames(X) <- namesX
  N <- length(Ylist[[1]]); Tm <- 1

  if (demean){ # demeaning for pure panel data
    # First reorder X matrix
    X <- NULL
    for (i in 1:G){
      X <- rbind(X,Xlist[[i]])
    }
    #Tm == G
    demeanXY <- demeaning(X = X, Y = Y, G = G, N = N, Tm = G,
                          p = p, pdemean = TRUE)
      X <- demeanXY$Xdem
      Y <- demeanXY$Ydem
      p <- demeanXY$pdem
  }

  if (method == "ml"){
    res <- spsurml(X = X, Y = Y, W = W, G = G, N = N, Tm = Tm,
                   p = p, R = R, b = b, type = type, cov = cov,
                   control = list(tol = 0.05, maxit = 200,
                                  trace = trace) )
  }
  if (method == "3sls"){
    res <- spsur3sls(X = X, Y = Y, W = W, G = G, N = N, Tm = Tm,
                     p = p, type = type, maxlagW = maxlagW)
  }
  res
}




