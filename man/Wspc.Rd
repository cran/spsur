% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Wspc}
\alias{Wspc}
\title{Spatial weight matrix for South-West Ohio Counties to estimate
Spatial Phillips-Curve}
\format{
A row-standardized squared matrix with 25 rows and columns.
The rows and columns follow the same order than provinces included in
\emph{spc} data frame.
}
\source{
Anselin (1988, p. 207)
}
\usage{
Wspc
}
\description{
A spatial weight matrix row-standardized based on first order
contiguity criterium.
}
\references{
\itemize{
    \item Anselin, L. (1988). \emph{Spatial Econometrics:
      Methods and Models}. Springer Science & Business Media.
  }
}
\keyword{datasets}
