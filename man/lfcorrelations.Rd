\name{lfcorrelations}
\alias{lfcorrelations}
\docType{data}
\title{Labor force data, correlations of CPS unemployment estimates (used in Tiller, R. 1989)}
\description{
  Data used in Tiller, R. 1989. A Kalman filter approach to labor force estimation using survey data. Pages 16–25 of:
Proceedings of the Survey Research Methods Section, American Statistical Association, vol. 16.
}
\usage{data(laborforce)}
\format{
  A data frame with 36 observations and 2 variables.
  \describe{
    \item{lag}{lag}
    \item{correlations}{Correlation}
}
}

\source{Statistical Analysis with Missing data, Little and Rubin, 2nd Ed. p.~57}
\examples{
  data(lfcorrelations)
}
\keyword{datasets}
