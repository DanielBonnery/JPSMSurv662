\name{lfstate}
\alias{lfstate}
\docType{data}
\title{Labor force data, state level (used in Tiller, R. 1989)}
\description{
  Data used in Tiller, R. 1989. A Kalman filter approach to labor force estimation using survey data. Pages 16–25 of:
Proceedings of the Survey Research Methods Section, American Statistical Association, vol. 16.
}
\usage{data(laborforce)}
\format{
  A data frame with 8 observations and 6 variables.
  \describe{
    \item{ST}{ST}
    \item{Year}{Year}
    \item{month}{month}
    \item{CPSun}{CPSun}
    \item{CPSem}{CPSem}
    \item{CPSpop}{CPSpop}
    \item{Cntwoer}{Cntwoer}
    \item{CesEm}{CesEm}
    \item{StdCPSUn}{StdCPSUn}
    \item{StdCPSEm}{StdCPSEm}}
}

\source{Statistical Analysis with Missing data, Little and Rubin, 2nd Ed. p.~57}
\examples{
  data(lfstate)
}
\keyword{datasets}
