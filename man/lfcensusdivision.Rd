\name{lfcensusdivision}
\alias{lfcensusdivision}
\docType{data}
\title{Labor force data, census division level (used in Tiller, R. 1989)}
\description{
  Data used in Tiller, R. 1989. A Kalman filter approach to labor force estimation using survey data. Pages 16–25 of:
Proceedings of the Survey Research Methods Section, American Statistical Association, vol. 16.
}
\usage{data(lfcensusdivision)}
\format{
  A data frame with 8 observations and 6 variables.
  The variables are:
  \describe{
\item{Div}{Census division}
\item{Year}{Year}
\item{month}{Month}
\item{CPSUn}{Unemployment (from CPS)}
\item{BUnSIGupd}{Benchmarked Unemployment Signal }
\item{BUnTRDupd}{Benchmarked Unemployment Trend}
\item{UnSIGupd}{Unemployment (not benchmarked) Signal }
\item{UnTRDupd}{Unemployment (not benchmarked) Trend}
\item{StdCPSUn}{Standard deviation of "CPSUn" (obtained via GVF method, used in Tiller, 1996 (JOS))}
\item{BUnStdSIGoupd}{Benchmarked unemployment signal standard error }
\item{BUnStdTRDoupd}{Benchmarked unemployment trend standard error}
\item{UnStdSIGupd}{(Not benchmarked) unemployment signal standard error}
\item{UnStdTRDupd}{(Not benchmarked) unemployment trend standard error}
\item{UnRegSmt}{Level shifts (included in signal & trend)}
}
}
\source{Data used for 
Tiller, R. 1989. A Kalman filter approach to labor force estimation using survey data. Pages 16–25 of:
Proceedings of the Survey Research Methods Section, American Statistical Association, vol. 16.}
\examples{
  data(lfcensusdivision)
}
\keyword{datasets}
