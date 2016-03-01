\name{natality}
\alias{natality}
\docType{data}
\title{Natality data}
\description{
  This data contains variables about mother age and baby weight at birth in the united states.
  "True population parameters"" are given for each state and mother's race: population size, true.
}
\usage{data(natality)}
\format{
  \describe{
    \name{natality}
    \item{state}{State}
 \item{mrace}{Mother's race}
 \item{bwt}{Birth weight in grams}
 \item{lowbwt}{Indicator of low Child birth weight}
 \item{alive}{Child is the first alive child in family}
 \item{magelt15}{Mother's age is less than 15}
 \item{p.true}{Stratum level population mean of lowbwt}
 \item{magelt15.true}{Stratum level population mean of magelt15}
 \item{alive.true}{Stratum level population mean of alive}
 \item{popn}{Stratum level population counts}
 \item{sampn}{Stratum level sample counts}
  }
}


\source{ ?}
\examples{
  data(natality)
}
\keyword{datasets}
