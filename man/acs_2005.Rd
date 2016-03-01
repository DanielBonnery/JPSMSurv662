\name{acs_2005}
\alias{acs_2005}
\docType{data}
\title{acs_2005}
\description{
  
}
\usage{data(acs_2005)}
\format{
  \describe{
    \name{acs_2005}
    \item{ST}{State code}
    \item{CTY}{County code}
    \item{acspop}{ACS 2005 direct population estimate}
    \item{acspopse}{Estimated standard error for ACS 2005 direct population estimate}
    \item{acspov}{ACS direct estimate of poverty (survey weighted count from 2005 ACS data)}
    \item{acspovse}{Estimated standard error of ACS direct estimate of poverty (survey weighted count)}
    \item{acspovrt}{ACS direct estimate of poverty rate in percentage}
\item{acspovrtse}{Estimated standard error of ACS direct estimate of poverty rate in percentage}
\item{acspop017}{ACS direct estimate of population (survey weighted count from 2005 ACS data) for under 17 years old population}
\item{acspop017se}{Estimated standard error of  ACS direct estimate of population for under 17 years old population}
\item{acspov017}{ACS direct estimate of poverty  (survey weighted count from 2005 ACS data) for under 17 years old population}
\item{acspov017se}{Estimated standard error of  ACS direct estimate of poverty (survey weighted count from 2005 ACS data) for under 17 years old population}
\item{acspovrt017}{ACS direct estimate of poverty percentage for under 17 years old population}
\item{acspovrt017se}{acspovrt017se}
\item{cenpop}{Estimated population size from Census 2000.}
\item{cenpov}{Estimated poverty from Census 2000.}
\item{cenpov017}{Estimated poverty for under 17 yo from Census 2000}
\item{cenpovrt}{Estimated poverty percentage from Census 2000.}
\item{cenpovrt017}{Estimated poverty percentage for under 17 yo from Census 2000.}
\item{cenpop017}{Estimated population of under 17 years old  from Census 2000.}
  \item{foodstamps}{Number of foodstamps participants (administrative data, from SNAP, USDA program)}
\item{dempop}{demographic estimate (projection)  of population using ACS-like poverty universe: can be used for benchmarking in 2005}
\item{dempop017}{same as dempop for under 17 years old.}
\item{foodstamprt}{Percentage of foodstamps participants (administrative data, from SNAP, USDA program)}
  }
}
\source{Data sources
Source: American Factfinder (http://factfinder.census.gov)
ACS 2005, Table C17001: POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
Decennial Census 2000, Table QT-P1: Age Groups and Sex: 2000

Source: http://www.census.gov/did/www/saipe/data/model/tables.html

Foodstamps: County SNAP benefits data

Demographic population estimates: there are  problems matching these numbers. These numbers are mostly to use as a denominator of rate predictor variables and used as a population base to multiply rate estimates to get totals (which ignores uncertainty about the population estimates). American FactFinder only goes back to 2011 for the population estimates program.

 
}
\examples{}
\keyword{datasets}
