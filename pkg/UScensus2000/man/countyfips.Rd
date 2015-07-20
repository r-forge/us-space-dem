\name{countyfips}
\alias{countyfips}
\docType{data}
\title{
County FIPS codes
}
\description{
County names and FIPS codes for use in \code{\link{county}}
}
\usage{data(countyfips)}
\format{
  A data frame with 3143 observations on the following 4 variables.
  \describe{
    \item{\code{fips}}{a character vector}
    \item{\code{countyname}}{a character vector}
    \item{\code{statename}}{a character vector}
    \item{\code{acronym}}{a character vector}
  }
}
\details{
This file lists all counties and equivalent areas in the United States defined as of 2000. Built from \url{http://www.census.gov/tiger/tms/gazetteer/county2k.txt}. This is primarily for use in \code{\link{county}}.
}
\source{
\url{https://www.census.gov/geo/reference/codes/cou.html}
}
%\references{}
\examples{
data(countyfips)
}
\keyword{datasets}
