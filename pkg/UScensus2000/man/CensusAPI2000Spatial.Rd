\name{CensusAPI2000Spatial}
\alias{CensusAPI2000Spatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
US Census API function
}
\description{
Allows the user to access all Summary File 1 (SF1; 100-percent data; \url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}) and SF3 (economic data). Requires the user to sign up for US Census API key, see \url{http://api.census.gov/data/key_signup.html}.
}
\usage{
CensusAPI2000Spatial(variables, state.fips, 
level = c("county", "tract", "block group", "block", "cdp"), 
key, summaryfile = c("sf1", "sf3"), sp.object = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{
This takes a character vector of SF1 IDs or ACS IDs, see \url{http://www.census.gov/prod/cen2010/doc/sf1.pdf} for SF1 and \url{http://www.census.gov/programs-surveys/acs/} for ACS variables.
}
  \item{state.fips}{ State fips for desired demographic variables.}
  \item{level}{ Takes "county", "tract", "block group", "block", or "cdp"}
  \item{key}{User provided key (see \url{http://api.census.gov/data/key_signup.html})}
  \item{summaryfile}{ "sf1" or "ACS"}
  \item{sp.object}{ sp object}
}
\details{
Add API Details.
}
\value{
data.frame object.
}
\references{
\url{http://api.census.gov/} \cr \cr

Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. 
}
\author{
Zack W. Almquist (almquist@umn.edu), Scott Chase (chase245@umn.edu)
}
\examples{
\dontrun{

key="YOUR KEY HERE"
sf1variables<-c("P001001","P003002",
"P003003","P003004","P003005")
sf3variables<-c("PCT024001",
"PCT024002","PCT024013")

mainecdp<-CensusData2000Spatial(sf1variables,
state.fips=c("01","27"),level="cdp",key,summaryfile="sf1")

az<-CensusData2000Spatial(sf1variables,
state.fips="04",level="tract",key,summaryfile="sf1")


}

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
