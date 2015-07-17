\name{CensusAPI2000}
\alias{CensusAPI2000}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
US Census API function
}
\description{
Allows the user to access all Summary File 1 (SF1; 100-percent data; \url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}) and SF3 (economic data). Requires the user to sign up for US Census API key, see \url{http://api.census.gov/data/key_signup.html}.
}
\usage{
CensusAPI2000(variables, state.fips, 
level = c("county", "tract", "block group", "block", "cdp"),
 key, summaryfile = c("sf1", "sf3"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{
This takes a character vector of SF1 IDs or ACS IDs, see \url{http://www.census.gov/prod/cen2000/doc/sf1.pdf} for SF1 and \url{http://www.census.gov/programs-surveys/acs/} for ACS variables.
}
  \item{state.fips}{ State fips for desired demographic variables.}
  \item{level}{ Takes "county", "tract", "block group", "block", or "cdp"}
  \item{key}{User provided key (see \url{http://api.census.gov/data/key_signup.html})}
  \item{summaryfile}{ "sf1" or "ACS"}
}
\details{
Add API Details.
}
\value{
data.frame object.
}
\references{
\url{http://api.census.gov/} \cr \cr

Zack W. Almquist (2000). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. 
}
\author{
Scott Chase (chase245@umn.edu), Zack W. Almquist (almquist@umn.edu)
}
\examples{
\dontrun{

key="YOUR KEY HERE"
sf1variables<-c("P001001","P003002",
"P003003","P003004","P003005")
sf3variables<-c("PCT024001",
"PCT024002","PCT024013")



#cdp level works
mainecdp<-CensusData2000(sf1variables,
state.fips=c("01","27"),level="cdp",key,summaryfile="sf1")
mncdp<-CensusData2000(sf1variables,
state.fips="27",level="cdp",key,summaryfile="sf1")

#county level works
mainecounty<-CensusData2000(sf1variables,
state.fips="23",level="county",key,summaryfile="sf1")
Twocounty<-CensusData2000(sf1variables,
state.fips=c("23","01"),level="county",key,summaryfile="sf1")

mncounty<-CensusData2000(sf1variables,state.fips="27",level="county",key,summaryfile="sf1")

#tract level works
az<-CensusData2000(sf1variables,
state.fips="04",level="tract",key,summaryfile="sf1")
mainetract<-CensusData2000(sf1variables,
state.fips="23",level="tract",key,summaryfile="sf1")

#block group level works
maineblkgrp<-CensusData2000(sf1variables,
state.fips="23",level="block group",key,summaryfile="sf1")
az<-CensusData2000(sf1variables,
state.fips="04",level="block group",key,summaryfile="sf1")

#block level works
maineblk<-CensusData2000(sf1variables,
state.fips="23",level="block",key,summaryfile="sf1")
ct<-CensusData2000(sf1variables,
state.fips="09",level="block",key,summaryfile="sf1")

}

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
