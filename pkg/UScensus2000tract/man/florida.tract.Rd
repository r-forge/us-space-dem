\name{florida.tract}
\Rdversion{1.1}
\alias{florida.tract}
\docType{data}
\title{
florida.tract
}
\description{
 florida.tract is a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} with polygons made from the 2000 US Census tiger/line boundary files (\url{http://www.census.gov/geo/www/tiger/}) for Census Tracts. It also contains 86 variables from the Summary File 1 (SF 1) which contains the 100-percent data (\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}). 

All polygons are projected in CRS("+proj=longlat +datum=NAD83")

}
\usage{data(florida.tract)}
%\format{

%}
\details{
\bold{ID Variables} \cr
\tabular{ll}{ 
data field name \tab Full Description \cr
state \tab State FIPS code \cr
county \tab County FIPS code \cr
tract \tab Tract FIPS code \cr
}

\bold{Census Variables} \cr
\tabular{lll}{ 
Census SF1 Field Name	\tab	data field name	\tab	Full Description	\cr
(P007001)	\tab	pop2000 	\tab	population 2000	\cr
(P007002)	\tab	white	\tab	white alone	\cr
(P007003)	\tab	black	\tab	black or african american alone	\cr
(P007004)	\tab	ameri.es	\tab	american indian and alaska native alone	\cr
(P007005)	\tab	asian 	\tab	asian alone	\cr
(P007006)	\tab	hawn.pi 	\tab	native hawaiian and other pacific islander alone	\cr
(P007007)	\tab	other	\tab	some other race alone	\cr
(P007008)	\tab	mult.race	\tab	2 or more races	\cr
(P011001)	\tab	hispanic	\tab	people who are hispanic or latino	\cr
(P008002)	\tab	not.hispanic.t 	\tab	Not Hispanic or Latino	\cr
(P008003)	\tab	nh.white 	\tab	White alone	\cr
(P008004)	\tab	nh.black 	\tab	Black or African American alone	\cr
(P008005)	\tab	nh.ameri.es 	\tab	American Indian and Alaska Native alone	\cr
(P008006)	\tab	nh.asian 	\tab	Asian alone	\cr
(P008007)	\tab	nh.hawn.pi 	\tab	Native Hawaiian and Other Pacific Islander alone	\cr
(P008008)	\tab	nh.other 	\tab	Some other race alone	\cr
(P008010)	\tab	hispanic.t 	\tab	Hispanic or Latino	\cr
(P008011)	\tab	h.white 	\tab	White alone	\cr
(P008012)	\tab	h.black 	\tab	Black or African American alone	\cr
(P008013)	\tab	h.american.es 	\tab	American Indian and Alaska Native alone	\cr
(P008014)	\tab	h.asian 	\tab	Asian alone	\cr
(P008015)	\tab	h.hawn.pi 	\tab	Native Hawaiian and Other Pacific Islander alone	\cr
(P008016)	\tab	h.other 	\tab	Some other race alone	\cr
(P012002)	\tab	males	\tab	males	\cr
(P012026)	\tab	females	\tab	females	\cr
(P012003 + P012027)	\tab	age.under5	\tab	male and female under 5 yrs	\cr
(P012004-006 +  P012028-030)	\tab	age.5.17	\tab	male and female 5 to 17 yrs	\cr
(P012007-009 +  P012031-033)	\tab	age.18.21	\tab	male and female 18 to 21 yrs	\cr
(P012010-011 +  P012034-035)	\tab	age.22.29	\tab	male and female 22 to 29 yrs	\cr
(P012012-013 +  P012036-037)	\tab	age.30.39	\tab	male and female 30 to 39 yrs	\cr
(P012014-015 +  P012038-039)	\tab	age.40.49	\tab	male and female 40 to 49 yrs	\cr
(P012016-019 +  P012040-043)	\tab	age.50.64	\tab	male and female 50 to 64 yrs	\cr
(P012020-025 +  P012044-049)	\tab	age.65.up	\tab	male and female 65 yrs and over	\cr
(P013001)	\tab	med.age	\tab	median age, both sexes	\cr
(P013002)	\tab	med.age.m	\tab	median age, males	\cr
(P013003)	\tab	med.age.f	\tab	median age, females	\cr
(P015001)	\tab	households	\tab	households	\cr
(P017001)	\tab	ave.hh.sz	\tab	average household size	\cr
(P018003)	\tab	hsehld.1.m	\tab	1-person household, male householder	\cr
(P018004)	\tab	hsehld.1.f	\tab	1-person household, female householder	\cr
(P018008)	\tab	marhh.chd	\tab	family households, married-couple family, w/ own children under 18 yrs	\cr
(P018009)	\tab	marhh.no.c	\tab	family households, married-couple family, no own children under 18 yrs	\cr
(P018012)	\tab	mhh.child	\tab	family households, other family, male householder, no wife present, w/ own children under 18 yrs	\cr
(P018015)	\tab	fhh.child	\tab	family households, other family, female householder, no husband present, w/ own children under 18 yrs	\cr
(H001001)	\tab	hh.units 	\tab	housng units total	\cr
(H002002)	\tab	hh.urban 	\tab	urban housing units	\cr
(H002005)	\tab	hh.rural 	\tab	rural housing units	\cr
(H003002)	\tab	hh.occupied 	\tab	occupied housing units	\cr
(H003003)	\tab	hh.vacant 	\tab	vacant housing units	\cr
(H004002)	\tab	hh.owner 	\tab	owner occupied housing units	\cr
(H004003)	\tab	hh.renter 	\tab	renter occupied housing units	\cr
(H013002)	\tab	hh.1person 	\tab	1-person household	\cr
(H013003)	\tab	hh.2person 	\tab	2-person household	\cr
(H013004)	\tab	hh.3person 	\tab	3-person household	\cr
(H013005)	\tab	hh.4person 	\tab	4-person household	\cr
(H013006)	\tab	hh.5person 	\tab	5-person household	\cr
(H013007)	\tab	hh.6person 	\tab	6-person household	\cr
(H013008)	\tab	hh.7person 	\tab	7-person household	\cr
(H015I003)+(H015I011)	\tab	hh.nh.white.1p 	\tab	(white only, not hispanic ) 1-person household	\cr
(H015I004)+(H015I012)	\tab	hh.nh.white.2p 	\tab	(white only, not hispanic ) 2-person household	\cr
(H015I005)+(H015I013)	\tab	hh.nh.white.3p 	\tab	(white only, not hispanic ) 3-person household	\cr
(H015I006)+(H015I014)	\tab	hh.nh.white.4p 	\tab	(white only, not hispanic ) 4-person household	\cr
(H015I007)+(H015I015)	\tab	hh.nh.white.5p 	\tab	(white only, not hispanic ) 5-person household	\cr
(H015I008)+(H015I016)	\tab	hh.nh.white.6p 	\tab	(white only, not hispanic ) 6-person household	\cr
(H015I009)+(H015I017)	\tab	hh.nh.white.7p 	\tab	(white only, not hispanic ) 7-person household	\cr
(H015H003)+(H015H011)	\tab	hh.hisp.1p 	\tab	(hispanic) 1-person household	\cr
(H015H004)+(H015H012)	\tab	hh.hisp.2p 	\tab	(hispanic) 2-person household	\cr
(H015H005)+(H015H013)	\tab	hh.hisp.3p 	\tab	(hispanic) 3-person household	\cr
(H015H006)+(H015H014)	\tab	hh.hisp.4p 	\tab	(hispanic) 4-person household	\cr
(H015H007)+(H015H015)	\tab	hh.hisp.5p 	\tab	(hispanic) 5-person household	\cr
(H015H008)+(H015H016)	\tab	hh.hisp.6p 	\tab	(hispanic) 6-person household	\cr
(H015H009)+(H015H017)	\tab	hh.hisp.7p 	\tab	(hispanic) 7-person household	\cr
(H015B003)+(H015B011)	\tab	hh.black.1p 	\tab	(black) 1-person household	\cr
(H015B004)+(H015B012)	\tab	hh.black.2p 	\tab	(black) 2-person household	\cr
(H015B005)+(H015B013)	\tab	hh.black.3p 	\tab	(black) 3-person household	\cr
(H015B006)+(H015B014)	\tab	hh.black.4p 	\tab	(black) 4-person household	\cr
(H015B007)+(H015B015)	\tab	hh.black.5p 	\tab	(black) 5-person household	\cr
(H015B008)+(H015B016)	\tab	hh.black.6p 	\tab	(black) 6-person household	\cr
(H015B009)+(H015B017)	\tab	hh.black.7p 	\tab	(black) 7-person household	\cr
(H015D003)+(H015D011)	\tab	hh.asian.1p 	\tab	(asian) 1-person household	\cr
(H015D004)+(H015D012)	\tab	hh.asian.2p 	\tab	(asian) 2-person household	\cr
(H015D005)+(H015D013)	\tab	hh.asian.3p 	\tab	(asian) 3-person household	\cr
(H015D006)+(H015D014)	\tab	hh.asian.4p 	\tab	(asian) 4-person household	\cr
(H015D007)+(H015D015)	\tab	hh.asian.5p 	\tab	(asian) 5-person household	\cr
(H015D008)+(H015D016)	\tab	hh.asian.6p 	\tab	(asian) 6-person household	\cr
(H015D009)+(H015D017)	\tab	hh.asian.7p 	\tab	(asian) 7-person household	\cr
}


}
\source{
Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001.
}
\references{
\url{http://www.census.gov/ }\cr
\url{https://www.census.gov/geo/maps-data/data/tiger-line.html} \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf} \cr
}
\examples{
data(florida.tract)

############################################
## Helper function for handling coloring of the map
############################################
color.map<- function(x,dem,y=NULL){
	l.poly<-length(x@polygons)
	dem.num<- cut(dem ,breaks=ceiling(quantile(dem)),dig.lab = 6)
	dem.num[which(is.na(dem.num)==TRUE)]<-levels(dem.num)[1]
	l.uc<-length(table(dem.num))
if(is.null(y)){
	##commented out, but creates different color schemes 
	## using runif, may take a couple times to get a good color scheme.
	##col.heat<-rgb( runif(l.uc,0,1), runif(l.uc,0,1) , runif(l.uc,0,1) )
	col.heat<-heat.colors(16)[c(14,8,4,1)] ##fixed set of four colors
}else{
	col.heat<-y
	}
dem.col<-cbind(col.heat,names(table(dem.num)))
colors.dem<-vector(length=l.poly)
for(i in 1:l.uc){
	colors.dem[which(dem.num==dem.col[i,2])]<-dem.col[i,1]
	}
out<-list(colors=colors.dem,dem.cut=dem.col[,2],table.colors=dem.col[,1])
return(out)
}
############################################
## Helper function for handling coloring of the map
############################################

colors.use<-color.map(florida.tract,florida.tract$pop2000)
plot(florida.tract,col=colors.use$colors)
#text(coordinates(florida.tract),florida.tract@data$name,cex=.3)
title(main="Census Tracts \n of Florida, 2000", 
sub="Quantiles (equal frequency)")
legend("bottomright",legend=colors.use$dem.cut,
fill=colors.use$table.colors,bty="o",title="Population Count",bg="white")



###############################
### Alternative way to do the above
###############################
\dontrun{
####This example requires the following additional libraries
library(RColorBrewer)
library(classInt)  
library(maps)
####This example requires the following additional libraries

data(florida.tract)

map('state',region='florida')
plotvar <- florida.tract$pop2000
	nclr <- 4
	#BuPu
	plotclr <- brewer.pal(nclr,"BuPu")
	class <- classIntervals(plotvar, nclr, style="quantile")
	colcode <- findColours(class, plotclr)
	plot(florida.tract, col=colcode, border="transparent",add=TRUE)
	#transparent
title(main="Census Tracts \n of Florida, 2000", 
sub="Quantiles (equal frequency)")
map.text("county", "florida",cex=.7,add=TRUE)
map('county','florida',add=TRUE)
legend("bottomright","(x,y)", legend=names(attr(colcode, "table")),
fill=attr(colcode, "palette"), 
cex=0.9, bty="o", title="Population Count",bg="white")
}
}
\keyword{datasets}
