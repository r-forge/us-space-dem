\name{areaPoly}
\alias{areaPoly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Area of the polygons in SpatialPolygonsDataFrame
}
\description{
Calculates the area of each polygon in \code{SpatialPolygonsDataFrame}.
}
\usage{
areaPoly(sp.object=NULL) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp.object}{
\code{SpatialPolygonsDataFrame}, must be a \code{SpatialPolygonsDataFrame} object. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a numeric vector.
}
\references{

Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Also see: \link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}
}
\author{
Zack W. Almquist
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
############################################
## Helper function for handling coloring of the map
############################################
color.map<- function(x,dem,y=NULL){
	l.poly<-length(x@polygons)
	dem.num<- cut(dem,breaks=ceiling(quantile(dem)),dig.lab = 10)
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

data(oregon.tract)





## Build density using areaPoly()
den00<-oregon.tract$pop2000/areaPoly(oregon.tract)
oregon.tract$den00<-den00

colors.use<-color.map(oregon.tract,oregon.tract$den00)
plot(oregon.tract,col=colors.use$colors,border="transparent")
title(main="Census Tracts \n of Oregon, 2000", sub="Quantiles (equal frequency)")
legend("bottomright",legend=colors.use$dem.cut,fill=colors.use$table.colors,bty="o",title="Population Density",bg="white")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
