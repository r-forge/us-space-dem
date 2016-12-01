poly.clipper<-function (name, state, statefips = FALSE, level = c("tract", 
    "blk", "blkgrp"), sp.object = NULL, proj = NULL) 
{
poly.clipper.sub<-function (name, state, statefips = FALSE, level = c("tract","blk", "blkgrp"), sp.object = NULL, proj = NULL){ 
require(paste("UScensus2000", level, sep = ""), character.only = TRUE)
#require("UScensus2010cdp")
#require(rgeos)

city <- city(name, state, statefips)
state <- check.state(state, statefips)
        if (is.null(state)) {
            stop("Not a State! \n")
        }
        
 
        if (!is.null(sp.object)) {
            sp<- sp.object
        }else {
            x <- paste(state,".",level, sep = "")
            utils::data(list = x, envir = parent.frame()) ###Check enviroment
            sp <- get(x)
        }


int<-suppressWarnings(gIntersection(sp,city,byid = TRUE,drop_lower_td=TRUE))
sp<-spTransform(sp,CRS(proj4string(int)))
int<-spTransform(int,CRS(proj4string(sp)))
#int<-int@polyobj
int<-as(int,"SpatialPolygonsDataFrame")
#int<-spChFIDs(int,sapply(strsplit(rownames(int@data)," "),"[[",1))
#int<-spChFIDs(int,sapply(strsplit(sapply(slot(int, "polygons"), function(i) slot(i, "ID"))," "),"[[",1))
int<-spChFIDs(int,paste(rep(state,length(slot(int, "polygons"))),1:length(slot(int, "polygons")),sep="_"))
data<-over(int,sp)
int@data<-data

#m<-match(rownames(int@data),rownames(sp@data))
#int@data<-sp@data[m,]
#phat<-areaPoly(int)/areaPoly(sp[m,])
#phat[phat>1]<-1
#int@data<-int@data[,!sapply(int@data,is.character)]*as.vector(phat)

if (is.null(proj) == FALSE) {
  int <- spTransform(int, proj)
}
int
}
poly.clipper.sub(name=name, state=state, statefips = statefips, level = level, sp.object = sp.object, proj = proj)

}

