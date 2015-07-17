demographics<-function (dem = "pop2000", state, statefips = FALSE, level = c("tract", 
    "blk", "blkgrp", "cdp", "msa", "county"), msaname = NULL) 
{
    demographics.aux <- function(dem = "pop2000", state, statefips = FALSE, 
        level = c("tract", "blk", "blkgrp", "cdp", "msa", "county"), 
        msaname = NULL) {
        state2 <- state
        state <- check.state(state, statefips)
        if (is.null(state)) {
            stop("Not a State! \n")
        }
       dem.fun <- function(dem, state, level) {
            require(paste("UScensus2000", level, sep = ""), character.only = TRUE)
            x <- paste(state, level, sep = ".")
            data(list = x, envir = parent.frame())
            temp <- get(x)
            out <- temp@data[,dem]
            out<-as.matrix(out)
            return(out)
        }
        removeDup<-function(temp){
	temp$state<-as.character(temp$state)
            temp$county<-as.character(temp$county)
            temp$tract<-as.character(temp$tract)
            tract.fips <-temp$tract
            tract.fips[nchar(tract.fips) == 4] <- paste(temp$tract[nchar(tract.fips) == 4], "00", sep = "")
	  temp.names<-paste(temp$state,temp$county,tract.fips,sep = "")
		d<-which(duplicated(temp.names))
		temp<-temp[-d,]
}

 
        if (level == "county") {
        	denv <- new.env()
        	data(list=paste(state, "tract", sep = "."),envir=denv)
            temp <- get(paste(state, "tract", sep = "."), envir=denv)
			temp<-removeDup(temp)
			temp@data<-temp@data[,c("state","county","tract",dem)]
			stcty<-paste(as.character(temp$state),as.character(temp$county),sep="")
			ctyNam<-unique(stcty)
			if(length(dem)==1){
								ctyDem<-matrix(sapply(ctyNam,function(x){sum(temp@data[which(x==stcty),4:NCOL(temp@data)])}),ncol=1)
				colnames(ctyDem)<-dem
			}else{
			ctyDem<-t(sapply(ctyNam,function(x){apply(temp@data[which(x==stcty),4:NCOL(temp@data)],2,sum)}))}
			data("countyfips",envir = denv)
		nams<-get("countyfips", envir=denv)
		cnam<-nams$countyname[match(ctyNam,nams$fips)]
		out<-data.frame(fips=ctyNam,ctyDem,stringsAsFactors =FALSE)
		rownames(out)<-cnam	
        }
        else if (level == "msa") {
			temp <- MSA(msaname = msaname, state = toupper(state2),level = "tract")
			#dem=c("pop2000", "hh.units", "households")
			#dem="pop2000"
			#temp <- MSA(msaname = "washington", state = "va",level = "tract")
			temp<-removeDup(temp)
			temp@data<-temp@data[,c("state","county","tract",dem)]
			stcty<-paste(as.character(temp$state),as.character(temp$county),sep="")
			ctyNam<-unique(stcty)
			if(length(dem)==1){
				ctyDem<-matrix(sapply(ctyNam,function(x){sum(temp@data[which(x==stcty),4:NCOL(temp@data)])}),ncol=1)
				colnames(ctyDem)<-dem
			}else{
			ctyDem<-t(sapply(ctyNam,function(x){apply(temp@data[which(x==stcty),4:NCOL(temp@data)],2,sum)}))}

			denv <- new.env()
			data("countyfips",envir = denv)

			nams<-get("countyfips", envir=denv)
			cnam<-nams$countyname[match(ctyNam,nams$fips)]
			out<-data.frame(fips=ctyNam,ctyDem,stringsAsFactors =FALSE)
			rownames(out)<-cnam			
        }
        else if (level == "tract") {
            out <- dem.fun(dem, state, level)
            
            temp <- get(paste(state, level, sep = "."))
            tract.fips <- as.character(temp$tract)
            tract.fips[nchar(tract.fips) == 4] <- paste(as.character(temp$tract[nchar(tract.fips) == 
                4]), "00", sep = "") 
            temp.names<-paste(as.character(temp$state), as.character(temp$county), as.character(tract.fips),sep = "")
            d<-which(duplicated(temp.names))
            out<-out[-d,]       
            rownames(out) <-temp.names[-d]
        }
        else if (level == "blkgrp") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            tract.fips <- as.character(temp$tract)
            tract.fips[nchar(tract.fips) == 4] <- paste(temp$tract[nchar(tract.fips) == 4], "00", sep = "")
         fipState<-as.character(temp$state)   
         fipCounty<-as.character(temp$county)   
         fipBlkgrp<-as.character(temp$blkgrp)
            temp.names<-paste(fipState,fipCounty,tract.fips, fipBlkgrp, sep = "")
            d<-which(duplicated(temp.names))
            out<-out[-d,]       
            rownames(out) <-temp.names[-d] 
        }
        else if (level == "blk") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            temp.names<-as.character(temp$fips)
            d<-which(duplicated(temp.names))
            if(length(d)>0){out<-out[-d,]        
            rownames(out) <-temp.names[-d] 
            }else{
            	rownames(out) <-temp.names 
            	}
        }
        else if (level == "cdp") {
            out <- dem.fun(dem, state, level)
            temp <- get(paste(state, level, sep = "."))
            if (length(dem) == 1) {
                out <- out[match(unique(temp$name), temp$name)]
                names(out) <- unique(temp$name)
            }
            else {
                out <- out[match(unique(temp$name), temp$name), 
                  ]
                rownames(out) <- unique(temp$name)
            }
        }
        out
    }
    demographics.aux(dem, state, statefips, level, msaname)
}