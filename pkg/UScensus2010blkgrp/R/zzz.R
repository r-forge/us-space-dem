######################################################################
#
# zzz.R
#
# Written y Zack Almquist
# Last Modified 2/26/09
# Licensed under the GNU General Public License version 3 or later
#
# Part of the R/census package
#
# .onLoad is run when the package is loaded with library(UScensus2010blkgrp)
#
######################################################################

.onAttach <- function(libname, pkgname){
  	temp<-packageDescription("UScensus2010blkgrp")
  	msg<-paste(temp$Package,": ",temp$Title,"\n",
      "Version ",temp$Version,
      " created on ",
      temp$Date,".\n", sep="")
  msg<-paste(msg,"copyright (c) 2014, Zack W. Almquist, University of Minnesota\n",sep="")

  msg<-paste(msg,'For citation information, type citation("UScensus2010blkgrp").\n')
  msg<-paste(msg,'Type help(package="UScensus2010blkgrp") to get started.\n')
  packageStartupMessage(msg)
}


