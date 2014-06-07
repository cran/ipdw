#'@name pathdistGen
#'@title Path Distance generation
#'@author Joseph Stachelek
#'@param spdf SpatialPointsDataFrame object
#'@param costras RasterLayer cost raster
#'@param range numeric. Range of interpolation neighborhood
#'@param step numeric. Number of sub loops to manage memory during raster processing.
#'@param yearmon character. String specifying the name of the spdf
#'@return RasterStack object of path distances
#'@import raster
#'@import gdistance
#'@export


#Author: Joseph Stachelek
#This function generates path distances from each georeferenced point in a spdf

'pathdistGen'<-function(spdf,costras,range,step=16,yearmon="default"){
  
  #dir.create(file.path(getwd(),"DF_Surfaces"),showWarnings=F)
  #dir.create(file.path(getwd(),"DF_Surfaces/",yearmon),showWarnings=F)
  ipdw.range<-range/res(costras)[1]/2 #this is a per cell distance
  
  #start interpolation#####
  #calculate conductances hence 1/max(x)
  trans<-transition(costras,function(x)1/max(x),directions=16)
  i=1
  coord<-spdf[i,]
  A<-accCost(trans,coord)
  dist<-hist(A,plot=F)$breaks[2]
  
    for(i in 1:nrow(spdf)){
      coord<-spdf[i,]
      A<-accCost(trans,coord)
      A2<-reclassify(A,c(dist,+Inf,NA,ipdw.range,dist,ipdw.range)) #raster cells are 1 map unit
      A3<-((ipdw.range/A2)^2)
      A4<-reclassify(A3,c(-Inf,1,0))
      #check output with - zoom(A4,breaks=seq(from=0,to=5,by=1),col=rainbow(5))
      #showTmpFiles()
      
      rf<-writeRaster(A4,filename=file.path(tempdir(),paste(yearmon,"A4ras",i,".grd",sep="")),overwrite=T,NAflag=-99999)
      #print(paste(round((i/nrow(spdf))*100,1),"% complete"))
    }
     
  
  #create raster stack
  raster_data<-list.files(path=file.path(tempdir()),pattern=paste(yearmon,"A4ras*",sep=""),full.names=T)
  raster_data<-raster_data[grep(".grd",raster_data,fixed=T)]
  as.numeric(gsub('.*A4ras([0123456789]*)\\.grd$','\\1',raster_data))->fileNum
  raster_data<-raster_data[order(fileNum)]
  rstack<-stack(raster_data)
  rstack<-reclassify(rstack,c(-99999,-99999,NA))
  file.remove(list.files(path=file.path(tempdir()),pattern=paste(yearmon,"A4ras*",sep=""),full.names=T))
  
  return(rstack)
}