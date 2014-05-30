#'@name ipdwInterp
#'@title Inverse Distance Weighting with custom distances
#'@author Joseph Stachelek
#'@param spdf SpatialPointsDataFrame object
#'@param rstack RasterStack of path distances
#'@param paramlist chacter. String representing parameter names
#'@param overlapped logical. Default is FALSE, specify TRUE if some points lie on top of barriers
#'@param yearmon character. String specifying the name of the spdf
#'@param removefile logical. Remove files after processing?
#'@return RasterLayer
#'@import raster
#'@import gdistance
#'@export


#'@export
#Author: Joseph Stachelek
#This function takes a rasterstack of pathdistances and generates surfaces by weighting parameter values by these distances

'ipdwInterp'<-function(spdf,rstack,paramlist,overlapped=FALSE,yearmon="default",removefile=TRUE){
  
  for(k in 1:length(paramlist)){
    
    tr.names<-names(spdf)
    param.ind<-which(tr.names==paramlist[k])
    
    #exclude NA values for param from spdf and rstack.sum
    param.na2<-which(is.na(spdf@data[,param.ind]))
    if(length(param.na2)>=1){
      param.na<-as.numeric(row.names(spdf[which(is.na(spdf@data[,param.ind])),]))
      spdf<-spdf[-which(is.na(spdf@data[,param.ind])),]  
      rstack<-dropLayer(rstack,param.na)
      }    
    
     
    #raster sum
    #rstack.sum<-sum(rstack,na.rm=TRUE) #need to set na.rm = TRUE if points are on land
    if(overlapped==TRUE){
      
      rstack.sum<-sum(rstack,na.rm=TRUE) 
      rstack.sum<-reclassify(rstack.sum,cbind(0,NA))
      
    }else{
    
    rstack.sum<-sum(rstack,na.rm=FALSE) 
    rstack.sum<-reclassify(rstack.sum,cbind(0,NA))
    }
    
    #calculate the weight of the individual rasters 
    
    i=1
    for(i in 1:dim(rstack)[3])  {
      ras.weight<-rstack[[i]]/rstack.sum
      param.value<-data.frame(spdf[i,paramlist[k]])
      param.value2<-as.vector(unlist(param.value[1]))
      ras.mult<-ras.weight*param.value2
      
      rf<-writeRaster(ras.mult,filename=paste(tempdir(),"\\",paramlist[k],"A5ras",i,".grd",sep=""),overwrite=T)
      #rf<-writeRaster(ras.mult,filename=paste("DF_Surfaces/",yearmon,"/",paramlist[k],"A5ras",i,".grd",sep=""),overwrite=T)
    }
    
    raster_data_full<-list.files(path=paste(tempdir(),"\\",sep=""),pattern=paste(paramlist[k],"A5ras*",sep=""),full.names=T)
    #raster_data_full<-list.files(path=paste(getwd(),"/DF_Surfaces/",yearmon,"/",sep=""),pattern=paste(paramlist[k],"A5ras*",sep=""),full.names=T)
    raster_data<-raster_data_full[grep(".grd",raster_data_full,fixed=T)]
    as.numeric(gsub('.*A5ras([0123456789]*)\\.grd$','\\1',raster_data))->fileNum
    raster_data<-raster_data[order(fileNum)]
    
    #sum rasters to get final surface
    rstack.mult<-stack(raster_data)
    
    if(overlapped==TRUE){
        finalraster<-sum(rstack.mult,na.rm=T)
    }else{
      finalraster<-sum(rstack.mult,na.rm=F) #this is the correct on for kattegat
    }
          
    #finalraster<-sum(rstack.mult,na.rm=T) #this is the correct one for dflow
     
    r <- rasterize(spdf, rstack[[1]], paramlist[k])
    finalraster<-cover(r,finalraster)
    
    file.remove(raster_data_full)
    return(finalraster)
  }
#optional removal of path distances
if(removefile==TRUE){
  file.remove(list.files(path=paste(tempdir(),"\\",sep=""),pattern=paste(yearmon,"A4ras*",sep=""),full.names=T))
  file.remove(raster_data_full)
}
}