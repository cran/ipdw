#'@name costrasterGen
#'@title Cost Raster generation
#'@author Joseph Stachelek
#'@param xymat Matrix of coordinates
#'@param pols SpatialPolygons object
#'@param projstr proj4 string defining the inherent projection
#'@return RasterLayer
#'@import raster
#'@export

#Joseph Stachelek
#This function generates a cost raster from an object of class "SpatialPolygons"

'costrasterGen'<-function(xymat,pols,projstr){
  
  #define spatial domain#####
  #Y
  krow<-range(xymat[,2])[2]-range(xymat[,2])[1]
  krow.min<-range(xymat[,2])[1]
  krow.max<-range(xymat[,2])[2]
  
  #X
  kcol<-range(xymat[,1])[2]-range(xymat[,1])[1]
  kcol.min<-range(xymat[,1])[1]
  kcol.max<-range(xymat[,1])[2]
  
  #generate cost raster####
  r<-raster(nrow=krow,ncol=kcol,crs=projstr,xmn=kcol.min,xmx=kcol.max,ymn=krow.min,ymx=krow.max)
  costras<-rasterize(pols,r,silent=TRUE)
  m <- c(0, +Inf, 10000)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  costras2<-reclassify(costras,rclmat)
  costras3<-reclassify(costras2,cbind(NA,1))
  
  return(costras3)  
  
}

