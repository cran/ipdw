#'@name errorGen
#'@title Generate Error statistics
#'@author Joseph Stachelek
#'@param finalraster RasterLayer object
#'@param measured.spdf SpatialPointsDataFrame
#'@param measured.data data.frame
#'@param plot logical. Plot comparison?
#'@param title Plot labels
#'@return List of error statistics
#'@export
##generate interpolation error stats from validation datasets

'errorGen'<-function(finalraster,measured.spdf,measured.data,plot=FALSE,title=""){
  
predicted<-extract(finalraster,measured.spdf)
  
m.v.p<-data.frame(cbind(measured.data,predicted))
rmse<-sqrt(mean((m.v.p[,1]-m.v.p[,2])^2,na.rm=TRUE))
stats<-data.frame(c("r2","rmse", rmse,"pe","max","min","range"))
 
stats<-list(stats,m.v.p)


#optional plotting
if(plot==TRUE){
  plot(m.v.p[,1],m.v.p[,2],ylim=range(m.v.p[,1]),ylab="Interpolated",xlab="Measured",main=title)
  fit<-lm(m.v.p[,2]~m.v.p[,1])
  abline(fit,col="red")
  abline(a=0,b=1)
}

return(stats)
}