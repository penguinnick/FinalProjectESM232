#' function to calculate mean, max, min, and sd of raster data from polygons or points
#' @param ispat the input Spatial* data, or x in over function
#' @param isgdf a SpatialGridDataFrame to summarize
#' @param pointsonly If TRUE, one value returned, if false, all stats created
#' @return an updated version of ispat
#' 
#' 

get_site_stats <- function(ispat, isgdf, pointsonly=FALSE){
  if(is(p.wheat.sgdf, "SpatialGridDataFrame")==FALSE){
    stop("input raster data must be SpatialGridDataFrame.")
  }
ospat=ispat
if(pointsonly==FALSE){
  x1 <- over(ispat, isgdf, fn = mean)
  x2 <- over(ispat, isgdf, fn = max)
  x3 <- over(ispat, isgdf, fn = min)
  x4 <- over(ispat, isgdf, fn = sd)
  outdf=cbind.data.frame(x1,x2,x3,x4)
  colnames(outdf) <- c("mean", "max", "min", "sd")
  ospat@data=cbind.data.frame(ospat@data, outdf)
} else {
  outdf=over(ispat,isdgf)
  ospat=cbind.data.frame(ospat@data, outdf)
}
return(ospat)
}