#'  Growing Degree Days
#' @param df the data frame containing daily min and max temperature observations
#' @param tmin Minimum temperature 
#' @param tmax Maximum temperature 
#' @param tbase lower temperature limit for crop growth
#' @param tbase_max upper temperature limit for crop growth
#' @return daily growing degree days
#' @example GDD(df, df$tmin, df$tmax, 10, 25)

compute.GDD <- function(df, tmin, tmax, tbase, tbase_max){
  GDD <- array()
  for(i in 1:nrow(df)){
    maxTemp <- ifelse(tmax[i] > tbase_max, tbase_max, tmax[i])
    maxTemp <- ifelse(maxTemp < tbase, tbase, maxTemp)
    minTemp <- ifelse(tmin[i] < tbase, tbase, tmin[i])
    minTemp <- ifelse(minTemp > tbase_max, tbase_max, minTemp)
    GDD[i]<-mean(maxTemp, minTemp)-tbase
  }
  return(GDD)
}