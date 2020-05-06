#'  Growing Degree Days
#' @param df the data frame containing daily min and max temperature observations
#' @param tmin Minimum temperature 
#' @param tmax Maximum temperature 
#' @param tbase lower temperature limit for crop growth
#' @param tbase_max upper temperature limit for crop growth
#' @return daily growing degree days
#' @example GDD(df, df$tmin, df$tmax, 10, 25)

GDD <- function(tmin, tmax, tbase, tbase_max){
  maxTemp <- ifelse(tmax > tbase_max, tbase_max, tmax)
  maxTemp <- ifelse(maxTemp < tbase, tbase, maxTemp)
  minTemp <- ifelse(tmin < tbase, tbase, tmin)
  minTemp <- ifelse(minTemp > tbase_max, tbase_max, minTemp)
  gdd <-mean(maxTemp, minTemp)-tbase
  return(gdd)
}
