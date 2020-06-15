#' Growing Degree Days (GDD)
#'
#' computes GDD 
#' @param  weather data frame containing daily min and max temps
#' @param  tmin vector of minimum daily temps in degrees-Celcius
#' @param  tmax vector of maximum daily temps in degrees-Celcius
#' @param  tbase base temp for optimal crop growth (default is 10) 
#' @param  tmax max temp for  optimal crop growth (default is 25) 
#' @author Nick Triozzi
#' @return
#' a vector of Growing Degree Days
#'
#'

compute.GDD <- function(tmin, tmax, tbase=10, tbase_max=25){
  if (length(tmin)!=length(tmax)){
    stop("tmin and tmax must be of equal length")
  }
  if (isTRUE(max(tmax)>70)){
    stop("Please convert temperatures to Celcius")
  }
  GDD <- array()
  for(i in 1:length(tmax)){
    maxTemp <- ifelse(tmax[i] > tbase_max, tbase_max, tmax[i])
    maxTemp <- ifelse(maxTemp < tbase, tbase, maxTemp)
    minTemp <- ifelse(tmin[i] < tbase, tbase, tmin[i])
    minTemp <- ifelse(minTemp > tbase_max, tbase_max, minTemp)
    GDD[i]<-mean(maxTemp, minTemp, na.rm = TRUE, trim=0)-tbase
  }
  return(GDD)
}
