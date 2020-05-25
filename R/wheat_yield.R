#' @param  yields historic yield data frame provide at least 30 years of data
#' as annual (year, mean_yield (tonnes/ha))
#' Assumes data is normally distributed! Graph data beforehand
#' @param area is the area of the subsistance farm used for wheat in hectares
#
#' 
#' 
wheat_yield = function(yields, area=1) {
  wheat=yields
  
  mean_yield = mean(wheat$mean_yield)
  sd_yield = sd(wheat$mean_yield)
  
  yield = area*rnorm(1,mean=mean_yield, sd=sd_yield)

  if(yield <=0)
  {
    return(0)
  }
  return(yield)
}
