#' @param  yields historic yield data frame provide at least 30 years of data
#' as annual (year, mean_yield (tonnes/ha))
#' Assumes data is normally distributed! Graph data beforehand
barley_yield = function(yields) {
  barley=yields
  
  mean_yield = mean(barley$mean_yield)
  sd_yield = sd(barley$mean_yield)
  
  yield = rnorm(1,mean=mean_yield, sd=sd_yield)
  if(yield <=0)
  {
    return(0)
  }
  return(yield)
}
