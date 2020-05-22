#' @param  yields historic yield data frame provide at least 30 years of data
#' as annual (year, mean_yield (tonnes/ha))
#' Assumes data is normally distributed! Graph data beforehand
wheat_yield = function(yields) {
  wheat=yields
  
  mean_yield = mean(wheat$mean_yield)
  sd_yield = sd(wheat$mean_yield)
  
  yield = rnorm(1,mean=mean_yield, sd=sd_yield)

  if(yield <=0)
  {
    return(0)
  }
  return(yield)
}
