#' @param  yields historic yield data frame provide at least 30 years of data
#' as annual (year, mean_yield (tonnes/ha))
wheat_yield = function(yields) {
  wheat=yields
  
  mean_yield = mean(wheat$mean_yield)
  sd_yield = sd(wheat$mean_yield)
  
  
}
