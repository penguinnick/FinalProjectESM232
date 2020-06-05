#' wealth function performs appropriate cut-offs as specified in parms. for use in sdp_farm function 
#' @param  x vector length of possible outcomes
#' @param  t the current time step
#' @parms  parms as specified for use of sdp_farm function
#' returns vector of length x



wealth <- function(x,t, x_max=parms$x_max)
{
  
  xx <- pmin(x ,x_max)
  
  xx <- pmax(xx,0)
  
  return(xx)
}

