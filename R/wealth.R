#' wealth function performs appropriate cut-offs as specified in parms. for use in sdp_farm function 
#' @param  x vector length of possible outcomes
#' @param  t the current time step
#' @parms  parms as specified for use of sdp_farm function
#' returns vector of length x



wealth <- function(x,t, x_max=160,x_crit=0)
{
  # x <- as.integer(x)
  # xx<-ifelse(x<x_max,x,x_max)
  # xx <- ifelse(xx<x_crit,x_crit,xx)
  xx <- pmin(x ,x_max)
  xx <- pmax(xx,x_crit)
  # wealth <- f[t,as.integer(xx-x_crit+1)]
  # return(f[t,xx])
  wealth <- f[t,xx-x_crit+1]
}
# fitness <- function(x,t)
# {
#   xx <- pmin(x ,x_max)
#   xx <- pmax(xx,x_crit)
#   fitness <- f[t,xx-x_crit+1]
# }

# inherits()