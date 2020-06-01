## make.vector function takes age structured results of livestock offtake model and returns a vector containing the new population according to the best offtake strategy
#' @param Livestock.list a list of counts of individuals for each age class resulting from births, deaths, and offtakes
#' @return a vector containing the third item in each list

make.vector <- function(Livestock.list){
  L <- Livestock.list
  vL <- vector(length = length(L))
  v <- vector(length = length(L))
  for(i in 1: length(L)){
    vL[i] <- L[[i]][4]
    v[i] <- vL[[i]][3]
  }
  return(v)
}
