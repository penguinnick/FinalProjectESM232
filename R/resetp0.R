## reset.p0 function takes age structured results of livestock offtake model and returns a vector containing the new population according to the best offtake strategy
#' @param Livestock.list a list of counts of individuals for each age class resulting from births, deaths, and offtakes
#' @param bestV a vector serving as an index for the best strategy
#' @param classes an integer indicating how many age classes there are
#' @param even_odd logical, if TRUE, then function assumes Livestock.list is length 2. If false, will return the ith age structure in the list
#' @return a vector with couunt of individuals per age class



reset.p0 <- function(Livestock.list, bestV, classes=9, even_odd=TRUE){
  L <- Livestock.list
  mnewpop <- matrix(nrow = classes, ncol = length(L)) # vector(length = length(L))
  for(i in 1:length(L)){
    p <- L[[i]]
    p.df <- as.data.frame(p)
    p.df[,2] <- ifelse(p.df[,2]<0,0,p.df[,2]) # sets negative numbers to 0
    mnewpop[,i]<- p.df[,2]
  }
  if(even_odd==TRUE){
    p.ind <- ifelse(bestV%%2==0, 2, 1)
  } else {
    p.ind <- bestV
  }
  next.pop<-mnewpop[,p.ind]
  return(next.pop)
}
