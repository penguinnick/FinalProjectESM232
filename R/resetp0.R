## reset.p0 function takes age structured results of livestock offtake model and returns a vector containing the new population according to the best offtake strategy
#' @param Livestock.list a list of counts of individuals for each age class resulting from births, deaths, and offtakes
#' @param bestV a vector serving as an index for the best strategy
#' @param classes an integer indicating how many age classes there are
#' @param even_odd logical, if TRUE, then function assumes Livestock.list is length 2. If false, will return the ith age structure in the list
#' @return a vector with couunt of individuals per age class



reset.p0 <- function(Livestock.list, bestV, classes=9, even_odd=TRUE){
  l <- Livestock.list
  r <- c(1, length(l))
  l <- sapply(r, function(r){l[[r]]})
  if(even_odd==TRUE){
    p.ind <- ifelse(bestV%%2==0, 2, 1)
  } else {
    p.ind <- bestV
  }
  next.pop<- l[,p.ind]
  # next.pop<- L[[1]][3]
  
  # mnewpop <- matrix(nrow = classes, ncol = length(L)) # vector(length = length(L))
  # for(i in 1:length(L)){
  #   p <- L[[i]]
  #   p.df <- as.data.frame(p)
  #   p.df[,2] <- ifelse(p.df[,2]<0,0,p.df[,2]) # sets negative numbers to 0
  #   mnewpop[,i]<- p.df[,2]
  # }
  
  # next.pop<-mnewpop[,p.ind]
  # next.pop<-L[,p.ind] # [[p.ind]]$popbyage[,2]
  return(next.pop)
}
