#' @param g specifically for G.stock.products. Not sure why, but this needed to be in there.


nnfix<-function(g){
  x <- vector(length = 2)
  x[1]=g[3]
  x[2]=g[6]
  return(x)
}
