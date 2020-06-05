#' function to create an array for storing goat population
#' @param times times sequence of length t_max
#' @param x_class sequence of length x_max
#' @param agecats age category labels used in goat offtake population function
# 
# goat.array <- function( times, x_class, agecats){
#   time.strings<- vector(length = length(times))
#   # time.strings[1] <- "0"
#   for(i in times){
#     time.strings[i] <- toString(times[i])
#   }
#   # time.strings<- vector(length = length(times)+1)
#   # time.strings[1] <- "0"
#   # for(i in times[-1]){
#   #   time.strings[i] <- toString(times[i])
#   # }
#   # class.strings <- vector(length = length(x_class)-1)
#   # for(i in x_class[-1]){
#   #   class.strings[i] <- toString(x_class[i])
#   # }
#   class.strings <- vector(length = length(x_class))
#   for(i in x_class){
#     class.strings[i] <- toString(x_class[i])
#   }
#   goat.array = array(dim=c( length(time.strings), length(class.strings), length(agecats)),dimnames = list(time.strings,class.strings,agecats))
#   # goat.array = array(dim=c(length(class.strings), length(time.strings), length(agecats)-1),dimnames = list(class.strings,time.strings,agecats))
#                      # dimnames = list(class.strings,time.strings,agecats[-1]))
#   return(goat.array)
# }


goat.array <- function( x_class, t_max, agecats){
 
  # time.strings<- vector(length = length(times)+1)
  # time.strings[1] <- "0"
  # for(i in times[-1]){
  #   time.strings[i] <- toString(times[i])
  # }
  # class.strings <- vector(length = length(x_class)-1)
  # for(i in x_class[-1]){
  #   class.strings[i] <- toString(x_class[i])
  # }
  class.strings <- vector(length = length(x_class))
  for(i in x_class){
    class.strings[i] <- toString(x_class[i])
  }
  timeseq <- seq(1:t_max)
  time.strings<- vector(length = length(timeseq))
  # time.strings[1] <- "0"
  for(i in timeseq){
    time.strings[i] <- toString(timeseq[i])
  }
  goat.array = array(dim=c( length(class.strings),  length(time.strings), length(agecats)),dimnames = list(class.strings,time.strings,agecats))
  # goat.array = array(dim=c(length(class.strings), length(time.strings), length(agecats)-1),dimnames = list(class.strings,time.strings,agecats))
  # dimnames = list(class.strings,time.strings,agecats[-1]))
  return(goat.array)
}

