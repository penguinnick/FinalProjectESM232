#' @param df results of sdp_df function
#' @return a data frame
#' 

compute.wealth.metrics= function(df, mean.only=FALSE){
  df=df[-1,]
  
  if(mean.only==TRUE){
    return(mean.wealth=mean(df$avg.wealth, na.rm = T))
  } else {
    return(list(mean.wealth=mean(df$avg.wealth, na.rm = T),
                variance=var(df$avg.wealth, na.rm = T),
                cor.wheat=cor(df$wheat.yld, df$avg.wealth),
                cor.barley=cor(df$barley.yld, df$avg.wealth),
                cor.herd= cor(df$mean.herd, df$avg.wealth) ))
  }
}
