#' @param sdp_output results of sdp_farm function
#' @return a data frame

sdp_df=function(sdp_output){
  fw=sdp_output[[1]]
  # gp=sdp_output[[4]]
  year= 1:sdp_output[[5]]
  # avg.wealth=apply(sdp.res[[1]],1 ,mean)
  avg.wealth=apply(fw,1 ,mean)
  sd.wealth=apply(fw,1 ,sd)
  # sd.wealth=apply(sdp.res[[1]],1 ,sd)
  wheat.yld=sdp_output[[6]]#crop.parms$wheatY$mean_yield
  barley.yld=sdp_output[[7]]#crop.parms$barleyY$mean_yield
  gp<-t(apply(sdp_output[[4]], c(1,2), sum))
  mean.herd=apply(gp,1,mean)
  
  df=data.frame(year=year,avg.wealth=avg.wealth, sd.wealth=sd.wealth, wheat.yld=wheat.yld, barley.yld=barley.yld, mean.herd=mean.herd)
  return(df)
}
