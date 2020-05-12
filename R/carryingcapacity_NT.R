#'  Carrying Capacity
#' @param kc fraction consumed
#' @param kd fraction digested
#' @param P Net production (kg biomass/ha/year)
#' @param HI Harvest index (portion of harvest that can be consumed by organism)
#' @param GE Gross energy content of feed in MJ/kg
#' @param DE daily energy requirement of organism (in MJ energy/day)
#' @return carrying capacity
#' @example 
#' parms=list(kc=0.8, kd=0.85, HI=0.4)
#' carryingcapacity(0.8, 0.85, 15000, 0.4)

carryingcapacity = function(yield, parms, GE=17, DE=10.5){
  eP=yield*parms$HI                            # portion of net production that is edible (accounting for losses to P)
  DEd=DE                             # daily energy requirement in MJ
  DEy=DEd*365/1000                   # annual energy requirement in GJ  
  minyield=(DEy/(parms$kc*parms$kd*GE))*1000         # minimum yield per person (in kg/year)
  K=eP/minyield                          # carrying capacity in number of organisms per year per hectare
  return(K)
}
