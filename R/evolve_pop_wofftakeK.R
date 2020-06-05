#' Population Evolution using Leslie Matrix
#' Evolve a population
#' @param fertility fertility rates
#' @param survivability survivability rates
#' @param initialpop initial population
#' @param nstep number of time steps
#' @param K total population carrying capacity
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_pop_wofftake = function(fertility, survivability, initialpop, nstep, offtake, K=0, minHerd=NULL) {
  
  nclasses = length(fertility)
  
  # make sure inputs are in the right format
  if ((nclasses!=length(survivability) ))
  { return("fertility doesn’t match survivability") }
  
  if ((nclasses!=length(initialpop) ))
  { return("population initialization  doesn’t match fertility") }
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  lm.offtake = leslie_matrix
  leslie_matrix[1,] = fertility
  
  # lm.offtake[1,] = survivability
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivability[i]
    lm.offtake[i,i] = (1-survivability[i])*offtake
    # lm.offtake[i+1,i] = (1-survivability[i])*offtake
    # leslie_matrix[i,i] = offtake[i]
  }
  leslie_matrix[nclasses,nclasses] = survivability[nclasses]
  # lm.offtake[nclasses,nclasses] = (1-survivability[nclasses])*offtake
  # create an matrix to store population structure
  pop.structure = matrix(nrow=nclasses, ncol=nstep)
  offtake.str = matrix(nrow=nclasses, ncol=nstep)
  total.pop = rep(0, times=nstep)
  total_off = rep(0, times=nstep)
  pop.structure[,1] = initialpop
  additions = rep(0, times=nstep)
  for (i in 2:nstep) {
    
    total.pop[i]=sum(pop.structure[,i-1])
    # total_off[i]=sum(offtake.str[,i-1])
    
    # if we are using a carrying capacity adjust fertitlity if we are getting close
    if (K>0) {
      ratio = max(0, 1.0-total.pop[i]/K)
      leslie_matrix[1,] = fertility*ratio
    }
    tp <- total.pop[i]
    if(tp<minHerd){
      leslie_matrix[1,] = fertility
      ratio=(minHerd-tp)/tp
      offtake.str[,i] = (lm.offtake*ratio)%*%pop.structure[,i-1]
    }
    offtake.str[,i] = lm.offtake%*%pop.structure[,i-1]
    pop.structure[,i] = (leslie_matrix %*% pop.structure[,i-1]) - offtake.str[,i]
    
    # pop.structure[,i] = (leslie_matrix %*% pop.structure[,i-1])- (pop.structure[,i-1]*offtake)
    
    for(j in 1:nclasses){
      pop.structure[j,i]=ifelse(pop.structure[j,i]<0,0,pop.structure[j,i])
      # offtake.str[j,i] = ifelse(lm.offtake[j,i]<0,0,lm.offtake[j,i]) #%*%pop.structure[,i-1]
    }
    # total_off[i]=sum(pop.structure[,i-1])-sum(pop.structure[,i])
    total.pop[i]=sum(pop.structure[,i])
    additions[i]=sum(total.pop[i]-total.pop[i-1])
    total_off[i]=sum(offtake.str[,i])
  }
  # return(list(popbyage=pop.structure, newstock=additions[-2], offtakes=offtake.str, annofftakes=total_off, anntotp=total.pop))
  return(list(popbyage=pop.structure, newstock=additions[-2], offtakes=offtake.str[,-1]))
  # return(list(popbyage=pop.structure, newstock=additions[-2], annofftakes=total_off))
  # return(total.pop)
}


