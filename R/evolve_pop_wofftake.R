#' Population Evolution using Leslie Matrix
#' Evolve a population
#' @param fertility fertility rates
#' @param survivability survivability rates
#' @param initialpop initial population
#' @param nstep number of time steps
#' @param offtake animals available for offtake by age group as percent of initial pop
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_pop_wofftake = function(fertility, survivability, initialpop, nstep, offtake) {
  
  nclasses = length(fertility)
  
  # make sure inputs are in the right format
  if ((nclasses!=length(survivability) )){ 
    return(sprintf("fertility %d doesnt match survivability %d", nclasses, length(survivability))) 
  }
  if ((nclasses!=length(initialpop) )){ 
    return(sprintf("population initialization %d  doesnt match fertility %d", length(initialpop),length(fertility))) 
  }
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = fertility
  offtake_matrix = matrix(nrow=nclasses, ncol=nclasses)
  offtake_matrix[,] = 0.0
  # natural_deaths = matrix(nrow=nclasses, ncol=nstep)
  # natural_deaths[,] = 0.0
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivability[i]
    # natural_deaths[i+1,i] = (1-survivability[i])
  }
  leslie_matrix[nclasses,nclasses] = survivability[nclasses]
    # create an matrix to store population structure
  pop_structure = matrix(nrow=nclasses, ncol=nstep)
  offtake_str = matrix(nrow=nclasses, ncol=nstep)
  total_pop = rep(0, times=nstep)
  total_off = rep(0, times=nstep)
  additions = rep(0, times=nstep)
  # popchange = rep(0, times=nstep)
  maxofftake = rep(0, times=nstep)
  pop_structure[,1] = initialpop
  offtake_str[,1] = initialpop
  for (i in 2:nstep) {
    # pop_structure[,i]=pop_structure[,i-1] - (pop_structure[,i-1]*offtake)
    pop_structure[,i] = (leslie_matrix %*% pop_structure[,i-1]) - (pop_structure[,i-1]*offtake)
    offtake_str[,i] = (pop_structure[,i-1]*offtake) #+ ((natural_deaths%*%pop_structure[,i-1])/2)
    offtake_str[,i] = ifelse(offtake_str[,i]<0, 0, offtake_str[,i])
    # popchange[i]=pop_structure[,i-1] - pop_structure[,i]
    total_pop[i]=sum(pop_structure[,i-1])
    # total_off[i]=sum(offtake_str[,i-1])
    total_off[i]=sum(offtake_str[,i])
    additions[i]=sum(total_pop[i]-total_pop[i-1])
     #-(offtake_matrix  %*% pop_structure[,i-1])
    # natural_deaths[,i] = 
    # offtake_str[,i]= offtake_matrix  %*% pop_structure[,i-1] 
    }
  # return(list(popbyage=pop_structure, poptot=total_pop, offtakes=offtake_str[,-1], offtot=total_off, newstock=additions))
  return(list(popbyage=pop_structure, poptot=total_pop, offtakes=offtake_str[,-1], offtot=total_off[nstep], newstock=additions))
  # return(list(popbyage=pop_structure, poptot=total_pop,  culledpop=offtake_str))
}



