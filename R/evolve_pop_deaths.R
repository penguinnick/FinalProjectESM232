#' Population Evolution using Leslie Matrix
#' Evolve a population
#' @param fertility fertility rates
#' @param survivability survivability rates
#' @param initialpop initial population
#' @param nstep number of time steps
#' @param offtake animals available for offtake by age group
#' @parms list(offtakerates=matrix(nrow=length(fertility), ncol=length(userrate),0),
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_pop = function(fertility, survivability, initialpop, nstep, offtake) {


nclasses = length(fertility)

# make sure inputs are in the right format
if ((nclasses!=length(survivability) ))
{ return(sprintf("fertility %d doesn’t match survivability %d", 
                 nclasses, length(survivability))) }

if ((nclasses!=length(initialpop) ))
{ return(sprintf("population initialization %d  doesn’t match fertility %d ", length(initialpop),
         length(fertility))) }

#initialize the Leslie matrix
leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
leslie_matrix[,] = 0.0
leslie_matrix[1,] = fertility
offtake_matrix = matrix(nrow=nclasses, ncol=nclasses)
offtake_matrix[,] = 0.0

for (i in 1:(nclasses-1)) {
leslie_matrix[i+1,i] = survivability[i]
offtake_matrix[i+1,i] = offtake[i]
}
leslie_matrix[nclasses,nclasses] = survivability[nclasses]


# create an matrix to store population structure
pop_structure = matrix(nrow=nclasses, ncol=nstep)
offtake_str = matrix(nrow=nclasses, ncol=nstep)
total_pop = rep(0, times=nstep)
maxofftake = rep(0, times=nstep)
pop_structure[,1] = initialpop
offtake_str[,1] = initialpop

for (i in 2:nstep) {

total_pop[i]=sum(pop_structure[,i-1])
maxofftake[i]=sum(offtake_str[,i-1])
pop_structure[,i] = leslie_matrix %*% pop_structure[,i-1]
offtake_str[,i]= leslie_matrix %*% offtake_matrix %*% pop_structure[,i-1] 

}

return(list(popbyage=pop_structure, poptot=total_pop,  culledpop=offtake_str))
}


