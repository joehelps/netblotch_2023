#' Calculate the transmission rate at time t in field i, given the current fungicide dose
#'
#' @param fungicide_doses A vector giving the dose of each fungicide in field i
#' @param transmission_rate The default transmission rate
#' @param transmission_rate_fitness_cost A fitness cost associated with the transmission rate. Values of 0.0 indicate no fitness cost, while values of 1.0 indicate a full fitness cost.
#' @param n_genotypes Integer, the number of genotypes
#' @param n_fungicides Integer, the number of fungicides
#' @param dose_response_asymptote Matrix, the asymptotes of the dose-response curves
#' @param dose_response_shape Matrix, the shapes of the dose-response curves
#'
#' @return A vector giving the transmission rate of each genotype
transmission_rate<-function(fungicide_doses,transmission_rate,transmission_rate_fitness_cost,n_genotypes,n_fungicides,dose_response_asymptote,dose_response_shape){

  # Work out a coefficient for each genotype
  coef_genotype = rep(1,n_genotypes)

  # Add the effect of each foliar applied fungicide
  for(iF in seq_len(n_fungicides)){
    for(iG in seq_len(n_genotypes)){
      # If this genotype is resistant, calculate the fitness cost
      if(iG == 2 | iG == 3){
        trfc = 1 - transmission_rate_fitness_cost
      } else if(iG == 4){
        trfc = (1 - transmission_rate_fitness_cost)^2
      } else trfc = 1 - 0.0
      # Calculate the transmission rate for this genotype
      coef_genotype[iG] = coef_genotype[iG] * trfc * fungicide_effect(.dose=fungicide_doses[iF],.genotype=iG,iF,dose_response_asymptote,dose_response_shape)
    }
  }

  return(transmission_rate * coef_genotype)

}
