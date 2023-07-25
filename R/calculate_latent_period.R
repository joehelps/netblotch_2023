#' Calculate the latent period at a given time, based on the fungicide dose
#'
#' @param fungicide_doses Vector of numeric. The dose of each fungicide
#' @param n_genotypes Integer, the number of genotypes
#' @param n_fungicides Integer, the number of fungicides
#' @param latent_period Numeric, the latent period
#' @param dose_response_asymptote The asymptotes of the dose-response curves
#' @param dose_response_shape The shapes of the dose-response curves
#'
#' @return A vector giving the latent period for each genotype
latent_period<-function(fungicide_doses,n_genotypes,n_fungicides,latent_period,dose_response_asymptote,dose_response_shape){

  # Work out a coefficient for each genotype
  coef_genotype = rep(1,n_genotypes)

  # Add the effect of each foliar applied fungicide
  for(iF in seq_len(n_fungicides)){
    for(iG in seq_len(n_genotypes)) coef_genotype[iG] = coef_genotype[iG] * fungicide_effect(.dose=fungicide_doses[iF],.genotype=iG,iF,dose_response_asymptote,dose_response_shape)
  }

  return(latent_period / coef_genotype)

}
