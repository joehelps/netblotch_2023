#' Function passed to ode, returning the derivatives of all state variables
#'
#'The parameters are those needed for the ode solver, deSolve::ode()
#'
#' @param t Numeric, the current time
#' @param y The current state variables
#' @param parms A list containing all the model parameters
#' @param year Integer, giving the year
#' @param genotype_proportions Genotype proportions of the initial inoculum for this year
#'
#' @return A list, whose first element is a vector of the derivatives
full_model<-function(t,y,parms,year,genotype_proportions){

  with(parms,{

    # Make a vector to store the derivatives
    dy = numeric()

    # Work out each field in turn
    for(iField in seq_len(n_fields)){

      # Extract the variables for field iField
      y.f = variables(y,iField)
      dy.f = rep(0,length(y.f))

      # Name the variable indexes
      index_healthy = 1
      index_latent = 1 + seq_len(n_genotypes)
      index_infectious = 1 + n_genotypes + seq_len(n_genotypes)
      index_removed = 1 + 2 * n_genotypes + 1
      index_fungicide = numeric()
      if(n_fungicides > 0) index_fungicide =  1 + 2 * n_genotypes + 1 + seq_len(n_fungicides)

      # **** Effect of fungicide
      if(n_fungicides > 0){
        latent_period_genotype = latent_period(fungicide_doses=y.f[index_fungicide],n_genotypes,n_fungicides,latent_period,dose_response_asymptote,dose_response_shape)
        transmission_rate_genotype = transmission_rate(y.f[index_fungicide],transmission_rate,transmission_fitness_cost,n_genotypes,n_fungicides,dose_response_asymptote,dose_response_shape)
      } else {
        latent_period_genotype = rep(latent_period,n_genotypes)
        transmission_rate_genotype = rep(transmission_rate,n_genotypes)
      }

      # **** Inoculum

      # Primary inoculum
      primaryInoculum = primary_inoculum(.time=t,field_index = iField,initial_inoculum,inoculum_shape,inoculum_scale,genotype_proportions)

      # Secondary inoculum
      secondaryInoculum = y.f[index_infectious] * sporulation_rate

      # And mutations...
      secondaryInoculum = mutations(secondaryInoculum,n_resistance_genes,n_genotypes,mutation_rate)

      # Total inoculum
      total_inoculum = primaryInoculum + secondaryInoculum

      # Calculate crop canopy
      dy.f[index_healthy] = crop_growth(t,crop_max_area_index,crop_growth_rate,crop_growth_midpoint) -
        crop_senescence(t,crop_max_area_index,crop_growth_rate,crop_senesence_midpoint) * y.f[index_healthy] / sum(y.f[c(index_healthy,index_latent,index_infectious)])

      # Calculate infection derivatives
      for(iG in seq_len(n_genotypes)){
        # Work out the number of new latent spores
        new_latent = y.f[index_healthy] / sum(y.f[c(index_healthy,index_latent,index_infectious)]) * total_inoculum[iG] * transmission_rate_genotype[iG]
        # Healthy
        dy.f[index_healthy] = dy.f[index_healthy] - new_latent
        # Latent
        dy.f[index_latent][iG] = new_latent - y.f[index_latent][iG] / latent_period_genotype[iG] - crop_senescence(t,crop_max_area_index,crop_growth_rate,crop_senesence_midpoint) * y.f[index_latent][iG] / sum(y.f[c(index_healthy,index_latent,index_infectious)])
        # Infectious
        dy.f[index_infectious][iG] = y.f[index_latent][iG] / latent_period_genotype[iG] - y.f[index_infectious][iG] / infectious_period
        # Removed
        dy.f[index_removed] = dy.f[index_removed] + y.f[index_infectious][iG] / infectious_period
      }

      # Calculate fungicide rates
      for(iF in seq_len(n_fungicides)){
        # Addition from seed treatment.year,.field,.t,.fungicide
        dy.f[index_fungicide][iF] = seed_treatment_deriv(.year=year,.field = iField,.t = t,.fungicide = iF,management,seed_treatment_shape,seed_treatment_scale,seed_treatment_coefficient)
        # Decay
        dy.f[index_fungicide][iF] = dy.f[index_fungicide][iF] - y.f[index_fungicide][iF] * decay_rate[iF]
      }

      # Add to the derivatives vector
      dy[((iField-1)*n_elements_field+1):(iField * n_elements_field)] = dy.f

    }

    return(list(dy))

  })

}
