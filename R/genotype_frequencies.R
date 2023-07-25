#' Calculate the frequency of each genotype in each field at the start of a year
#'
#' @param stubble A list of matrices, of length `year_i-1`, containing the infectious leaf area at the end of each year
#' @param parms List of parameters
#'
#' @return A matrix, each row of which corresponds to a field. The resistance frequency of the primary inoculum for the coming year
genotype_frequencies<-function(stubble,parms){

  with(parms,{

    # This will contain the amount of spores in the air
    airborne_spores = 0
    # And this the spores staying in each field
    in_field_spores = matrix(0,nrow=n_fields,ncol=n_genotypes)

    # year_i == 1 corresponds to the last year, 2 the penultimate year
    for(year_i in c(1,2)){

      # Get the amount of infectious leaf area at the end of the last year
      infectious_area_index = stubble[[length(stubble) + 1 - year_i]]

      if(all(is.na(infectious_area_index))) next

      # stubble_survival means a proportion of spores comes from the previous year, so (1-stubble_survival) comes from the most recent year
      if(year_i == 1) coef = 1 - stubble_survival else coef = stubble_survival
      if(all(is.na(stubble[[length(stubble)-1]]))) coef = 1.0 # In year 1, the first stubble element will be NA, so don't want to incorporate spores from the previous year

      infectious_area_freq = infectious_area_index / rowSums(infectious_area_index)
      infectious_area_freq[field_size == 0,] = 0

      clonal_spores = infectious_area_freq

      # Calculate frequency in each field from sexual recombination
      D = infectious_area_freq[,1] * infectious_area_freq[,4] - infectious_area_freq[,2] * infectious_area_freq[,3]
      sexual_spores = matrix(0,ncol=n_genotypes,nrow=n_fields)
      sexual_spores[,1] = infectious_area_freq[,1] - D * psi
      sexual_spores[,2] = infectious_area_freq[,2] + D * psi
      sexual_spores[,3] = infectious_area_freq[,3] + D * psi
      sexual_spores[,4] = infectious_area_freq[,4] - D * psi

      # Frequency of all genotypes in field i
      field_freq = (1 - sex_prop) * clonal_spores + sex_prop * sexual_spores

      # The total amount of spores in this field in year year_i:
      field_spores = field_freq * field_size * rowSums(infectious_area_index)

      # Work out the total amount of dispersing spores
      for(field_i in seq_len(n_fields)) if(field_size[field_i] > 0) airborne_spores = airborne_spores + coef * overwinter_dispersal_proportion * field_spores[field_i,]

      # Work out the amount of spores staying in each field
      for(field_i in seq_len(n_fields)) in_field_spores[field_i,] = in_field_spores[field_i,] + coef * (1-overwinter_dispersal_proportion) * field_spores[field_i,]

    }

    # Work out the amount of inoculum in each field
    inoc = matrix(0,nrow=n_fields,ncol=n_genotypes)
    for(field_i in seq_len(n_fields)) inoc[field_i,] = in_field_spores[field_i,] + airborne_spores * field_size[field_i]

    # This prevents dividing by zero
    g_prop = matrix(0,nrow=n_fields,ncol=n_genotypes)
    for(field_i in seq_len(n_fields)) if(field_size[field_i] > 0) g_prop[field_i,] = inoc[field_i,] / rowSums(inoc)[field_i]

    return(g_prop)

  })

}

#' Calculate the genotype frequency in year 1
#'
#' @param parms The parameters object, as produced by `parameters()`
#'
#' @return A matrix containing the frequency of each genotype (column) for each field (row)
initial_geno_freq <- function(parms){

  with(parms,{

    # Calculate the initial proportions from the frequency
    g_prop = rep(1,n_genotypes)

    if(n_resistance_genes > 0){
      for(gene_i in seq_len(n_resistance_genes)){

        # Create a vector of which genotypes are resistant for this gene
        res = (((seq_len(n_genotypes)-1) %/% 2^(gene_i-1)) %% 2)

        g_prop = g_prop * ifelse(res == 0,1-initial_resistance_frequency[gene_i],initial_resistance_frequency[gene_i])

      }

      g_prop = matrix(g_prop,ncol=n_genotypes,nrow=n_fields,byrow=TRUE)

    } else g_prop = matrix(1,nrow=n_fields,ncol=n_genotypes)

    return(g_prop)

  })

}
