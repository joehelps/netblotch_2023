#' Get the infectious area index at the end of a growing season
#'
#' @param .result A data.frame as produced within `simulate()`
#' @param parms List of parameters
#'
#' @return A matrix containing the final leaf area index, where each row is a field, and each column a genotype.
final_infectious_area_index<-function(.result,parms){

  with(parms,{

    infectious_area_index = NA

    # If there's nothing in .result, then just return NA
    if(nrow(.result) > 0){

      # Get the last row of the .result data.frame
      last_row = unlist(.result[nrow(.result),])

      # First sum the infectious leaf area index in each field, of each genotype, including from previous years
      infectious_area_index = matrix(0,nrow=n_fields,ncol=n_genotypes)

      for(from_field in 1:n_fields){

        # If this field has zero size, then don't include it.
        if(field_size[from_field] == 0.0) next

        this_field = variables(last_row,from_field)

        # Calculate total area of infectious tissue for this field
        infectious_area_index[from_field,] = this_field[1+n_genotypes+(1:n_genotypes)]

      }

    }

    return(infectious_area_index)

  })

}
