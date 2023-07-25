#' Calculate the rate of spore influx from all other fields
#'
#' @param y The state variables
#' @param iField The field that we're calculating the spore influx to
#' @param dispersal_proportion The proportion of spores that leave each field
#' @param n_genotypes The number of genotypes
#' @param transmission_rate The transmission rate
#' @param field_size A vector, the size of each field
#' @param n_fields The number of fields
#'
#' @return A numeric containing the total number of spores entering field 'iField'
dispersal<-function(y,iField,dispersal_proportion,n_genotypes,transmission_rate,field_size,n_fields){

  total_spores = 0

  for(jField in seq_len(n_fields)){

    if(iField == jField) next

    total_spores = total_spores +
      dispersal_proportion * variables(y,jField)[(1+n_genotypes+1):(1+2*n_genotypes)] * transmission_rate * field_size[jField] * field_size[iField]

  }

  return(total_spores)

}
