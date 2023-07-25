#' Return the variables for a single field
#'
#' @param y The whole vector of all variables for all fields
#' @param field_index An integer giving the number of the desired field
#'
#' @return A double vector with the variables for the single field
variables<-function(y,field_index){

    indices = grep(paste0(".*F",field_index,"$"),names(y))

    return(y[indices])

}
