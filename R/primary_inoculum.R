#' The rate of primary inoculum influx at time t
#'
#' @param field_index Integer, specifying which of the N fields in the landscape we're calculating for
#' @param .time The current time
#' @param initial_inoculum A vector containing the amount of initial inoculum in each field
#' @param inoculum_shape The shape parameter of the primary inoculum curve
#' @param inoculum_scale The scale parameter of the primary inoculum curve
#' @param genotype_proportions The proportion of each of the genotypes in the simulation at the start of the season
#'
#' @return A numeric giving the total influx of primary inoculum
#' @export
primary_inoculum<-function(.time,field_index,initial_inoculum,inoculum_shape,inoculum_scale,genotype_proportions){

    # Calculate the total influx of inoculum
    inoculum = initial_inoculum[field_index] * (.time^(inoculum_shape-1) * exp(- .time / inoculum_scale)) / (gamma(inoculum_shape) * inoculum_scale^inoculum_shape)

    # Multiply by the genotype frequencies in that field
    inoculum = inoculum * genotype_proportions[field_index,]

    return(inoculum)

}
