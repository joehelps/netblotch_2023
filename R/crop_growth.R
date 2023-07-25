#' Calculate the derivative of crop growth
#'
#'The following function calculates the rate of crop growth
#'We are assuming that the canopy dynamics are not affected by the pathogen
#'
#' @param .t The current time (in degree days)
#' @param max_area_index The maximum area index
#' @param growth_rate Growth rate when density is zero
#' @param midpoint Midpoint of the crop growth logistic curve (in degree days)
#'
#' @return A double giving the derivative of the crop area index at time t
#'
crop_growth<-function(.t,max_area_index,growth_rate,midpoint){

  # The growth rate at time t
  g_t = (max_area_index * growth_rate * exp(-growth_rate * (.t-midpoint))) / (1 + exp(-growth_rate * (.t - midpoint)))^2

  return(g_t)

}
