#' Calculate the rate of crop senescence at time t
#'
#' @param .t Time
#' @param max_area_index Maximum area index
#' @param growth_rate Crop growth rate
#' @param midpoint Midpoint of the senescence curve
#'
#' @return A numeric
crop_senescence<-function(.t,max_area_index,growth_rate,midpoint){

  # The senescence rate at time t
  s_t = (max_area_index * growth_rate * exp(-growth_rate * (.t-midpoint))) / (1 + exp(-growth_rate * (.t - midpoint)))^2

  return(s_t)

}
