#' Calculate the influx of dose from seed treatment at time t
#'
#' This function returns the rate of influx of seed treatment at time t
#'
#' @param .year A numeric, giving the current year. Needed to get the correct spray
#' @param .field An integer giving the field number
#' @param .t A double giving the current time
#' @param .fungicide An integer specifying the fungicide
#' @param management data.frame giving the management details
#' @param seed_treatment_shape Shape of seed treatment
#' @param seed_treatment_scale Scale of seed treatment
#' @param seed_treatment_coefficient Coefficient of seed treatment
#'
#' @return A single double, giving the derivative for a single fungicide in field .field at time .t
seed_treatment_deriv<-function(.year,.field,.t,.fungicide,management,seed_treatment_shape,seed_treatment_scale,seed_treatment_coefficient){

  # Get the management info for this field and year
  .management = management[management$Year == .year & management$Field == .field & management$Fungicide == .fungicide,]

  deriv = 0

  # Check that there is a spray applied at time == 0
  if(sum(.management$Time == 0) > 0){
    deriv = seed_treatment_coefficient[.fungicide] * .management$Dose[.management$Time==0] * (.t^(seed_treatment_shape-1) * exp(-.t/seed_treatment_scale)) / (gamma(seed_treatment_shape)*(seed_treatment_scale^seed_treatment_shape))
  }

  return(deriv)

}
