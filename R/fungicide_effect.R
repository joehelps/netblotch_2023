#' Return the proportional effect of a fungicide on a given genotype
#'
#' @param .dose Current dose
#' @param .genotype Current genotype
#' @param .fung_index Index of fungicide
#' @param dra Matrix, giving all the dose-response curve asymptotes
#' @param drs dose-response curve shapes
#'
#' @return A numeric giving the value of the dose-response curve
fungicide_effect<-function(.dose,.genotype,.fung_index,dra,drs){

  FE = 1 - dra[.fung_index,.genotype] + dra[.fung_index,.genotype] * exp(-drs[.fung_index,.genotype] * .dose)

  return(FE)

}
