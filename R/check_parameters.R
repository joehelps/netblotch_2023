#' Check the integrity of the passed parameters
#'
#'This function checks that the dimensions of the parameters passed to `simulate()`
#'
#' @param parameters A list of parameters, as passed to `simulate()`
check_parameters<-function(parameters){

  # Check the simulation parameters
  if(!(length(parameters$field_size) == parameters$n_fields)) stop("field_size vector needs to have a size for each field")

  # Check the pathogen parameters
  if(!(length(parameters$initial_resistance_frequency) == parameters$n_resistance_genes)) stop("initial_resistance_frequency should contain a frequency for each gene")
  if(length(parameters$initial_inoculum) < parameters$n_fields) stop("Need an initial inoculum for each field")

  # Check fungicide parameters
  if(!(length(parameters$decay_rate) == parameters$n_fungicides)) stop("decay_rate should be a vector of length n_fungicides")
  if(any(parameters$dose_response_asymptote > 1)) stop("Dose-response asymptote cannot be greater than 1")
  if(any(parameters$dose_response_asymptote < 0)) stop("Dose-response asymptote cannot be less than 0")
  if(!(ncol(parameters$dose_response_asymptote) == parameters$n_genotypes)) stop("dose_response_asymptote should contain a value for each genotype")
  if(!(ncol(parameters$dose_response_shape) == parameters$n_genotypes)) stop("dose_response_shape should contain a value for each genotype")
  if(!(nrow(parameters$dose_response_asymptote) == parameters$n_fungicides)) stop("dose_response_asymptote should contain a value for each fungicide")
  if(!(nrow(parameters$dose_response_shape) == parameters$n_fungicides)) stop("dose_response_shape should contain a value for each fungicide")

  # Check management data.frame
  if(any(parameters$management$Fungicide > parameters$n_fungicides)) stop("Too many fungicides in the management data.frame")
  if(any(parameters$management$Field > parameters$n_fields)) stop("Too many fields in the management data.frame")

  # Calculate the number of elements per field
  parameters$n_elements_field <- 2 + 2 * parameters$n_genotypes + parameters$n_fungicides
  # Zero the punnet square, so that it's recalculated
  parameters$punnet_square <- matrix(NA,parameters$n_genotypes,parameters$n_genotypes)
  assign("parms",parameters,envir=parent.frame())

}
