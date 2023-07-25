#' Create a parameters object.
#'
#' Creates a list containing all the parameters that can be set to run the NetBlotch model.
#'
#' @param crop_growth_midpoint The midpoint of the growth curve, in degree days since crop emergence
#' @param crop_growth_rate The growth rate of the crop canopy (leaf area per degree day)
#' @param crop_max_area_index The maximum area index of the crop canopy (leaf area per unit ground area)
#' @param crop_senesence_midpoint The midpoint of crop senescence
#' @param transmission_rate The transmission rate of the pathogen
#' @param latent_period Latent period of the pathogen
#' @param infectious_period Infectious period of the pathogen
#' @param sporulation_rate The sporulation rate
#' @param initial_resistance_frequency The initial resistance frequency, a vector of length n_resistance_genes
#' @param inoculum_scale A numeric, the scale parameter of inoculum release
#' @param inoculum_shape A numeric, the shape parameter of inoculum release
#' @param mutation_rate A numeric, between 0 and 1, the mutation rate
#' @param overwinter_dispersal_proportion The proportion of spores that overwinter
#' @param spore_escape_ratio The proportion of spores that escape
#' @param stubble_survival The proportion of stubble survival
#' @param field_size A vector, containing the size of each field in the landscape
#' @param initial_inoculum A vector of size 3 specifying the amount of inoculum at the start of each season in each field
#' @param dose_response_asymptote A matrix, containing the asymptote of the dose-response relationship for each fungicide:genotype pair. The number of rows of the matrix should be the number of fungicides, and the number of columns should equal the number of genotypes
#' @param dose_response_shape A matrix, containing the shape of the dose-response relationship for each fungicide:genotype pair. The number of rows of the matrix should be the number of fungicides, and the number of columns should equal the number of genotypes
#' @param decay_rate A vector, containing the decay rate of each fungicide
#' @param seed_treatment_coefficient A vector with an element for each fungicide, containing the coefficient of the seed treatment for each fungicide
#' @param seed_treatment_scale A numeric, being the scale parameter of the seed treatment release for all fungicides
#' @param seed_treatment_shape A numeric, being the shape parameter of the seed treatment release for all fungicides
#' @param sex_prop # Proportion of overwinter spores that go through sexual recombination
#' @param psi # Linkage disequilibirum coefficient
#' @param management (optional) A data.frame specifying when fungicide is applied to each year and each field. Required columns are: Year, Field, Fungicide, Dose, Time
#' @param n_fields An integer, giving the number of fields
#' @param n_fungicides An integer, specifying the number of fungicides
#' @param n_resistance_genes An integer, specifying the number of resistance genes
#' @param n_years An integer, specifying the number of years to simulate
#' @param punnet_square An empty matrix
#' @param max_time An integer, specifying the maximum time (in degree days) to simulate within each growing season
#' @param transmission_fitness_cost The fitness cost of the transmission rate, between 0 and 1
#'
#' @return A list, containing parameter values to be passed to `simulate()`
#' @export
parameters<-function(
  # Crop parameters
  crop_growth_midpoint = 550,
  crop_growth_rate = 0.01,
  crop_max_area_index = 2.75,
  crop_senesence_midpoint = 1050,
  # Pathogen parameters
  transmission_rate = 0.1172795,
  transmission_fitness_cost = 0.0,
  latent_period = 225,
  infectious_period = 225,
  sporulation_rate = 0.4001843,
  initial_resistance_frequency = rep(0, 2),
  inoculum_scale = 40,
  inoculum_shape = 1,
  mutation_rate = 1e-7,
  overwinter_dispersal_proportion = 1,
  spore_escape_ratio = 0.1,
  stubble_survival = 0.1,
  sex_prop = 0.0,
  psi = 0.5,
  # Spatial parameters
  field_size = c(0.5, 0.25, 0.25),
  initial_inoculum = rep(0.1304277, 3),
  # Fungicide parameters
  dose_response_asymptote = matrix(c(
    0.68813,0.8563,0.68813,0.8563,
    0.8604,0.8604,0.6,0.6,
    0.6,0.6,0.6,0.6
    ), ncol = 4, nrow = 3,byrow=TRUE),
  dose_response_shape = matrix(c(
    6.26876,0.255,6.26876,0.255,
    27.88,27.88,2.4,2.4,
    4,4,4,4
  ), ncol = 4, nrow = 3,byrow=TRUE),
  decay_rate = c(0.006, 0.00513, 0.006),
  seed_treatment_coefficient = c(0.248778,0.7984493,0.961949),
  seed_treatment_scale = 20,
  seed_treatment_shape = 4,
  # Management parameters
  management = NULL,
  # Simulation parameters
  n_fields = 3,
  n_fungicides = 3,
  n_resistance_genes = 2,
  n_years = 20,
  punnet_square = matrix(NA, ncol = 4, nrow = 4),
  max_time = 1200
){

  if(is.null(management)) management = create_management(n_years = n_years)

  # Check that if the number of resistance genes is reduced then everything associated is the correct dimension
  n_genotypes = 2^n_resistance_genes
  if(n_resistance_genes < 2){
    if(ncol(dose_response_asymptote) > n_genotypes) dose_response_asymptote = dose_response_asymptote[,seq_len(n_genotypes),drop=FALSE]
    if(ncol(dose_response_shape) > n_genotypes) dose_response_shape = dose_response_shape[,seq_len(n_genotypes),drop=FALSE]
    if(length(initial_resistance_frequency) != n_resistance_genes) initial_resistance_frequency = initial_resistance_frequency[seq_len(n_resistance_genes)]
  }

  # Check if the number of fields is reduced that everything is correctly changed
  if(n_fields < 3){
    if(length(initial_inoculum) > n_fields) initial_inoculum = initial_inoculum[seq_len(n_fields)]
    if(length(field_size) > n_fields) field_size = field_size[seq_len(n_fields)]
  }

  # And the number of fungicides:
  if(n_fungicides < 3){
    if(ncol(dose_response_asymptote) > n_fungicides) dose_response_asymptote = dose_response_asymptote[seq_len(n_fungicides),,drop=FALSE]
    if(ncol(dose_response_shape) > n_fungicides) dose_response_shape = dose_response_shape[seq_len(n_fungicides),,drop=FALSE]
    if(length(decay_rate) > n_fungicides) decay_rate = decay_rate[seq_len(n_fungicides)]
    management = management[management$Fungicide <= n_fungicides,]
  }

  parms = list(

    # Crop parameters
    crop_growth_midpoint = crop_growth_midpoint,
    crop_growth_rate = crop_growth_rate,
    crop_max_area_index = crop_max_area_index,
    crop_senesence_midpoint = crop_senesence_midpoint,

    # Pathogen parameters
    transmission_rate = transmission_rate,
    transmission_fitness_cost = transmission_fitness_cost,
    latent_period = latent_period,
    infectious_period = infectious_period,
    sporulation_rate = sporulation_rate,
    initial_resistance_frequency = initial_resistance_frequency,
    inoculum_scale = inoculum_scale,
    inoculum_shape = inoculum_shape,
    mutation_rate = mutation_rate,
    overwinter_dispersal_proportion = overwinter_dispersal_proportion,
    spore_escape_ratio = spore_escape_ratio,
    stubble_survival = stubble_survival,
    sex_prop = sex_prop,
    psi = psi,

    # Spatial parameters
    field_size = field_size,
    initial_inoculum = initial_inoculum,

    # Fungicide parameters
    dose_response_asymptote = dose_response_asymptote,
    dose_response_shape = dose_response_shape,
    decay_rate = decay_rate,
    seed_treatment_coefficient = seed_treatment_coefficient,
    seed_treatment_scale = seed_treatment_scale,
    seed_treatment_shape = seed_treatment_shape,

    # Management parameters
    management = management,

    # Simulation parameters
    n_fields = n_fields,
    n_fungicides = n_fungicides,
    n_resistance_genes = n_resistance_genes,
    n_genotypes = n_genotypes,
    n_years = n_years,
    punnet_square = punnet_square,
    max_time = max_time
  )

  return(parms)

}
