#' Create a vector of all the variables in the model, the initial parameters for the ode simulator
#'
#' @param parms List of parameters
#'
#' @return A numeric vector, giving the area index S,L_G,I_G,R,F_F for each field at the start of a growing season
initialise_variables<-function(parms){

  with(parms,{

    # Work out the initial crop leaf area index, given the parameters
    initial_crop = crop_max_area_index / (1 + exp(-crop_growth_rate * (0 - crop_growth_midpoint)))

    # Initial variables for a single field
    y = c(initial_crop,rep(0,n_genotypes),rep(0,n_genotypes),0)
    if(n_fungicides > 0) y = c(y,rep(0,n_fungicides))

    # Join together all the fields
    y = rep(y,n_fields)

    # Add names to the variables
    .names = c()
    field_name = c("H.F",
                   paste("L.G", seq_len(parms$n_genotypes), ".F",sep = ""),
                   paste("I.G", seq_len(parms$n_genotypes), ".F",sep = ""),
                   "R.F")
    if (parms$n_fungicides > 0) field_name = c(field_name, paste("D", seq_len(parms$n_fungicides), ".F",sep = ""))
    for (i in seq_len(parms$n_fields)) {
      .names = c(.names, paste(field_name, i, sep = ""))
    }
    names(y) = .names

    return(y)

  })

}
