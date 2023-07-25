#' Simulate a single year
#'
#' @param year_i Integer giving the current year of the simulation (starting from 1)
#' @param parms The list of parameters in the model
#' @param stubble A list containing the genotype frequencies from the stubble at the end of each year
#'
#' @return A data.frame
simulate_one_year<-function(year_i,parms,stubble){

  # Calculate the genotype frequency of the primary inoculum
  if (year_i == 1) {
    geno_props = initial_geno_freq(parms)
  } else {
    geno_props = genotype_frequencies(stubble,parms)
  }

  # Reset all the state variables - the only thing that changes between seasons is the genotype frequency of the primary inoculum
  .y = initialise_variables(parms)

  # Extract management info for this year
  management_year_i = parms$management[parms$management$Year == year_i,]

  # If there are foliar sprays, then need to stop at each spray
  if(length(management_year_i$Time[management_year_i$Time > 0]) > 0){

    # Make a vector of unique times
    event_times = unique(management_year_i$Time[management_year_i$Time > 0])
    event_times = event_times[order(event_times)]

    for(event_i in seq_along(event_times)){

      if(event_i == 1){

        year_result = deSolve::ode(y=.y,times=seq(0,event_times[event_i]),func=full_model,year=year_i,parms=parms,genotype_proportions=geno_props)

      } else {

        # Add fungicide spray
        .y = add_spray(.y=unlist(year_result[nrow(year_result),-1]),.time=event_times[event_i-1],.year=year_i,parms)
        # Run the integrator
        year_result = rbind(year_result[-nrow(year_result),],
                            deSolve::ode(y = .y,times=seq(event_times[event_i-1],event_times[event_i]),func=full_model,year=year_i,parms=parms,genotype_proportions=geno_props))

      }

    }

    # Finally, if the last spray was before the end of the iterator, do the last section
    .y = add_spray(.y=unlist(year_result[nrow(year_result),-1]),.time=event_times[length(event_times)],.year=year_i,parms)
    # Run the integrator
    if(event_times[event_i] < parms$max_time) year_result = rbind(year_result[-nrow(year_result),],deSolve::ode(y = .y,times=seq(event_times[length(event_times)],parms$max_time),func=full_model,year=year_i,parms=parms,genotype_proportions=geno_props))

  } else year_result = deSolve::ode(y=.y,times=seq(0,parms$max_time),func=full_model,year=year_i,parms=parms,genotype_proportions=geno_props)

  return(as.data.frame(year_result))

}
