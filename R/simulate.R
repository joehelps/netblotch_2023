#' Simulate the model
#'
#' @param parms A list containing all the parameters for the model, created by `parameters()`
#'
#' @return A data.frame, with the first column being the year, and second column time, followed by the state variables
#' @export
simulate <- function(parms) {

  # Check the correctness-ness of all parameters
  check_parameters(parms)

  # The results data.frame will contain the results to return
  result = data.frame()

  # Stubble contains the infectious area index at the end of each season in each field
  stubble = list()

  # Loop over all years
  for (year_i in seq_len(parms$n_years)) {

    # Stubble stores the final infectious area index at the end of each season
    stubble[[year_i]] = final_infectious_area_index(result,parms)

    # Simulate the next year
    one_year_result = simulate_one_year(
      year_i = year_i,
      parms=parms,
      stubble = stubble
    )

    if (year_i == 1) {
      result = cbind(Year = year_i, one_year_result)
    } else result = rbind(result, cbind(Year = year_i, one_year_result))

  }

  field_name = c("H.F",
                 paste("L.G", seq_len(parms$n_genotypes), ".F",sep = ""),
                 paste("I.G", seq_len(parms$n_genotypes), ".F",sep = ""),
                 "R.F")
  if (parms$n_fungicides > 0)
    field_name = c(field_name, paste("D", seq_len(parms$n_fungicides), ".F",
                                     sep = ""))
  result_names = c("Year", "DD")
  for (i in seq_len(parms$n_fields)) {
    result_names = c(result_names, paste(field_name, i, sep = ""))
  }
  names(result) = result_names

  result$time = result$Year + result$DD / (parms$max_time+1)
  result = result[,c(1,2,ncol(result),seq(3,ncol(result)-1))]

  return(result)

}
