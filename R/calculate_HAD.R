#' Calculate the healthy area duration (HAD) of each field each year
#'
#' @param df The data.frame returned from simulate()
#'
#' @return A data.frame giving the HAD in each field for each year
#' @export
calculate_HAD<-function(df){

  n_years = max(df$Year)
  n_fields = names(df)[grep("H.F",names(df))] |>
    substr(4,4) |> as.numeric() |> max()
  n_elements_field = grep(".F1$",names(df)) |> length()

  # Make a new data.frame to store the severity of each field
  HAD.df = data.frame(Year = seq_len(n_years))

  for(field_i in seq_len(n_fields)){

    # Extract the susceptible variables
    susceptible = get_variables(df,elements="H",field_index = field_i)

    HAD_field = rep(NA,n_years)
    # Calculate HAD for each year
    for(year_i in seq_len(n_years)){
      s_y = susceptible[df$Year == year_i,1]
      times = df$DD[df$Year == year_i]
      HAD = 0
      for(i in 2:length(times)) HAD = HAD + 0.5 * (s_y[i] + s_y[i-1]) * (times[i] - times[i-1])
      HAD_field[year_i] = HAD
    }

    HAD.df = cbind(HAD.df,HAD_field)
    names(HAD.df)[ncol(HAD.df)] = paste("field",field_i,sep=".")

  }

  return(HAD.df)

}
