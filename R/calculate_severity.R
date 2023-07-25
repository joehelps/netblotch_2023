#' Calculate the severity in each field at each time point
#'
#' @param df A data.frame returned from simulate()
#' @param severity_type An integer specifying the equation for severity:
#' 1 = I / (S + L + I)
#' 2 = I / (S + L + I + R)
#' 3 = (L + I) / (S + L + I)
#' 4 = (L + I) / (S + L + I + R)
#' 5 = (L + I + R) / (S + L + I + R)
#'
#' @return A data.frame containing a severity column for each field
#' @export
#'
#' @examples
#' parameters() |>
#'   simulate() |>
#'   calculate_severity()
calculate_severity<-function(df,severity_type = 1){

  n_fields = max(as.numeric(substr(names(df)[grep("H.F",names(df))],4,4)))
  n_elements_field = grep(".*F1$",names(df)) |> length()
  n_genotypes = max(as.numeric(substr(names(df)[grep("L.G",names(df))],4,4)))

  # Make a new data.frame to store the severity of each field
  severity.df = df[,c(1,2,3)]

  # For each field, calculate the severity: (I + R) / (S + L + I + R)
  for(field_i in seq_len(n_fields)){

    # Extract each of the infectious variables
    susceptible = get_variables(df,elements="H",field_index = field_i) |> rowSums()
    latent = get_variables(df,elements = "L",field_index = field_i) |> rowSums()
    infectious = get_variables(df,elements = "I",field_index = field_i) |> rowSums()
    removed = get_variables(df,elements="R",field_index = field_i) |> rowSums()

    sev = switch(severity_type,
                 infectious / (susceptible + latent + infectious),
                 infectious / (susceptible + latent + infectious + removed),
                 (latent + infectious) / (susceptible + latent + infectious),
                 (latent + infectious) / (susceptible + latent + infectious + removed),
                 (latent + infectious + removed) / (susceptible + latent + infectious + removed)
    )

    severity.df = cbind(severity.df,sev)

    names(severity.df)[ncol(severity.df)] = paste("field",field_i,sep=".")

  }

  return(severity.df)

}
