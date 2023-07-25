#' Calculate the frequency of each strain in each field of the given data.frame
#'
#' @param df A data.frame in which the first two columns are the time and year, and then the state variables for each field in turn
#'
#' @return A data.frame with a column for the frequency of each strain in each field. Columns are labelled: "rf_field_'fieldID'_gene_'geneID'".
#' @export
#'
#' @examples
#' parameters(n_years=1) |>
#'    simulate() |>
#'    calculate_strain_frequency()
calculate_strain_frequency<-function(df){

  n_years = max(df$Year)
  n_fields = max(as.numeric(substr(names(df)[grep("H.F",names(df))],4,4)))
  n_elements_field = grep(".*F1$",names(df)) |> length()
  n_genotypes = max(as.numeric(substr(names(df)[grep("L.G",names(df))],4,4)))

  # TODO: I don't think this is correct
  if(n_genotypes == 1){
    n_resistance_genes = 0
  } else if (n_genotypes == 2){
    n_resistance_genes = 1
  } else n_resistance_genes = sqrt(n_genotypes)

  # Make a new data.frame to store the severity of each field
  strain.df = data.frame()

  times = df[,c(1,2,3)]

  # For each field, calculate the severity: (I + R) / (S + L + I + R)
  for(field_i in seq_len(n_fields)){

    # Extract each of the infectious variables
    latent = df |> get_variables(elements="L",field_index = field_i)
    infectious = df |> get_variables(elements="I",field_index = field_i)

    tmp = latent + infectious
    tmp = tmp / rowSums(tmp)

    # Make a long data.frame including all strains
    tmp_df = data.frame()
    for(i in 1:ncol(tmp)){
      if(i==1){
        tmp_df = cbind(times,data.frame(Field=rep(field_i,nrow(times)),Strain=i,Freq=tmp[,i]))
      } else tmp_df = rbind(tmp_df,cbind(times,data.frame(Field=rep(field_i,nrow(times)),Strain=i,Freq=tmp[,i])))
    }

    strain.df = rbind(strain.df,tmp_df)

  }

  return(strain.df)

}
