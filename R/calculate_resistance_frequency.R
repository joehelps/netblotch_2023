#' Calculate resistance frequency for each gene in each field of the given data.frame
#'
#' @param df A data.frame in which the first two columns are the time and year, and then the state variables for each field in turn
#'
#' @return A data.frame with a column for the resistance frequency of each gene in each field. Columns are labelled: "rf_field_'fieldID'_gene_'geneID'".
#' @export
#'
#' @examples
#' parameters() |> simulate() |> calculate_resistance_frequency()
calculate_resistance_frequency<-function(df){

  n_years = max(df$Year)
  n_fields = max(as.numeric(substr(names(df)[grep("H.F",names(df))],4,4)))
  n_elements_field = (ncol(df) - 2) / n_fields
  n_genotypes = max(as.numeric(substr(names(df)[grep("L.G",names(df))],4,4)))

  # TODO: I don't think this is correct
  if(n_genotypes == 1){
    n_resistance_genes = 0
  } else if (n_genotypes == 2){
    n_resistance_genes = 1
  } else n_resistance_genes = sqrt(n_genotypes)

  # Make a new data.frame to store the severity of each field
  rf.df = df[,c(1,2,3)]

  # For each field, calculate the severity: (I + R) / (S + L + I + R)
  for(field_i in seq_len(n_fields)){

    # Get the S, L, I, and R variables for this field
    y_i = df[,seq(3 + (field_i - 1) * n_elements_field + 1,3 + field_i * n_elements_field)]

    # Extract each of the infectious variables
    latent = y_i[,1+seq_len(n_genotypes)]
    infectious = y_i[,1+n_genotypes+seq_len(n_genotypes)]

    lesions = latent + infectious

    # Loop over each gene
    for(gene_i in seq_len(n_resistance_genes)){

      # Work out which elements have resistance from this gene
      resistance = ((seq_len(n_genotypes)-1) %/% 2^(gene_i-1)) %% 2

      if(length(which(resistance == 1)) > 1){
        # Work out the frequency of resistance
        freq = rowSums(lesions[,which(resistance==1)]) / rowSums(lesions)
      } else freq = lesions[,which(resistance==1)] / rowSums(lesions)

      rf.df = cbind(rf.df,freq)
      names(rf.df)[length(names(rf.df))] = paste("rf_field",field_i,"_gene",gene_i,sep="")

    }

  }

  return(rf.df)

}
