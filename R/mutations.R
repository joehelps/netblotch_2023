#' Calculate the amount of mutation in secondary inoculum
#'
#' @param inoculum Current newly produced secondary inoculum
#' @param n_resistance_genes Integer, the number of resistance genes
#' @param n_genotypes Integer, number of genotypes
#' @param mutation_rate Numeric, the mutation rate
#'
#' @return The amount of secondary inoculum of each genotype
mutations<-function(inoculum,n_resistance_genes,n_genotypes,mutation_rate){

  if(n_resistance_genes == 0) return(inoculum)

  # The probability of one gene mutating from one gene to another is mutation_rate
  m_m = mutation_matrix(n_genotypes,mutation_rate,n_resistance_genes)

  new_inoculum = sapply(seq_len(n_genotypes),FUN=function(x) sum(inoculum * m_m[,x]))

  return(new_inoculum)

}
