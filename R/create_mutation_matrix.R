#' Create a matrix, giving the probability of mutating from genotype i to genotype j
#'
#' @param n_genotypes Integer, the number of genotypes
#' @param mutation_rate The mutation rate
#' @param n_resistance_genes The number of resistance genes
#'
#' @return A matrix of n_genotypes^2
mutation_matrix<-function(n_genotypes,mutation_rate,n_resistance_genes){

  mut_mat = matrix(NA,nrow=n_genotypes,ncol=n_genotypes)

  # Create a punnet square
  for(genotype_i in seq_len(n_genotypes)){

    for(genotype_j in seq_len(n_genotypes)){

      # Work out the allele for each gene
      allele_gene_i = ((genotype_i-1) %/% 2^(seq_len(n_resistance_genes)-1)) %% 2
      allele_gene_j = ((genotype_j-1) %/% 2^(seq_len(n_resistance_genes)-1)) %% 2

      # Work out the proportion that goes from i to j for each gene
      prop = 1

      for(gene_i in seq_len(n_resistance_genes)){

        if(allele_gene_i[gene_i] != allele_gene_j[gene_i]) prop = prop * mutation_rate else prop = prop * (1-mutation_rate)

      }

      mut_mat[genotype_i,genotype_j] = prop

    }

  }

  return(mut_mat)

}
