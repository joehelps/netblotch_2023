#' Calculate the effective life
#'
#' @param df A data.frame as returned from `simulate()`
#' @param max_HAD Numeric, the maximum HAD in a simulation with no pathogen
#' @param threshold An optional parameter, giving the threshold acceptable HAD loss as a percentage
#'
#' @return A vector
#' @export
calculate_effective_life<-function(df,max_HAD,threshold = 10){

  # Calculate the HAD for each field each year
  HADs = calculate_HAD(df)

  # Work out whether the HAD in each field/year is greater than the threshold
  eL = HADs
  for(col_i in 2:ncol(HADs)){
    eL[,col_i] = 100 * (1 - HADs[,col_i] / max_HAD) > threshold
  }

  effective_life = apply(eL[,-1,drop=FALSE],MARGIN=2,FUN=function(x){
    if(any(x == FALSE)){
      max(which(x==FALSE))
    } else 0
  })

  return(effective_life)

}
