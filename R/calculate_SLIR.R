#' Calculate S, L, I, R leaf area index for each field
#'
#' @param sim A data.frame, as produced by `simulate()`
#' @param year (optional) an integer, specifying a single year to extract
#'
#' @return A data.frame containing the values of each variable in each field in the model
#' @export
#'
#' @examples
#' simulate(parameters()) |>
#'     calculate_SLIR(year=2)
calculate_SLIR<-function(sim,year=NA){

  n_fields = max(as.numeric(substr(names(sim)[grep("H.F",names(sim))],4,4)))
  n_genotypes = max(as.numeric(substr(names(sim)[grep("L.G",names(sim))],4,4)))
  n_elements_field = grep(".*F1$",names(sim)) |> length()

  if(!is.na(year)){
    sim = sim[sim$Year==year,]
  }

  for(i in seq_len(n_fields)){

    # Extract the susceptible data
    susceptible = sim |> get_variables(elements="H",field_index = i) |> rowSums()
    latent = sim |> get_variables(elements="L",field_index = i) |> rowSums()
    infectious = sim |> get_variables(elements="I",field_index = i) |> rowSums()
    removed = sim |> get_variables(elements="R",field_index = i) |> rowSums()

    if(i==1){
      result = data.frame(Year = sim$Year,DD = sim$DD,time=sim$time,Field = i,H=susceptible,L = latent,I = infectious,R = removed)
    } else result = rbind(result,data.frame(Year = sim$Year,DD = sim$DD,time=sim$time,Field = i,H=susceptible,L = latent,I = infectious,R = removed))

  }

  result

}
