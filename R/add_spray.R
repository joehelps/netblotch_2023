#' Add a foliar treated spray to the state variables
#'
#'This function adds the relevant dose to the state variables
#'Management information is in the `parms$management` data.frame
#'
#' @param .y A vector with the current state variables
#' @param .time A numeric giving the current time
#' @param .year An integer giving the current year
#' @param parms A list containing the simulation parameters
#'
#' @return A vector with the updated state variables
add_spray<-function(.y,.time,.year,parms){

  # Get the management that needs to be applied
  management_i = parms$management[parms$management$Time == .time & parms$management$Year == .year,]

  with(parms,{

    for(row_index in seq_len(nrow(management_i))){

      fungicide_i = management_i$Fungicide[row_index]
      field_i = management_i$Field[row_index]
      dose_i = management_i$Dose[row_index]

      index = grep(paste0("^D",fungicide_i,".*F",field_i,"$"),names(.y))

      .y[index] = .y[index] + dose_i

    }

    return(.y)

  })

}
