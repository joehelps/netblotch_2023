#' Extract requested variables from a data.frame formatted according to `simulate()`
#'
#' @param .data A data.frame, of the same format of that returned by `simulate()`
#' @param elements A character vector containing one or more of the following: "S", "L", "I", "R", "D"
#' @param field_index (optional) The index of the field to extract
#'
#' @return A data.frame
get_variables <- function(.data,elements,field_index = NULL){

  # Check that .data has the correct starting pattern:
  if(!(names(.data)[1] == "Year" & names(.data)[2] == "DD" & names(.data)[3] == "time")) stop("Error in get_variables. Inappropriate column names supplied.")

  # Check that elements contains at least one of "H", "L", "I", "R", "D"
  if(any(!(elements %in% c("H","L","I","R","D")))) stop(paste("Variable",elements[which(!(elements %in% c("H","L","I","R","D")))],"not in the data.frame"))

  # If field_index isn't supplied, then do all fields
  if(is.null(field_index)){
    n_fields = names(df)[grep("H.F",names(df))] |> substr(4,4) |> as.numeric() |> max()
    field_index = seq(n_fields)
  }

  for(f_i in seq_along(field_index)){
    for(e_i in seq_along(elements)){

      .cols = grep(paste0("^",elements[e_i],".*","F",field_index[f_i],"$"),names(.data))
      if(f_i == 1 & e_i == 1){
        df <- .data[,.cols,drop=FALSE]
      } else df <- cbind(df,.data[,.cols,drop=FALSE])

    }
  }

  df

}
