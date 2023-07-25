#' Create a management data.frame
#'
#' @param field1 The management in field 1: a data.frame with three columns; Fungicide, Dose, Time
#' @param field2 The management in field 2: A data.frame with three columns; Fungicide, Dose, Time
#' @param field3 The management in field 3: A data.frame with three columns; Fungicide, Dose, Time
#' @param n_years An integer, the number of years to run a simulation
#'
#' @return A data.frame, containing all the management events over all the years
#' @export
#'
#' @examples
#' f1 = data.frame(
#'   Fungicide = c(1,1,3),
#'   Dose = 1,
#'   Time = c(0,400,700)
#' )
#' f2 = data.frame(
#'   Fungicide = c(2,1,2,3),
#'   Dose = c(1,0.5,0.5,1),
#'   Time = c(0,400,400,700)
#' )
#' f3 = data.frame(
#'   Fungicide = c(3,3,3),
#'   Dose = 1,
#'   Time = c(0,400,700)
#' )
#' mngmt = create_management(field1 = f1, field2 = f2, field3 = f3,n_years = 2)
create_management<-function(field1 = NULL,field2 = NULL,field3 = NULL,n_years){

  empty_df <- data.frame(Field = numeric(),Fungicide = numeric(),Year = numeric(),Time = numeric(),Dose = numeric())
  if(is.null(field1)){
    field1 = empty_df
  } else {
    field1 = data.frame(Field = 1,Fungicide = field1$Fungicide,Year = 1,Time = field1$Time,Dose = field1$Dose)
  }
  if(is.null(field2)){
    field2 = empty_df
  } else {
    field2 = data.frame(Field = 2,Fungicide = field2$Fungicide,Year = 1,Time = field2$Time,Dose = field2$Dose)
  }
  if(is.null(field3)){
    field3 = empty_df
  } else {
    field3 = data.frame(Field = 3,Fungicide = field3$Fungicide,Year = 1,Time = field3$Time,Dose = field3$Dose)
  }

  tmp_df = rbind(field1,field2,field3)
  df = tmp_df

  # If there's no rows, then just return the empty data.frame
  if(nrow(df) == 0) return(df)

  # Loop over the number of years
  for (i in 2:n_years) {
    tmp_df$Year = i
    df = rbind(df, tmp_df)
  }

  df

}
