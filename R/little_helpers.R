#' Build a dataframe of variable names, classes, and labels.
#'
#' \code{get_dflabs}
#'
#' This function is designed for use in the launch_varview GUI.
#'
#' @param .df A data.frame


get_dflabs<-function(.df){

temp_labels<-sapply(.df, function(.col){
  tmp<-attr(.col, "label")
  if(length(tmp)>1){ tmp<-NA }
  tmp
})

data.frame(
  "Name" = names(.df),
  "Class" = sapply(.df, mode),
  "Label" = temp_labels
)

}



#' Build a dataframe of basic descriptives.
#'
#' \code{get_dfdesc}
#'
#' This function is designed for use in the launch_varview GUI.
#'
#' @param .df A data.frame
get_dfdesc<-function(.df){

  data.frame(
    "Name" = names(.df),
    "Percent Complete" = sapply(.df, function(.col){
      round((sum(!is.na(.col))/nrow(.df))*100)}),
    "Mean" = sapply(.df, function(.col){
      if(is.numeric(.col)){
        round(mean(.col, na.rm = TRUE), digits = 2)
      } else{ NA }}),
    "Median" = sapply(.df, function(.col){
      if(is.numeric(.col)){
        median(.col, na.rm = TRUE)
      } else{ NA }}),
    "SD" = sapply(.df, function(.col){
      if(is.numeric(.col)){
        round(sd(.col, na.rm = TRUE), digits = 2)
      } else{ NA }}),
    "Min" = sapply(.df, function(.col){
      if(is.numeric(.col) & sum(!is.na(.col)) > 10){ min(.col, na.rm = TRUE)
      } else{ NA }}),
    "Max" = sapply(.df, function(.col){
      if(is.numeric(.col) & sum(!is.na(.col)) > 10){ max(.col, na.rm = TRUE)
      } else{ NA }})
  )
}
