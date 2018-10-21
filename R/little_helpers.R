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
  if(length(tmp)>1 | is.null(tmp)){ tmp<-NA }
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

#' Initiate a new Rproject from the console.
#'
#' \code{initiate_project}
#'
#' This function is designed for use in the launch_project GUI. By default,
#' it creates a folder inside the working directory with a name equal to the character
#' value given to the proj_name argument (this behavior can be suppressed by setting 
#' the argument create_folder = FALSE). A .Rproj file is then created and initialized 
#' inside the folder (or working directory, if create_folder = FALSE).
#'
#' @param proj_name a character string to be used as the name of the .Rproj file and directory
#' @param create_folder a logical value, whether to create a folder to hold the .Rproj file (TRUE) or
#' (FALSE) place the .Rproj folder directly in the current working directory.
#' 
initialize_project<-function(proj_name, create_folder = TRUE){

dir_check<-dir.exists(proj_name)

if(create_folder){
  if(dir_check){ 
    stop(paste0("Directory ", proj_name, " exists in working directory."))
  } 
  
  dir.create(proj_name, showWarnings = FALSE)
} else if(!dir_check){
  stop(paste0("No ", proj_name, " folder found in working directory."))
}

rproj_path<-paste0(proj_name, "/", proj_name, ".Rproj")

rproj_check<-file.exists(rproj_path)

if(rproj_check){ stop("Project already exists.")}

rproj_settings<-c("Version: 1.0",
                  "",
                  "RestoreWorkspace: Default",
                  "",
                  "SaveWorkspace: Default",
                  "AlwaysSaveHistory: Default",
                  "",
                  "EnableCodeIndexing: Yes",
                  "UseSpacesForTab: Yes",
                  "NumSpacesForTab: 2",
                  "Encoding: UTF-8",
                  "",
                  "RnwWeave: knitr",
                  "LaTeX: pdfLaTeX"
)

#output .Rproj
cat(
  paste(rproj_settings, collapse = "\n"),
  file = rproj_path
)

browseURL(rproj_path)
}
