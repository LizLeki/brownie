#' Initiate a new Rproject from the console.
#'
#' \code{init_project}
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
init_project<-function(proj_name, create_folder = TRUE){
  
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