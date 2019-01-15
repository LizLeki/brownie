#' Launch a GUI
#'
#' \code{set_launch}
#'
#' This function is designed for use in the brownie GUIs.
#'
#' @param launch_type A data frame containing the x,y coordinates of points to be rotated.
#' 
set_launch <- function(ui, server, launch_type){
  if(is.character(launch_type) & length(launch_type == 1)){
    if((launch_type %in% c("pane", "web"))){
      
    } else{ stop("launch_type must be one of 'pane' or 'web'")}
  } else{ stop("launch_type must be a character vector of length one.")}
  
  if(launch_type == "pane") runGadget(ui, server, viewer = paneViewer())
  if(launch_type == "web") runGadget(ui, server, viewer = browserViewer())
  
}