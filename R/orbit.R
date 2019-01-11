#' Calculate the rotated location of a point.
#'
#' \code{orbit}
#'
#' This function is designed for use in the launch_pmap GUI.
#'
#' @importFrom NISTunits NISTdegTOradian
#' @param coords A data frame containing the x,y coordinates of points to be rotated.
#' @param rotation A numeric value indicating the degreee of rotation.
#' @return a data frame of the same length as coords, containing the rotated x,y coordinates for each point.
#' 
orbit <- function(coords, rotation){

new_x <- sqrt(coords[,1]^2 + coords[,2]^2) * 
  cos(atan2(x = coords[,1], y = coords[,2]) - NISTunits::NISTdegTOradian(rotation))

new_y <- sqrt(coords[,1]^2 + coords[,2]^2) * 
  sin(atan2(x = coords[,1], y = coords[,2]) - NISTunits::NISTdegTOradian(rotation))

loc_df <- cbind.data.frame(new_x, new_y)
names(loc_df) <- names(coords)

loc_df
}