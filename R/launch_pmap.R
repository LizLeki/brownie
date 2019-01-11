
#' An interactive exploration of corresondence analysis objects.
#'
#' \code{launch_pmap} addional thing description
#'
#' Insert description of uses
#' 
#' @export
#' @import shiny
#' @import miniUI
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @param .df A data.frame
#' 
launch_pmap <- function(.ca){
  #this really shouldn't be an issue
  shiny_check<-require("shiny", quietly = TRUE)
  mini_check<-require("miniUI", quietly = TRUE)
  gg_check<-require("ggplot2", quietly = TRUE)
  
  if(inherits(.ca, "ca")){
    if(shiny_check){
      if(mini_check){
        #move along
      } else{ stop("You need to install miniUI before using this app.")}
    } else{ stop("You need to install shiny before using this app.")}
  } else{ stop(paste0(
    "\n .ca is not a ca object of the ca package.\n I can only work on ca objects.\n Sorry!")
  )}
  
  ui <- miniPage(
    gadgetTitleBar("Correspondance Analysis Map Explorer", left = NULL),
    miniContentPanel(
      fillCol(flex = c(NA,1), align = "center", height = "100%",
              sliderInput(inputId = "rotation", label = "Rotation", min = 0, max = 360, value = 0, step = 1),
              fillRow(plotOutput(outputId = "ca_map", height = "100%"))
      )
    ))
  
  server <- function(input, output, session) {
    user_ca <- .ca
    
    pmap_atts <- reactive({
      row_coords <- cbind.data.frame(user_ca$rowcoord[,1:2])
      
      orbit(row_coords, input$rotation)
    })
    
    pmap_brands <- reactive({
      col_coords <- cbind.data.frame(user_ca$colcoord[,1:2])
      
      orbit(col_coords, input$rotation)
    })
    
    output$ca_map <- renderPlot({
      
      ggplot() +
        geom_point(data = pmap_atts(), aes(x = Dim1, y = Dim2), color = "blue", size = 7) +
        geom_point(data = pmap_brands(), aes(x = Dim1, y = Dim2), color = "#AC2214", size = 7) +
        geom_hline(yintercept = 0, color = "black") +
        geom_vline(xintercept = 0, color = "black") +
        labs(x = "", y = "") +
        theme_minimal() +
        theme(axis.text = element_blank())
    })
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- "See you next time!"
      stopApp(returnValue)
    })
  }
  runGadget(ui, server, viewer = paneViewer())
}
