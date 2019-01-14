
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
    
    pmap_df <- reactive({
      row_coords <- orbit(data.frame(user_ca$rowcoord[,1:2]), input$rotation)
      col_coords <- orbit(data.frame(user_ca$colcoord[,1:2]), input$rotation)
      
      rbind(cbind.data.frame(type = rep("attributes", nrow(row_coords)), row_coords),
            cbind.data.frame(type = rep("brands", nrow(col_coords)), col_coords))
    
      })

    output$ca_map <- renderPlot({
      
      ggplot(data = pmap_df()) +
        geom_point(aes(x = Dim1, y = Dim2, color = type), size = 7) +
        geom_hline(yintercept = 0, color = "black") +
        geom_vline(xintercept = 0, color = "black") +
        labs(x = "", y = "") +
        theme_minimal() +
        theme(axis.text = element_blank(), legend.position = "none")
    })
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- "See you next time!"
      stopApp(returnValue)
    })
  }
  runGadget(ui, server, viewer = paneViewer())
}
