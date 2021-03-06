
#' An interactive exploration of corresondence analysis objects.
#'
#' \code{launch_pmap} addional thing description
#'
#' Insert description of uses
#' 
#' @export
#' @param .ca A ca object, as returned from the package "ca".
#' @param row_labels A vector of row labels.
#' @param col_labels A vector of column labels.
#' 
launch_pmap <- function(.ca, row_labels = NULL, col_labels = NULL, launch_type = "web"){

  if(inherits(.ca, "ca")){
    if(is.null(row_labels) | is.vector(row_labels)){
      if(is.null(row_labels) | length(row_labels) == nrow(.ca$rowcoord)){
        if(is.null(col_labels) | is.vector(col_labels)){
          if(is.null(col_labels) | length(col_labels) == nrow(.ca$colcoord)){
            
          } else{ stop(paste0(length(col_labels), " column labels given for ", nrow(.ca$colcoord), " coordinates."))}
        } else{ stop("If not NULL, col_labels must be a vector of length equal to the number of .ca$colcoord")}
      } else{ stop(paste0(length(row_labels), " row labels given for ", nrow(.ca$rowcoord), " coordinates."))}
    } else{ stop("If not NULL, row_labels must be a vector of length equal to the number of .ca$rowcoord")}
  } else{ stop(paste0(
    "\n .ca is not a ca object of the ca package.\n I can only work on ca objects.\n Sorry!")
  )}
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Correspondance Analysis Map Explorer", left = NULL),
    miniUI::miniContentPanel(
      shiny::fillCol(flex = c(NA,1), align = "center", height = "100%",
              shiny::sliderInput(inputId = "rotation", label = "Rotation", min = 0, max = 360, value = 0, step = 1),
              shiny::div(style = "position:relative",
                  shiny::plotOutput(outputId = "ca_map", hover = shiny::hoverOpts(id = "pmap_hover_info")),
                  shiny::uiOutput(outputId = "pmap_hover"))
      )
    ))
  
  server <- function(input, output, session) {
    user_ca <- .ca
    
    pmap_df <- shiny::reactive({
      row_coords <- orbit(data.frame(user_ca$rowcoord[,1:2]), input$rotation)
      col_coords <- orbit(data.frame(user_ca$colcoord[,1:2]), input$rotation)
      
      tmp_df <- rbind(cbind.data.frame(type = rep("attributes", nrow(row_coords)), row_coords),
                      cbind.data.frame(type = rep("brands", nrow(col_coords)), col_coords))
      
      if(!is.null(row_labels) & !is.null(col_labels)){
        tmp_df <- cbind(data.frame("labels" = c(row_labels, col_labels), tmp_df))
      } else if(!is.null(row_labels)){
        tmp_df <- cbind(data.frame("labels" = c(row_labels, rep(NA, length(which(tmp_df$type == "brands")))), 
                                                tmp_df))
      } else if(!is.null(col_labels)){
        tmp_df <- cbind(data.frame("labels" = c(rep(NA, length(which(tmp_df$type == "attributes"))), col_labels), 
                                   tmp_df))
      }
      
      tmp_df
    
      })

    output$ca_map <- shiny::renderPlot({
      
      ggplot2::ggplot(data = pmap_df()) +
        ggplot2::geom_point(ggplot2::aes(x = Dim1, y = Dim2, color = type), size = 7) +
        ggplot2::geom_hline(yintercept = 0, color = "black") +
        ggplot2::geom_vline(xintercept = 0, color = "black") +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = ggplot2::element_blank(), legend.position = "none")
    })
    
    output$pmap_hover <- shiny::renderUI({
      if(!is.null(row_labels) | !is.null(col_labels)){
        hover <- input$pmap_hover_info
        if(is.null(hover)) return(NULL)
        
        point <- shiny::nearPoints(pmap_df(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        #tooltip location
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
          tooltip <- paste0("<b>", point$labels, "</b>") 
        
        shiny::wellPanel(style = style, HTML(tooltip))
      }
    })
    
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- "See you next time!"
      shiny::stopApp(returnValue)
    })
  }
  set_launch(ui, server, launch_type)
}
