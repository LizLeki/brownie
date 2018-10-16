
#' An interactive dataframe explorer!
#'
#' \code{launch_varview} addional thing description
#'
#' The purpose of this gadget is to facilitate quick exploration of the variables contained
#' within a dataframe.
#' @export
#' @import shiny
#' @import miniUI
#' @importFrom shinycssloaders withSpinner
#' @param .df A data.frame

launch_varview <- function(.df) {
  #this really shouldn't be an issue
  shiny_check<-require("shiny", quietly = TRUE)
  mini_check<-require("miniUI", quietly = TRUE)

  if(inherits(.df, "data.frame")){
    if(shiny_check){
      if(mini_check){
        #move along
      } else{ stop("You need to install miniUI before using this app.")}
    } else{ stop("You need to install shiny before using this app.")}
  } else{ stop(paste0(
    "\n .df is not a data.frame. \n I can only work on data.frames. \n Sorry!")
    )}

  ui <- miniPage(

    gadgetTitleBar("Data.frame Explorer",
                   left = NULL),
    miniTabstripPanel(
      miniTabPanel("Data Labels", icon = icon(name = "tags", lib = "glyphicon"),
                   miniContentPanel(
                     shinycssloaders::withSpinner(dataTableOutput("label_tbl"), size = 2)
                   )
      ),
      miniTabPanel("Variable Descriptives", icon = icon("calculator"),
                   miniContentPanel(
                     shinycssloaders::withSpinner(dataTableOutput("desc_tbl"), size = 2)
                   )
      ),
      miniTabPanel("Variable Explorer", icon = icon(name = "map-o"),
                   miniContentPanel(
                     fillRow(
                       fillCol(selectizeInput("var_name", label = "Variable",
                                              choices = names(.df),
                                              options = list(maxOptions = 3000)),
                               textOutput(outputId = "unlab")
                               
                       ),
                       fillCol(dataTableOutput(outputId = "varresp_labels"))
                       )
                   )
      )
    )
  )

  server <- function(input, output, session) {

    output$label_tbl<-renderDataTable(get_dflabs(.df))

    output$desc_tbl<-renderDataTable(get_dfdesc(.df))

    output$unlab<-renderText({paste0("")})

    output$varresp_labels<-renderDataTable({

      freq_df<-data.frame(table(.df[[input$var_name]]))
      names(freq_df)<-c("Response", "Frequency")

      tmp_lbls<-attr(.df[[input$var_name]], "labels")

      if(is.null(tmp_lbls)){
        output$unlab<-renderText({
          paste0("No response labels found for ", input$var_name )
          })
        freq_df
      } else{
        output$unlab<-renderText({paste0("")})
        lbl_df<-data.frame(
          "Response" = as.character(tmp_lbls),
          "Label" = names(tmp_lbls)
          )
        dplyr::full_join(x = lbl_df,
                         y = freq_df,
                         by = "Response")
      }
    }, options = list(lengthMenu = list(c(5, 10, 20),
                                        c("5", "10", "20")),
                      pageLength = 5)
    )



    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- "Thanks for playing!"
      stopApp(returnValue)
    })

  }

  runGadget(ui, server, viewer = paneViewer())
}
