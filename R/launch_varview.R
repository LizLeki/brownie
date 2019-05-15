
#' An interactive dataframe explorer!
#'
#' \code{launch_varview} addional thing description
#'
#' The purpose of this gadget is to facilitate quick exploration of the variables contained
#' within a dataframe.
#' @export
#' @param .df A data.frame

launch_varview <- function(.df, launch_type = "web") {

  if(inherits(.df, "data.frame")){

  } else{ stop(paste0(
    "\n .df is not a data.frame. \n I can only work on data.frames. \n Sorry!")
    )}

  ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar("Data.frame Explorer",
                           left = NULL),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Data Labels", icon = shiny::icon(name = "tags", lib = "glyphicon"),
                           miniUI::miniContentPanel(
                             shinycssloaders::withSpinner(shiny::dataTableOutput("label_tbl"), size = 2)
                   )
      ),
      miniUI::miniTabPanel("Variable Descriptives", icon = shiny::icon("calculator"),
                           miniUI::miniContentPanel(
                     shinycssloaders::withSpinner(shiny::dataTableOutput("desc_tbl"), size = 2)
                     )
      ),
      miniUI::miniTabPanel("Variable Explorer", icon = shiny::icon(name = "map-o"),
                           miniUI::miniContentPanel(
                     shiny::fillRow(
                       shiny::fillCol(shiny::selectizeInput("var_name", label = "Variable",
                                                            choices = names(.df),
                                                            options = list(maxOptions = 3000)),
                                      shiny::textOutput(outputId = "unlab")
                               
                                      ),
                       shiny::fillCol(shiny::dataTableOutput(outputId = "varresp_labels"))
                       )
              )
      )
    )
  )

  server <- function(input, output, session) {

    output$label_tbl <- shiny::renderDataTable(get_dflabs(.df))

    output$desc_tbl <- shiny::renderDataTable(get_dfdesc(.df))

    output$unlab <- shiny::renderText({paste0("")})

    output$varresp_labels <- shiny::renderDataTable({

      freq_df <- data.frame(table(.df[[input$var_name]]))
      names(freq_df) <- c("Response", "Frequency")

      tmp_lbls <- attr(.df[[input$var_name]], "labels")

      if(is.null(tmp_lbls)){
        output$unlab <- shiny::renderText({
          paste0("No response labels found for ", input$var_name )
          })
        freq_df
      } else{
        output$unlab <- shiny::renderText({paste0("")})
        lbl_df <- data.frame(
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
    shiny::observeEvent(input$done, {
      returnValue <- "Thanks for playing!"
      shiny::stopApp(returnValue)
    })

  }

  set_launch(ui, server, launch_type)
}
