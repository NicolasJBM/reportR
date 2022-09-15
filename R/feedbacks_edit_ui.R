#' @name feedbacks_edit_ui
#' @title Edit feedback
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of explanations associated with answers students could provide.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified feedback in the file "2_documents/feedbacks.RData".
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput
#' @export


feedbacks_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        6,
        shinyWidgets::radioGroupButtons(
          inputId = ns("slcttypequest"),label = "Type", 
          choices = c(
            "Statements", "Alternatives", "Computation", "Essay", "Problem"
          ), status = "primary", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      ),
      shiny::column(4, shiny::uiOutput(ns("selectdocument"))),
      shiny::column(
        2,
        shiny::actionButton(
          ns("savefeedbacks"), "Save", icon = shiny::icon("save"),
          style = "background-color:#009933;color:#FFF;width:100%;margin-top:25px;" 
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        rhandsontable::rHandsontableOutput(ns("edit_feedbacks"))
      )
    )
  )
}
