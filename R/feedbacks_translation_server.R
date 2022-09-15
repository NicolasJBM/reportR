#' @name feedbacks_translation_server
#' @title Translate feedback
#' @author Nicolas Mangin
#' @description Module facilitating the translation of feedback.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param preselection Tibble. List of pre-selected documents.
#' @return Create and save documents' translations in a dedicated folder.
#' @import shiny
#' @importFrom dplyr filter
#' @export


feedbacks_translation_server <- function(id, preselection){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    
    
    
  })
}

