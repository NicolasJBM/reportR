#' @name feedbacks_edit_server
#' @title Edit feedback
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of explanations associated with answers students could provide.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param preselection Tibble. List of pre-selected documents.
#' @param item_parameters Tibble. Statistics associated with each item in the feedbacks table.
#' @return Save the new or modified feedback in the file "2_documents/feedbacks.RData".
#' @import shiny
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom dplyr rename
#' @importFrom dplyr inner_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr desc
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom dplyr mutate_if
#' @importFrom dplyr anti_join
#' @export


feedbacks_edit_server <- function(
  id, preselection, item_parameters
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    success <- NULL
    discrimination <- NULL
    document <- NULL
    title <- NULL
    explanation <- NULL
    item <- NULL
    keywords <- NULL
    language <- NULL
    modifications <- NULL
    proposition <- NULL
    type <- NULL
    value <- NULL
    
    modrval <- shiny::reactiveValues()
    shiny::observe({
      base::load("2_documents/feedbacks.RData")
      modrval$feedbacks <- feedbacks
    })
    
    feedbacks_after_preselection <- shiny::reactive({
      shiny::req(!base::is.null(preselection()))
      shiny::req(base::nrow(preselection()) > 0)
      
      selection <- base::sort(base::unique(c(
        "", preselection()$document, preselection()$code
      )))
      
      modrval$feedbacks |>
        dplyr::filter(document %in% selection | code %in% selection)
    })
    
    feedbacks_after_type_selection <- shiny::reactive({
      shiny::req(!base::is.null(feedbacks_after_preselection()))
      shiny::req(!base::is.null(input$slcttypequest))
      feedbacks_after_preselection() |>
        dplyr::filter(type == input$slcttypequest)
    })
    
    output$selectdocument <- shiny::renderUI({
      shiny::req(!base::is.null(input$slcttypequest))
      shiny::isolate({
        shiny::req(!base::is.null(preselection()))
        tmpslct <- preselection() |>
          dplyr::select(code, document, title) |>
          base::unique()
      })
      if (base::nrow(tmpslct) > 0){
        codes <- c("", tmpslct$code)
        base::names(codes) <- c("select", base::paste0(
          tmpslct$code, " - ", tmpslct$title
        ))
        documents <- c("", tmpslct$document)
        base::names(documents) <- c("select", base::paste0(
          tmpslct$document, " - ", tmpslct$title
        ))
      } else {
        codes <- ""
        base::names(codes) <- "select"
      }
      selection <- base::sort(base::unique(base::union(codes, documents)))
      shiny::selectInput(
        ns("slctdocfeed"), "Code or document:",
        choices = selection, selected = "", width = "100%"
      )
    })
    
    feedbacks_after_document_selection <- shiny::reactive({
      shiny::req(!base::is.null(input$slcttypequest))
      shiny::req(!base::is.null(input$slctdocfeed))
      if (input$slctdocfeed != ""){
        feedbacks_after_type_selection() |>
          dplyr::filter(
            document == input$slctdocfeed |
              code == input$slctdocfeed
          )
      } else feedbacks_after_type_selection()
    })
    
    output$edit_feedbacks <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(feedbacks_after_document_selection()))
      existing_names <- modrval$feedbacks |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      tmplanguage <- base::list.files("2_documents/main_language")[1]
      tmplanguage <- stringr::str_extract(tmplanguage, "...Rmd$") |>
        stringr::str_remove(".Rmd$")
      newitemid <- teachR::feedbacks_name_new_item(existing_names)
      doculevels <- c("", preselection()$code, preselection()$document) |>
        base::unique() |> base::sort()
      codelevels <- doculevels[stringr::str_detect(doculevels, "^Q")]
      tmprow <- tibble::tibble(
        item = newitemid,
        language = tmplanguage,
        code = base::factor("", levels = codelevels),
        type = input$slcttypequest,
        document = base::factor("", levels = doculevels),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor("logical", levels = c(
          "logical","qualitative","percentage"
        )),
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        success = base::as.numeric(NA),
        discrimination = base::as.numeric(NA)
      )
      if (base::nrow(feedbacks_after_document_selection()) > 0){
        itemsublist <- feedbacks_after_document_selection() |>
          dplyr::mutate(
            code = base::factor(
              code,
              levels = codelevels
            ),
            type = base::factor(type, levels = base::sort(base::unique(type))),
            document = base::factor(
              document,
              levels = doculevels
            ),
            scale = base::factor(scale, levels = c(
              "logical","qualitative","percentage"
            ))
          ) |>
          dplyr::arrange(code, document, dplyr::desc(value), proposition) |>
          dplyr::left_join(item_parameters, by = c("item","language")) |>
          dplyr::select(
            item, language, code, type, document, modifications, proposition,
            value, scale, explanation, keywords, success, discrimination
          ) |>
          dplyr::bind_rows(tmprow)
      } else {
        itemsublist <- tmprow
      }
      
      itemsublist |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,12,13), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "7%","2%","7%","7%","7%","3%","18%","3%",
            "5%","25%","10%","3%","3%"
          )
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$savefeedbacks, {
      shiny::req(!base::is.null(input$edit_feedbacks))
      modified <- rhandsontable::hot_to_r(input$edit_feedbacks) |>
        dplyr::mutate_if(base::is.factor, base::as.character)
      
      if (base::is.na(modified[base::nrow(modified), "proposition"])){
        modified <- modified[-base::nrow(modified),]
      }
      
      modified <- modified |>
        dplyr::select(base::names(modrval$feedbacks))
      
      not_modified <- modrval$feedbacks |>
        dplyr::anti_join(modified, by = c("item","language"))
      
      feedbacks <- not_modified |>
        dplyr::bind_rows(modified) |>
        dplyr::filter(!base::is.na(type), !base::is.na(value)) |>
        dplyr::arrange(item)
      
      modrval$feedbacks <- feedbacks
      base::save(feedbacks, file = "2_documents/feedbacks.RData")
    })
    
  })
}

