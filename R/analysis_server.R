#' @name analysis_server
#' @title Analyse data
#' @author Nicolas Mangin
#' @description Module allowing the user to analyse data about teaching, learning, and testing materials.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param references Reactive. Function containing a list of references.
#' @importFrom bibliogR make_bib_file
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom quarto quarto_render
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny addResourcePath
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shiny updateSelectInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom tibble tribble
#' @export


analysis_server <- function(id, course_paths, references){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    language <- NULL
    question <- NULL
    results <- NULL
    student <- NULL
    test <- NULL
    tests <- NULL
    tree <- NULL
    lastname <- NULL
    firstname <- NULL
    students <- NULL
    
    
    
    # Create analysis ##########################################################
    
    shiny::observeEvent(input$newanalysis, {
      analysis_templates <- course_paths()$subfolders$templates_report |>
        base::list.files()
      analysis_templates <- analysis_templates[stringr::str_detect(analysis_templates, "^analysis_")]
      analysis_templates <- analysis_templates[stringr::str_detect(analysis_templates, "qmd$")]
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::textInput(
            ns("defnewtitle"), "Title of the analysis:", width = "100%"
          ),
          shiny::selectInput(
            ns("slcttemplatebasis"), "Based on the following template:",
            choices = analysis_templates, selected = "", width = "100%"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("checkanalysis"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$checkanalysis, {
      title <- input$defnewtitle
      folder_name <- title |>
        base::tolower() |>
        stringr::str_replace_all(" ", "_")
      existing_analyses <- base::list.files(
        course_paths()$subfolders$analyses,
        full.names = FALSE, recursive = FALSE
      )
      if (base::nchar(title) < 5){
        #Check that not empty
        shinyalert::shinyalert(
          title = "Insufficient title!",
          text = "The lenght of the title must be greater than 5 characters.",
          type = "error",
          closeOnEsc = FALSE
        )
      } else if (title %in% existing_analyses){
        #Check that does not already exist
        shinyalert::shinyalert(
          title = "Existing analysis!",
          text = "An analysis with the same title already exists. Please change the title.",
          type = "error",
          closeOnEsc = FALSE
        )
      } else {
        shinyalert::shinyalert(
          title = "Create analysis?",
          text = "This will create a new analysis. Do you wish to proceed?",
          type = "warning",
          inputId = "createanalysis",
          showCancelButton = TRUE,
          closeOnEsc = FALSE
        )
      }
    })
    
    shiny::observeEvent(input$createanalysis, {
      shiny::removeModal()
      
      title <- input$defnewtitle
      folder_name <- title |>
        base::tolower() |>
        stringr::str_replace_all(" ", "_")
      
      folders_from <- base::paste0(
        course_paths()$subfolders$templates_report,
        "/analysis_subfolders"
      )
      folders_to <- base::paste0(
        course_paths()$subfolders$analyses,
        "/analysis_subfolders"
      )
      folder_path <- base::paste0(
        course_paths()$subfolders$analyses,
        "/", folder_name
      )
      base::file.copy(folders_from, course_paths()$subfolders$analyses, recursive = TRUE)
      base::file.rename(folders_to, folder_path)
      
      qmd_template <- base::paste0(
        course_paths()$subfolders$templates_report,
        "/", input$slcttemplatebasis
      )
      qmd_analysis <- base::paste0(folder_path, "/index.qmd")
      base::file.copy(qmd_template, qmd_analysis)
      
      shinyalert::shinyalert(
        title = "Analysis created",
        text = "You can now refresh the list of analyses to select it for display or edition.",
        type = "success"
      )
      
    })
    
    
    # Select analysis parameters ###############################################
    
    selection_base <- shiny::reactive({
      shiny::req(!base::is.null(course_paths()$databases))
      shiny::req(base::file.exists(course_paths()$databases$results))
      shiny::req(base::file.exists(course_paths()$databases$tests))
      base::load(course_paths()$databases$results)
      base::load(course_paths()$databases$tests)
      base::load(course_paths()$databases$students)
      shiny::req(base::nrow(results) > 1)
      results |>
        dplyr::select(test, student, question, language) |>
        dplyr::left_join(base::unique(dplyr::select(tests, test, tree)), by = "test") |>
        dplyr::left_join(base::unique(dplyr::select(students, student, lastname, firstname)), by = "student") |>
        dplyr::select(tree, language, lastname, firstname, student, test, question) |>
        dplyr::mutate(
          lastname = stringr::str_replace_all(lastname, "[^[:alnum:]]", "-"),
          firstname = stringr::str_replace_all(firstname, "[^[:alnum:]]", "-")
        ) |>
        base::unique() |>
        stats::na.omit()
    })
    
    shiny::observe({
      shiny::req(!base::is.null(selection_base()))
      
      input$refreshlist
      
      analyses <- base::list.dirs(
        course_paths()$subfolders$analyses,
        full.names = FALSE, recursive = FALSE
      )
      shiny::updateSelectInput(
        session,
        "slctanalysis",
        choices = analyses,
        selected = input$slctanalysis
      )
      
      selected <- selection_base()
      
      tmpchoices <- base::unique(selected$tree)
      if (input$slcttree %in% tmpchoices) tmpselected <- input$slcttree else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slcttree",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slcttree %in% selected$tree){
        selected <- dplyr::filter(selected, tree == input$slcttree)
      }
      
      tmpchoices <- base::unique(selected$language)
      if (input$slctlanguage %in% tmpchoices) tmpselected <- input$slctlanguage else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slctlanguage",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slctlanguage %in% selected$language){
        selected <- dplyr::filter(selected, language == input$slctlanguage)
      }
      
      tmpchoices <- c("", base::unique(selected$lastname))
      if (input$slctlastname %in% tmpchoices) tmpselected <- input$slctlastname else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slctlastname",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slctlastname %in% selected$lastname){
        selected <- dplyr::filter(selected, lastname == input$slctlastname)
      }
      
      tmpchoices <- c("", base::unique(selected$firstname))
      if (input$slctfirstname %in% tmpchoices) tmpselected <- input$slctfirstname else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slctfirstname",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slctfirstname %in% selected$firstname){
        selected <- dplyr::filter(selected, firstname == input$slctfirstname)
      }
      
      tmpchoices <- base::unique(selected$student)
      if (input$slctstudent %in% tmpchoices) tmpselected <- input$slctstudent else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slctstudent",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slctstudent %in% selected$student){
        selected <- dplyr::filter(selected, student == input$slctstudent)
      }
      
      tmpchoices <- base::unique(selected$test)
      if (input$slcttest %in% tmpchoices) tmpselected <- input$slcttest else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slcttest",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slcttest %in% selected$test){
        selected <- dplyr::filter(selected, test == input$slcttest)
      }
      
      tmpchoices <- base::unique(selected$question)
      if (input$slctquestion %in% tmpchoices) tmpselected <- input$slctquestion else tmpselected <- tmpchoices[1]
      shiny::updateSelectInput(
        session, "slctquestion",
        choices = tmpchoices, selected = tmpselected
      )
      if (input$slctquestion %in% selected$question){
        selected <- dplyr::filter(selected, question == input$slctquestion)
      }
      
    })
    
    selections <- shiny::reactive({
      input$applyselection
      analysis_to_edit <- shiny::isolate({ analysis_to_edit() })
      selections <- shiny::isolate({
        tibble::tribble(
          ~"variable", ~"value",
          "treeid", input$slcttree,
          "languageid", input$slctlanguage,
          "studentid", input$slctstudent,
          "testid", input$slcttest,
          "questionid", input$slctquestion
        )
      })
      reportR::apply_selections_to_analysis(selections, analysis_to_edit)
      selections
    })
    
    
    
    # Display or edit analysis #################################################
    
    analysis_to_edit <- shiny::reactive({
      folderpath <- base::paste0(
        course_paths()$subfolders$analyses,
        "/", input$slctanalysis
      )
      qmdpath <- base::paste0(folderpath, "/index.qmd")
      shiny::req(base::file.exists(qmdpath))
      reportR::retrieve_data_for_analysis(folderpath, course_paths())
      qmdpath
    })
    
    edited_lines <- shiny::reactive({
      shiny::req(!base::is.null(analysis_to_edit()))
      if (!base::is.null(input$anarefresh)) input$anarefresh
      base::readLines(analysis_to_edit())
    })
    
    output$analysis <- shiny::renderUI({
      input$anarefresh
      selections()
      shiny::req(!base::is.null(edited_lines()))
      if (input$editionswitch){
        shinydashboardPlus::box(
          width = 12, title = "Edition", solidHeader = TRUE,
          status = "navy", collapsible = FALSE, collapsed = FALSE,
          height = "750px",
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                ns("anainrstudio"), "RStudio",
                icon = shiny::icon("r-project"),
                style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("anarefresh"), "Refresh",
                icon = shiny::icon("rotate"),
                style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("saveana"), "Save",
                icon = shiny::icon("floppy-disk"),
                style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12, shinyAce::aceEditor(
              outputId = ns("editeddoc"), value = edited_lines(),
              mode = "markdown", wordWrap = TRUE, debounce = 10,
              autoComplete = "live", height = "800"
            ))
          )
        )
      } else {
        if (base::file.exists(analysis_to_edit())){
          shinybusy::show_modal_spinner(spin = "orbit", text = "Generating the report...")
          
          analysis_folder <- base::paste0(
            course_paths()$subfolders$analyses,
            "/", input$slctanalysis
          )
          bibliogR::make_bib_file(
            source_folder = analysis_folder,
            references = references(),
            destination_folder = base::paste0(analysis_folder, "/data"),
            file_name = "references.bib"
          )
          quarto::quarto_render(input = analysis_to_edit(), output_format = "html")
          shiny::addResourcePath("analysis", analysis_folder)
          shinybusy::remove_modal_spinner()
          shiny::tags$iframe(src="analysis/index.html", height = 900, width = "100%")
        }
      }
    })
    
    shiny::observeEvent(input$anainrstudio, {
      analysis_to_edit <- shiny::isolate({ analysis_to_edit() })
      shiny::req(!base::is.null(analysis_to_edit))
      rstudioapi::navigateToFile(analysis_to_edit)
    })
    
    shiny::observeEvent(input$saveana, {
      analysis_to_edit <- shiny::isolate({ analysis_to_edit() })
      editeddoc <- shiny::isolate({ input$editeddoc })
      shiny::req(!base::is.null(editeddoc))
      base::writeLines(editeddoc, analysis_to_edit, useBytes = TRUE)
      shinyalert::shinyalert(
        "Analysis saved", "Switch out of the edition mode to see the result.",
        type = "success"
      )
    })
    
    
    
    
    shiny::observeEvent(input$updateanalytics, {
      
      
      shinybusy::show_modal_progress_line(value = 0/2, text = "Updating analytics")
      
      shinybusy::update_modal_progress(value = 1/2, text = "Updating tests")
      
      
      shinybusy::update_modal_progress(value = 2/2, text = "Analytics updated")
      shinybusy::remove_modal_spinner()
      
      shinyalert::shinyalert(
        title = "Analytics updated!",
        text = "Reload the cours to see the results.",
        type = "success"
      )
      
    })
    
  })
}

