#' @name feedback_server
#' @title Edit feedbacks
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of individual feedback.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param test Reactive. Selected test.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @importFrom blastula creds_file
#' @importFrom blastula render_email
#' @importFrom blastula smtp_send
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom editR selection_server
#' @importFrom gmailr gm_auth_configure
#' @importFrom gmailr gm_from
#' @importFrom gmailr gm_html_body
#' @importFrom gmailr gm_mime
#' @importFrom gmailr gm_send_message
#' @importFrom gmailr gm_subject
#' @importFrom gmailr gm_to
#' @importFrom knitr knit2html
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny withMathJax
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_progress
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy update_modal_progress
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export


feedback_server <- function(id, test, tree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    attempt <- NULL
    filepath <- NULL
    flag <- NULL
    langiso <- NULL
    language <- NULL
    path <- NULL
    checked <- NULL
    document <- NULL
    earned <- NULL
    explanation <- NULL
    grade <- NULL
    interrogation <- NULL
    item <- NULL
    link <- NULL
    partial_credits <- NULL
    penalty <- NULL
    points <- NULL
    proposition <- NULL
    question <- NULL
    title <- NULL
    type <- NULL
    letter <- NULL
    value <- NULL
    weight <- NULL
    correct <- NULL
    page_link <- NULL
    question_type <- NULL
    question_points <- NULL
    question_grade <- NULL
    item_earned <- NULL
    page_title <- NULL
    team <- NULL
    
    ############################################################################
    # Load
    
    modrval <- shiny::reactiveValues()
    
    shiny::observe({
      
      shiny::req(!base::is.na(test()$test[1]))
      test_name <- test()$test[1]
      test_path <- course_paths()$subfolders$tests |>
        base::paste0("/", test_name)
      modrval$test_path <- test_path
      
      modrval$documents <- course_data()$documents
      
      modrval$textbook <- tree()$textbook
      
      test_parameters <- base::paste0(test_path, "/test_parameters.RData")
      shiny::req(base::file.exists(test_parameters))
      base::load(test_parameters)
      modrval$test_parameters <- test_parameters
      
      solutions <- base::paste0(test_path, "/4_solutions") |>
        base::list.files(full.names = TRUE)
      shiny::req(base::length(solutions) > 0)
      modrval$solutions <- tibble::tibble(path = solutions) |>
        dplyr::mutate(solutions = purrr::map(path, function(x){
          readr::read_csv(x, col_types = "ccncccccnccncccnn")
        })) |>
        tidyr::unnest(solutions)
      
      students <- base::paste0(test_path, "/6_students/student_list.csv")
      shiny::req(base::file.exists(students))
      students <- readr::read_csv(students, col_types = "ccccc")
      modrval$students <- students
      
      team_to_student <- students |>
        dplyr::select(team, student) |>
        base::unique()
      testunit <- test_parameters$test_unit[1]
      
      answers <- base::paste0(test_path, "/8_results/answers.csv")
      shiny::req(base::file.exists(answers))
      answers <- readr::read_csv(answers, col_types = "cncccn")
      if (testunit != "student"){
        answers <- answers |>
          dplyr::rename(team = student) |>
          dplyr::left_join(team_to_student, by = "team") |>
          dplyr::select(student, -team, dplyr::everything())
      }
      modrval$answers <- answers
      
      results <- base::paste0(test_path, "/8_results/results.csv")
      shiny::req(base::file.exists(results))
      results <- readr::read_csv(results, col_types = "cnccnccccnnnnnnn")
      if (testunit != "student"){
        results <- results |>
          dplyr::rename(team = student) |>
          dplyr::left_join(team_to_student, by = "team") |>
          dplyr::select(student, -team, dplyr::everything())
      }
      modrval$results <- results
      
      modrval$selection_basis <- modrval$results |>
        dplyr::select(student, attempt, language = version) |>
        dplyr::mutate(language = stringr::str_extract_all(language, "^..", simplify = TRUE)) |>
        base::unique()
      
      question_grades <- base::paste0(test_path, "/8_results/question_grades.csv")
      shiny::req(base::file.exists(question_grades))
      question_grades <- readr::read_csv(question_grades, col_types = "cncnn")
      if (testunit != "student"){
        question_grades <- question_grades |>
          dplyr::rename(team = student) |>
          dplyr::left_join(team_to_student, by = "team") |>
          dplyr::select(student, -team, dplyr::everything())
      }
      modrval$question_grades <- question_grades
      
      student_grades <- base::paste0(test_path, "/8_results/student_grades.csv")
      shiny::req(base::file.exists(student_grades))
      student_grades <- readr::read_csv(student_grades, col_types = "cnnn")
      if (testunit != "student"){
        student_grades <- student_grades |>
          dplyr::rename(team = student) |>
          dplyr::left_join(team_to_student, by = "team") |>
          dplyr::select(student, -team, dplyr::everything())
      }
      modrval$student_grades <- student_grades
      
      templates <- course_paths()$subfolders$templates_report |>
        base::list.files()
      templates <- templates[stringr::str_detect(templates, "^feedback_")]
      modrval$feedback_templates <- templates[stringr::str_detect(templates, "Rmd$")]
      
      modrval$feedback_files <- test_path |>
        base::paste0("/9_feedback") |>
        base::list.files()
      
      modrval$credentials <- base::list.files(course_paths()$subfolders$credentials)
       
    })
    
    
    
    ############################################################################
    # Selection
    
    output$slctlanguage <- shiny::renderUI({
      shiny::req(!base::is.null(modrval$test_parameters))
      shiny::req(base::nrow(modrval$test_parameters) > 1)
      exam_languages <- course_data()$languages |>
        dplyr::select(langiso, language, flag) |>
        dplyr::filter(langiso %in% modrval$test_parameters$test_languages[1])
      shinyWidgets::radioGroupButtons(
        inputId = ns("slctexamlang"), label = NULL, 
        choiceNames = base::lapply(
          base::seq_along(exam_languages$langiso), 
          function(i) shiny::tagList(
            shiny::tags$img(src = exam_languages$flag[i], width = 20, height = 15),
            exam_languages$language[i]
          )
        ),
        choiceValues = exam_languages$langiso,
        status = "primary", justified = FALSE, size = "sm",
        checkIcon = base::list(yes = shiny::icon("check"))
      )
    })
    
    students <- shiny::reactive({
      shiny::req(!base::is.null(input$slctexamlang))
      shiny::req(!base::is.null(modrval$selection_basis))
      shiny::req(base::nrow(modrval$selection_basis) > 1)
      students <- modrval$selection_basis
      students <- students[students$language == input$slctexamlang,]
      students <- students |>
        dplyr::select(student) |> base::unique() |>
        base::unlist() |> base::as.character()
      students
    })
    selected_student <- editR::selection_server("slctstudent", students)
    
    attempts <- shiny::reactive({
      shiny::req(!base::is.null(selected_student()))
      shiny::req(!base::is.null(modrval$selection_basis))
      shiny::req(base::nrow(modrval$selection_basis) > 1)
      attempts <- modrval$selection_basis |>
        dplyr::filter(student == selected_student()) |>
        dplyr::select(attempt) |> base::unique() |>
        base::unlist() |> base::as.character()
      attempts
    })
    selected_attempt <- editR::selection_server("slctattempt", attempts)
    
    
    
    ############################################################################
    # Creation
    
    shiny::observeEvent(input$newfeedback, {
      generic_template_names <- modrval$feedback_templates |>
        stringr::str_remove_all("^feedback_.._") |>
        stringr::str_remove_all(".Rmd$") |>
        base::unique()
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shinyWidgets::radioGroupButtons(
            inputId = ns("slctfeedbacktemplate"),
            label = "Based on the following template:",
            choices = generic_template_names,
            status = "info", justified = TRUE,
            size = "normal", direction = "vertical"
          ),
          footer = tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirmcreatefeedback"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$confirmcreatefeedback, {
      shiny::removeModal()
      shinyalert::shinyalert(
        "Are you sure?",
        "Creating a new feedback based on a template will overwrite any existing file.",
        inputId = "createfeedback",
        type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    shiny::observeEvent(input$createfeedback, {
      shiny::req(!base::is.null(modrval$test_parameters))
      shiny::req(base::nrow(modrval$test_parameters) > 1)
      shiny::req(!base::is.null(input$slctfeedbacktemplate))
      languages <- modrval$test_parameters$test_languages[1]
      languages_pattern <- base::paste(c(
        "(", base::paste(languages, collapse = "|"), ")"
        ), collapse = "")
      template_pattern <- base::paste0(
        "^feedback_",
        languages_pattern,
        "_",
        input$slctfeedbacktemplate,
        ".Rmd"
      )
      selection <- stringr::str_detect(
        modrval$feedback_templates,
        template_pattern
      )
      selected_templates <- modrval$feedback_templates[selection]
      nbrtemplates <- base::length(selected_templates)
      if (nbrtemplates > 0){
        fromfiles <- base::paste0(
          course_paths()$subfolders$templates_report,
          "/",
          selected_templates
        )
        tofiles <- base::paste0(
          modrval$test_path,
          "/9_feedback/",
          stringr::str_extract(selected_templates, "^feedback_.."),
          ".Rmd"
        )
        for (i in base::seq_len(nbrtemplates)){
          base::file.copy(
            from = fromfiles[i],
            to = tofiles[i]
          )
        }
        shinyalert::shinyalert(
          "Feedback templates imported!",
          "Templates of feedback have been added to your test.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE,
          inputId = "refreshfeedbackpath"
        )
      } else {
        shinyalert::shinyalert(
          "Feedback templates not imported!",
          "Sorry. No templated seemed to match exactly your selection.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    shiny::observeEvent(input$opentestfolder, {
      shiny::req(base::dir.exists(modrval$test_path))
      folder <- modrval$test_path
      if (base::dir.exists(folder)){
        if (.Platform['OS.type'] == "windows"){
          shell.exec(folder)
        } else {
          system2("open", folder)
        }
      } else {
        shinyalert::shinyalert(
          "Non-existing folder", "It seems that the folder you are trying to open does not exist. Did you already export files in it?",
          type = "error"
        )
      }
    })
    
    
    
    ############################################################################
    # Edition
    shiny::observeEvent(input$refreshfeedbackpath, {
      modrval$feedback_files <- modrval$test_path |>
        base::paste0("/9_feedback") |>
        base::list.files()
    })
    
    feedback_file_path <- shiny::reactive({
      shiny::req(base::length(modrval$feedback_files) > 0)
      shiny::req(!base::is.null(input$slctexamlang))
      shiny::req(base::any(stringr::str_detect(modrval$feedback_files, input$slctexamlang)))
      template_file <- modrval$feedback_files[stringr::str_detect(modrval$feedback_files, input$slctexamlang)]
      shiny::req(base::length(template_file) == 1)
      base::paste0(modrval$test_path, "/9_feedback/", template_file)
    })
    
    output$edit_feedback <- shiny::renderUI({
      shiny::req(!base::is.null(feedback_file_path()))
      shiny::req(base::file.exists(feedback_file_path()))
      lines <- readr::read_lines(feedback_file_path())
      input$refreshfeedback
      shinyAce::aceEditor(
        outputId = ns("editedfeedback"), value = lines,
        mode = "markdown", wordWrap = TRUE, debounce = 10,
        autoComplete = "live", height = "500"
      )
    })
    
    shiny::observeEvent(input$feedbackinrstudio, {
      shiny::req(!base::is.null(feedback_file_path()))
      shiny::req(base::file.exists(feedback_file_path()))
      rstudioapi::navigateToFile(feedback_file_path())
    })
    
    shiny::observeEvent(input$savefeedback, {
      shiny::req(!base::is.null(input$editedfeedback))
      base::writeLines(
        input$editedfeedback, feedback_file_path(), useBytes = TRUE
      )
      shinyalert::shinyalert(
        "Feedback saved!",
        "Refresh to preview with the changes you just made.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE,
        inputId = "refreshpreview"
      )
    })
    
    
    
    ############################################################################
    # Preview
    
    feedback_data <- shiny::reactive({
      shiny::req(!base::is.null(modrval$test_parameters))
      shiny::req(!base::is.null(modrval$documents))
      shiny::req(!base::is.null(modrval$textbook))
      shiny::req(!base::is.null(modrval$solutions))
      shiny::req(!base::is.null(modrval$students))
      shiny::req(!base::is.null(modrval$results))
      shiny::req(!base::is.null(modrval$question_grades))
      shiny::req(!base::is.null(modrval$student_grades))
      shiny::req(!base::is.null(feedback_file_path()))
      shiny::req(!base::is.null(selected_student()))
      shiny::req(!base::is.null(selected_attempt()))
      
      test <- modrval$student_grades |>
        dplyr::select(student, attempt, grade) |>
        base::unique() |>
        dplyr::mutate(points = modrval$test_parameters$test_points[1]) |>
        dplyr::select(student, attempt, points, grade)
      
      results <- modrval$results |>
        dplyr::select(
          student, attempt, question, version, item, letter,
          weight, checked, correct, item_earned = earned
        ) |>
        base::unique() |>
        dplyr::mutate(attempt = base::as.numeric(attempt))
      
      question_grades <- modrval$question_grades |>
        dplyr::select(
          student, attempt, question,
          question_type = type, question_points = points, question_grade = earned
        ) |>
        base::unique()
      
      questions_info <- modrval$documents |>
        dplyr::filter(type %in% c("Statements","Alternatives","Computation","Essay","Problem")) |>
        dplyr::select(
          question = file,
          question_type = type, dplyr::starts_with("tag_")
        ) |>
        base::unique()
      
      question_parameters <- modrval$test_parameters |>
        dplyr::select(
          question,
          penalty, partial_credits
        ) |>
        base::unique()
      
      solutions <- modrval$solutions |>
        dplyr::select(
          version, item, letter,
          document, language, interrogation, proposition, value, explanation
        ) |>
        base::unique()
      
      textbook <- modrval$textbook |>
        dplyr::mutate(language = base::toupper(language)) |>
        base::unique() |>
        dplyr::select(
          document, language,
          page_title = title, page_link = link
        )
      
      details <- results |>
        dplyr::left_join(question_grades, by = c("student","attempt","question")) |>
        dplyr::left_join(questions_info, by = "question") |>
        dplyr::left_join(question_parameters, by = "question") |>
        dplyr::filter(question_type %in% c("Essay","Problem") | checked == 1) |>
        dplyr::left_join(solutions, by = c("version","item","letter")) |>
        dplyr::left_join(textbook, by = c("document","language")) |>
        dplyr::select(
          student, attempt,
          question, question_type, interrogation, question_points, question_grade, penalty, partial_credits,
          item, proposition, value, explanation, checked, correct, weight, item_earned, page_title, page_link
        )
      
      base::list(
        test = test,
        details = details
      )
    })
    
    output$preview_feedback <- shiny::renderUI({
      shiny::req(!base::is.null(feedback_data()))
      shiny::req(!base::is.null(modrval$students))
      shiny::req(!base::is.null(selected_student()))
      shiny::req(!base::is.null(selected_attempt()))
      input$refreshfeedback
      input$refreshpreview
      input$refreshfeedbackpath #Still does not refresh everything when adding a feedback file.
      student <- modrval$students |>
        dplyr::filter(student == selected_student())
      studentid <- student$student[1]
      attemptid <- selected_attempt()
      teamid <- student$team[1]
      firstname <- student$firstname[1]
      lastname <- student$lastname[1]
      feedback_data <- feedback_data()
      lines <- readr::read_lines(feedback_file_path())
      base::suppressWarnings(
        shiny::withMathJax(shiny::HTML(knitr::knit2html(
          text = lines, quiet = TRUE, template = FALSE
        )))
      )
    })
    
    
    
    ############################################################################
    # Messaging
    
    output$slctcredentials <- shiny::renderUI({
      shiny::req(base::length(modrval$credentials) > 0)
      shinyWidgets::radioGroupButtons(
        inputId = ns("credentials"),
        label = "Select credentials to send e-mails:", 
        choices = modrval$credentials,
        status = "danger", justified = FALSE, size = "sm",
        checkIcon = base::list(yes = shiny::icon("check"))
      )
    })
    
    shiny::observeEvent(input$sendtestemail, {
      shiny::req(!base::is.null(feedback_file_path()))
      shiny::req(base::file.exists(feedback_file_path()))
      shiny::req(!base::is.null(selected_student()))
      shiny::req(!base::is.null(feedback_data()))
      shiny::req(base::length(feedback_data()) == 2)
      shiny::req(base::nrow(modrval$students) > 0)
      if (base::grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", input$emailtest)){
        feedback_data <- feedback_data()
        student <- modrval$students |>
          dplyr::filter(student == selected_student())
        studentid <- student$student[1]
        teamid <- student$team[1]
        firstname <- student$firstname[1]
        lastname <- student$lastname[1]
        emailaddress <- student$email[1]
        if (stringr::str_detect(input$credentials, ".creds$")){
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Please wait while the e-mail is sent..."
          )
          credentials <- base::paste0(
            course_paths()$subfolders$credentials, "/", input$credentials
          )
          sender <- base::readLines(credentials) |>
            stringr::str_extract(
              "([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9_-]+)"
            )
          blastula::render_email(feedback_file_path()) |>
            blastula::smtp_send(
              from = sender,
              to = input$emailtest,
              subject = input$mailsubject,
              credentials = blastula::creds_file(credentials)
            )
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Test e-mail sent!", "Check the test recipient's inbox.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        } else {
          credentials <- readr::read_csv(base::paste0(
            course_paths()$subfolders$credentials, "/", input$credentials
          ), col_types = "ccc")
          gmailr::gm_auth_configure(
            key = credentials$key,
            secret = credentials$secret
          )
          message <- knitr::knit2html(
            text = readr::read_lines(feedback_file_path()),
            quiet = TRUE, template = FALSE
          )
          email <- gmailr::gm_mime() |>
            gmailr::gm_to(input$emailtest) |>
            gmailr::gm_from(credentials$user) |>
            gmailr::gm_subject(input$mailsubject) |>
            gmailr::gm_html_body(message)
          gmailr::gm_send_message(email)
          shinyalert::shinyalert(
            "Test e-mail sent!", "Check the test recipient's inbox.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        }
      } else {
        shinyalert::shinyalert(
          "Check test e-mail address!", "Please enter a valid e-mail address for the test.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    shiny::observeEvent(input$sendfeedback, {
      shiny::req(!base::is.null(feedback_file_path()))
      shiny::req(base::file.exists(feedback_file_path()))
      shiny::req(!base::is.null(selected_student()))
      shiny::req(!base::is.null(feedback_data()))
      shiny::req(base::length(feedback_data()) == 3)
      shiny::req(!base::is.null(students()))
      shiny::req(base::nrow(modrval$students) > 0)
      
      feedback_data <- feedback_data()
      
      valid_recipients <- modrval$students |>
        stats::na.omit() |>
        dplyr::filter(student %in% students()$student)
      valid_recipients[base::grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", valid_recipients$email),]
      shiny::req(base::nrow(valid_recipients) > 0)
      
      if (stringr::str_detect(input$credentials, ".creds$")){
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while the e-mail is sent..."
        )
        credentials <- base::paste0(
          course_paths()$subfolders$credentials, "/", input$credentials
        )
        sender <- base::readLines(credentials) |>
          stringr::str_extract(
            "([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9_-]+)"
          )
        msgnbr <- base::nrow(valid_recipients)
        pgr <- 1/msgnbr
        shinybusy::show_modal_progress_line(
          value = pgr, text = "Reporting progress:"
        )
        for (i in base::seq_len(msgnbr)){
          pgr <- pgr + 1/msgnbr
          studentid <- valid_recipients$student[i]
          teamid <- valid_recipients$team[i]
          firstname <- valid_recipients$firstname[i]
          lastname <- valid_recipients$lastname[i]
          emailaddress <- valid_recipients$email[i]
          blastula::render_email(feedback_file_path()) |>
            blastula::smtp_send(
              from = sender,
              to = input$emailtest,
              subject = input$mailsubject,
              credentials = blastula::creds_file(credentials)
            )
          shinybusy::update_modal_progress(pgr)
        }
        shinybusy::remove_modal_progress()
        shinyalert::shinyalert(
          "Task complete!", "A report has been sent to each recipient individually.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        credentials <- readr::read_csv(base::paste0(
          course_paths()$subfolders$credentials, "/", input$credentials
        ), col_types = "ccc")
        gmailr::gm_auth_configure(
          key = credentials$key,
          secret = credentials$secret
        )
        msgnbr <- base::nrow(valid_recipients)
        pgr <- 1/msgnbr
        shinybusy::show_modal_progress_line(
          value = pgr, text = "Reporting progress:"
        )
        for (i in base::seq_len(msgnbr)){
          studentid <- valid_recipients$student[i]
          teamid <- valid_recipients$team[i]
          firstname <- valid_recipients$firstname[i]
          lastname <- valid_recipients$lastname[i]
          emailaddress <- valid_recipients$email[i]
          message <- knitr::knit2html(
            text = readr::read_lines(feedback_file_path()),
            quiet = TRUE, template = FALSE
          )
          email <- gmailr::gm_mime() |>
            gmailr::gm_to(input$emailtest) |>
            gmailr::gm_from(credentials$user) |>
            gmailr::gm_subject(input$mailsubject) |>
            gmailr::gm_html_body(message)
          gmailr::gm_send_message(email)
          shinybusy::update_modal_progress(pgr)
        }
        shinybusy::remove_modal_progress()
        shinyalert::shinyalert(
          "Task complete!", "A report has been sent to each recipient individually.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    
  })
}

