#' @name test_reporting_server
#' @title Edit reports
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of individual feedback or course analyses.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the report in "6_reports" and allows individualizing and sending it to a list of recipients.
#' @import shiny
#' @import gmailr
#' @import blastula
#' @importFrom dplyr filter
#' @importFrom rstudioapi navigateToFile
#' @importFrom shinyalert shinyalert
#' @importFrom shinyAce aceEditor
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @export


test_reporting_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    student <- NULL
    team <- NULL
    recipient <- NULL
    
    modrval <- shiny::reactiveValues()
    credentials <- base::list.files("6_reports/credentials")
    
    shiny::observe({
      if ("gmailr_profile.RData" %in% credentials){
        base::load("6_reports/credentials/gmailr_profile.RData")
        credentials <- base::setdiff(credentials, "gmailr_profile.RData")
      } else {
        gmailr_profile <- base::list(
          from = base::character(0),
          gmail_key = character(0), 
          gmail_secret = character(0)
        )
        base::save(gmailr_profile, "6_reports/credentials/gmailr_profile.RData")
      }
      modrval$gmailr_profile <- gmailr_profile
      modrval$other_profiles <- credentials
      
      feedback_templates <- base::list.files("1_preparation/templates/feedback")
      analysis_templates <- base::list.files("1_preparation/templates/analysis")
      
      modrval$feedback_templates <- feedback_templates
      modrval$analysis_templates <- analysis_templates
    })
    
    
    
    
    
    # Content definition #######################################################
    
    
    output$select_tests <- shiny::renderUI({
      shiny::req(!base::is.null(input$reporttype))
      if (input$reporttype == "feedback"){
        tests <- "6_reports" |>
          base::list.dirs(recursive = FALSE, full.names = FALSE) |>
          base::setdiff(c("additional_data","analyses","credentials"))
        shiny::req(base::length(tests) > 0)
        shiny::selectInput(
          ns("slcttest"), "Tests:", choices = c("", tests),
          selected = "", width = "100%"
        )
      }
    })
    
    # Initialize list of reports and list of recipients
    shiny::observe({
      shiny::req(!base::is.null(input$reporttype))
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        grades_file <- base::paste0("6_reports/", input$slcttest, "/grades.csv")
        if (base::file.exists(grades_file)){
          modrval$grades <- utils::read.csv(grades_file)
        } else {
          modrval$grades <- tibble::tibble(
            student = base::character(0),
            points = base::numeric(0),
            grade = base::numeric(0)
          )
        }
        feedback_file <- base::paste0(
          "6_reports/", input$slcttest, "/feedback.csv"
        )
        if (base::file.exists(feedback_file)){
          modrval$feedback <- utils::read.csv(feedback_file)
        } else {
          modrval$feedback <- tibble::tibble(
            test0 = base::character(0),
            question = base::character(0),
            version = base::character(0),
            tree = base::character(0),
            test_points = base::numeric(0),
            points = base::numeric(0),
            document = base::character(0),
            language = base::character(0),
            number = base::numeric(0),
            letter = base::character(0),
            item = base::character(0),
            type = base::character(0),
            interrogation = base::character(0),
            proposition = base::character(0),
            explanation = base::character(0),
            student = base::character(0),
            scale = base::character(0),
            checked = base::numeric(0),
            weight = base::numeric(0),
            earned = base::numeric(0),
            title = base::character(0),
            website = base::character(0)
          )
        }
        students_file <- base::paste0(
          "6_reports/", input$slcttest, "/students.csv"
        )
        if (base::file.exists(students_file)){
          modrval$students <- utils::read.csv(students_file) |>
            dplyr::filter(
              student %in% modrval$grades$student |
                team %in% modrval$grades$student
            )
        } else {
          modrval$students <- tibble::tibble(
            student = base::character(0),
            team = base::character(0),
            firstname = base::character(0),
            lastname = base::character(0),
            email = base::character(0)
          )
        }
        modrval$recipients <- tibble::tibble(
          recipient = base::character(0),
          firstname = base::character(0),
          lastname = base::character(0),
          email = base::character(0)
        )
      } else {
        modrval$grades <- tibble::tibble(
          student = base::character(0),
          points = base::numeric(0),
          grade = base::numeric(0)
        )
        modrval$feedback <- tibble::tibble(
          test0 = base::character(0),
          question = base::character(0),
          version = base::character(0),
          tree = base::character(0),
          test_points = base::numeric(0),
          points = base::numeric(0),
          document = base::character(0),
          language = base::character(0),
          number = base::numeric(0),
          letter = base::character(0),
          item = base::character(0),
          type = base::character(0),
          interrogation = base::character(0),
          proposition = base::character(0),
          explanation = base::character(0),
          student = base::character(0),
          scale = base::character(0),
          checked = base::numeric(0),
          weight = base::numeric(0),
          earned = base::numeric(0),
          title = base::character(0),
          website = base::character(0)
        )
        modrval$students <- tibble::tibble(
          student = base::character(0),
          team = base::character(0),
          firstname = base::character(0),
          lastname = base::character(0),
          email = base::character(0)
        )
      }
      modrval$selectable_students <- modrval$students$student
      modrval$selected_student_rank <- 1
    })
    
    output$select_report <- shiny::renderUI({
      shiny::req(!base::is.null(input$reporttype))
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        reports <- base::paste0("6_reports/", input$slcttest) |>
          base::list.files(recursive = FALSE, full.names = FALSE)
        reports <- reports[stringr::str_detect(reports, ".Rmd$")]
      } else {
        reports <- base::paste0("6_reports/analyses") |>
          base::list.files(recursive = FALSE, full.names = FALSE)
        reports <- reports[stringr::str_detect(reports, ".Rmd$")]
      }
      shiny::selectInput(
        ns("slctreport"), "Report:", choices = c("", reports),
        selected = "", width = "100%"
      )
    })
    
    shiny::observeEvent(input$new_report, {
      shiny::req(!base::is.null(input$reporttype))
      if (input$reporttype == "feedback"){
        templates <- modrval$feedback_templates
      } else {
        templates <- modrval$analysis_templates
      }
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::textInput(
            ns("namenewreport"), "File name:", value = "new_report", width = "100%"
          ),
          shiny::selectInput(
            ns("slcttemplatebasis"), "Based on the following template:",
            choices = c("", templates),
            selected = "", width = "100%"
          ),
          footer = tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("createreport"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$createreport, {
      shiny::removeModal()
      shiny::req(!base::is.null(input$reporttype))
      shiny::req(!base::is.null(input$namenewreport))
      shiny::req(!base::is.null(input$slcttemplatebasis))
      templatepath <- base::paste0(
        "1_preparation/templates/", input$reporttype,
        "/",input$slcttemplatebasis
      )
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        reportpath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$namenewreport, ".Rmd"
        )
      } else {
        reportpath <- base::paste0(
          "6_reports/analyses/", input$namenewreport, ".Rmd"
        )
      }
      if (base::file.exists(templatepath)){
        base::file.copy(
          from = templatepath, to = reportpath
        )
      } else {
        lines <- c('---', 'output: html_document', '---', '', '')
        base::writeLines(lines, reportpath, useBytes = TRUE)
      }
      shinyalert::shinyalert(
        "Report created!", "Toggle back and forth on report type to see it.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
    
    
    
    
    
    
    # Student-attempt selection ##############################################
    
    
    
    
    
    
    
    output$select_recipient <- shiny::renderUI({
      shiny::req(!base::is.null(input$reporttype))
      shiny::req(!base::is.null(input$sendingmethod))
      shiny::req(input$reporttype == "feedback")
      shiny::req(input$sendingmethod == "individual")
      shiny::req(base::length(modrval$selectable_students) > 0)
      students <- modrval$selectable_students
      rank <- modrval$selected_student_rank
      shiny::selectInput(
        ns("slctstudent"), "Select a student to display",
        choices = students, selected = students[rank], width = "100%"
      )
    })
    
    shiny::observeEvent(input$slctstudent, {
      new_student_rank <- base::match(
        input$slctstudent, modrval$selectable_students
      )
      modrval$selected_student_rank <- new_student_rank
    })
    
    output$student_rank_selection <- shiny::renderUI({
      shiny::req(!base::is.null(modrval$selectable_students))
      shiny::req(!base::is.null(modrval$selected_student_rank))
      shiny::req(modrval$selected_student_rank <= base::length(modrval$selectable_students))
      shiny::sliderInput(
        ns("slctstudentrank"),
        "Students:",
        value = modrval$selected_student_rank,
        min = 1, max = base::length(modrval$selectable_students),
        step = 1, width = "100%"
      )
    })
    shiny::observeEvent(input$previousstudent, {
      modrval$selected_student_rank <- base::max(c(1,modrval$selected_student_rank-1))
    })
    shiny::observeEvent(input$slctstudentrank, {
      modrval$selected_student_rank <- input$slctstudentrank
    })
    shiny::observeEvent(input$nextstudent, {
      modrval$selected_student_rank <- base::min(
        modrval$selected_student_rank+1, base::length(modrval$selectable_students)
      )
    })
    
    output$gradingprogress <- shiny::renderUI({
      shiny::req(base::length(modrval$selected_student_rank) > 0)
      shinyWidgets::progressBar(
        id = "progressbar", value = modrval$selected_student_rank,
        total = base::length(modrval$selectable_students), status = "info"
      )
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    shiny::observeEvent(input$sendtestemail, {
      shiny::req(!base::is.null(input$emailtest))
      shiny::req(!base::is.null(input$sendingpkg))
      if (input$sendingpkg == "gmailr"){
        shiny::req(
          !base::is.null(input$deffrom) & !base::is.na(input$deffrom)
        )
      } else {
        shiny::req(
          !base::is.null(input$slctcreds) & !base::is.na(input$slctcreds)
        )
      }
      shiny::req(!base::is.null(input$mailsubject))
      grades <- modrval$grades
      feedback <- modrval$feedback
      students <- modrval$students
      recipients <- modrval$recipients
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slctstudent))
        shiny::req(input$slctstudent != "")
        student <- students |>
          dplyr::filter(
            student == input$slctstudent | team == input$slctstudent
          )
        studentid <- student$student[1]
        teamid <- student$team[1]
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
      }
      if (input$emailtest == ""){
        shinyalert::shinyalert(
          "Missing e-mail address!", "Please indicate the address to which the test should be sent.",
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        if (input$sendingpkg == "gmailr"){
          gmailr::gm_auth_configure(
            key = input$defkey,
            secret = input$defsecret
          )
          message <- knitr::knit2html(
            text = base::readLines(filepath),
            fragment.only = TRUE, quiet = TRUE
          )
          email <- gmailr::gm_mime() |>
            gmailr::gm_to(input$emailtest) |>
            gmailr::gm_from(input$deffrom) |>
            gmailr::gm_subject(input$mailsubject) |>
            gmailr::gm_html_body(message)
          gmailr::gm_send_message(email)
          shinyalert::shinyalert(
            "Test e-mail sent!", "Check the test recipient's inbox.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        } else {
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Please wait while the e-mail is sent..."
          )
          credentials <- base::paste0("6_reports/credentials/", input$slctcreds)
          sender <- base::readLines(credentials) |>
            stringr::str_extract(
              "([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9_-]+)"
            )
          blastula::render_email(filepath) |>
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
        }
        
      }
    })
    
    
    
    
    
    
    
    
    
    
    
    
    # Report view and edition ##################################################
    
    output$viewreport <- shiny::renderUI({
      shiny::req(input$slctreport)
      input$reportrefresh
      grades <- modrval$grades
      feedback <- modrval$feedback
      students <- modrval$students
      recipients <- modrval$recipients
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
        if (input$sendingmethod == "individual"){
          shiny::req(!base::is.null(input$slctstudent))
          shiny::req(input$slctstudent != "")
          student <- students |>
            dplyr::filter(
              student == input$slctstudent | team == input$slctstudent
            )
          studentid <- student$student[1]
          teamid <- student$team[1]
        }
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
      }
      input$savereport
      if (base::file.exists(filepath)){
        shinydashboardPlus::box(
          width = 12, title = "View", solidHeader = TRUE,
          status = "teal", collapsible = FALSE, collapsed = FALSE,
          shiny::fluidRow(
            shiny::column(
              1
              
            ),
            shiny::column(
              10
              
            ),
            shiny::column(
              1
              
            )
          ),
          base::suppressWarnings(
            shiny::withMathJax(shiny::HTML(knitr::knit2html(
              text = base::readLines(filepath),
              fragment.only = TRUE, quiet = TRUE
            )))
          )
        )
      } else NULL
    })
    
    output$editreport <- shiny::renderUI({
      shiny::req(input$slctreport)
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
      }
      if (base::file.exists(filepath)){
        lines <- base::readLines(filepath)
        shinydashboardPlus::box(
          width = 12, title = "Edition", solidHeader = TRUE,
          status = "info", collapsible = FALSE, collapsed = FALSE,
          height = "750px",
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                ns("savereport"), "Save", icon = shiny::icon("save"),
                style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("reportinrstudio"), "RStudio",
                icon = shiny::icon("r-project"),
                style = "background-color:#222222;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                ns("reportrefresh"), "Refresh",
                icon = shiny::icon("sync"),
                style = "background-color:#003399;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12, shinyAce::aceEditor(
              outputId = ns("editedreport"), value = lines, mode = "markdown",
              wordWrap = TRUE, debounce = 10, autoComplete = "live",
              height = "750px"
            ))
          )
        )
      } else NULL
    })
    
    shiny::observeEvent(input$savereport, {
      shiny::req(input$slctreport != "")
      shiny::req(!base::is.null(input$editedreport))
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
      }
      base::writeLines(input$editedreport, filepath, useBytes = TRUE)
    })
    
    shiny::observeEvent(input$reportinrstudio, {
      shiny::req(input$slctreport != "")
      if (input$reporttype == "feedback"){
        shiny::req(!base::is.null(input$slcttest))
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
      }
      rstudioapi::navigateToFile(filepath)
    })
    
    
    
    
    
    # Mailing definition #######################################################
    
    output$defcredentials <- shiny::renderUI({
      shiny::req(!base::is.null(input$sendingpkg))
      
      if (input$sendingpkg == "gmailr"){
        base::list(
          shiny::column(
            3,
            shiny::textInput(
              ns("deffrom"), "From:",
              value = modrval$gmailr_profile$from,
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::passwordInput(
              ns("defkey"), "Key:",
              value = modrval$gmailr_profile$gmail_key,
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::passwordInput(
              ns("defsecret"), "Secret:",
              value = modrval$gmailr_profile$gmail_secret,
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("save_gmailr_profile"), "Save",
              icon = shiny::icon("upload"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-top:25px;"
            )
          )
        )
      } else {
        creds <- c("", modrval$other_profiles)
        shiny::selectInput(
          ns("slctcreds"), "Select server:",
          choices = creds, selected = creds[1]
        )
      }
    })
    
    shiny::observeEvent(input$save_gmailr_profile,  {
      gmailr_profile <- base::list(
        from = input$deffrom,
        gmail_key = input$defkey, 
        gmail_secret = input$defsecret
      )
      base::save(
        gmailr_profile,
        file = "6_reports/credentials/gmailr_profile.RData"
      )
      modrval$gmailr_profile <- gmailr_profile
      shinyalert::shinyalert(
        "Saved", "You gmailr profile has been updated.", type = "success"
      )
    })
    
    
    
    shiny::observeEvent(input$importrecipientlist, {
      shiny::req(!base::is.null(input$reporttype))
      shiny::req(!base::is.null(input$recipientlisttoimport))
      file <- input$recipientlisttoimport
      filepath <- file$datapath
      if (input$reporttype == "feedback"){
        modrval$students <- utils::read.csv(filepath)
        shiny::req(!base::is.null(input$slcttest))
        utils::write.csv(
          modrval$students,
          base::paste0("6_reports/", input$slcttest, "/students.csv"),
          row.names = FALSE
        )
        shinyalert::shinyalert(
          "Imported!", "Your student list has been updated.", type = "success"
        )
      } else {
        modrval$recipients <- utils::read.csv(filepath)
        shinyalert::shinyalert(
          "Imported!", "Your recipient list has been updated.", type = "success"
        )
      }
    })
    
    
    
    output$recipientnbr <- shiny::renderText({
      shiny::req(!base::is.null(modrval$students))
      shiny::req(!base::is.null(modrval$recipients))
      nbr <- base::nrow(modrval$students) + base::nrow(modrval$recipients)
      base::paste0(nbr, " persons are listed as recipients.")
    })
    
    
    
    shiny::observeEvent(input$sendtoall, {
      shiny::req(!base::is.null(input$sendingpkg))
      shiny::req(!base::is.null(input$mailsubject))
      shiny::req(!base::is.null(input$sendingmethod))
      shiny::req(!base::is.null(input$sendbyteam))
      grades <- modrval$grades
      feedback <- modrval$feedback
      students <- modrval$students
      recipients <- modrval$recipients
      if (input$reporttype == "feedback"){
        filepath <- base::paste0(
          "6_reports/", input$slcttest, "/", input$slctreport
        )
        sendinglist <- students
      } else {
        filepath <- base::paste0(
          "6_reports/analyses/", input$slctreport
        )
        sendinglist <- recipients |>
          dplyr::rename(student = recipient)
      }
      if (input$sendbyteam){
        sendinglist <- sendinglist |>
          dplyr::select(team, email) |>
          dplyr::group_by(team) |>
          dplyr::summarise(email=base::paste0(email, collapse = ",")) |>
          dplyr::mutate(student = NA)
      } else {
        sendinglist <- sendinglist
      }
      if (input$sendingpkg == "gmailr"){
        gmailr::gm_auth_configure(
          key = input$defkey,
          secret = input$defsecret
        )
        if (input$sendingmethod == "individual"){
          msgnbr <- base::nrow(sendinglist)
          pgr <- 1/msgnbr
          shinybusy::show_modal_progress_circle(
            value = pgr, text = "Reporting progress:"
          )
          for (i in base::seq_len(msgnbr)){
            pgr <- pgr + 1/msgnbr
            address <- sendinglist$email[i]
            studentid <- sendinglist$student[i]
            teamid <- sendinglist$team[i]
            message <- knitr::knit2html(
              text = base::readLines(filepath),
              fragment.only = TRUE, quiet = TRUE
            )
            email <- gmailr::gm_mime() |>
              gmailr::gm_to(address) |>
              gmailr::gm_from(input$deffrom) |>
              gmailr::gm_subject(input$mailsubject) |>
              gmailr::gm_html_body(message)
            gmailr::gm_send_message(email)
            base::Sys.sleep(2)
            base::print(base::paste0("Report sent to ", address))
            shinybusy::update_modal_progress(pgr)
          }
          shinybusy::remove_modal_progress()
          shinyalert::shinyalert(
            "Task complete!", "A report has been sent to each recipient individually.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        } else {
          message <- knitr::knit2html(
            text = base::readLines(filepath),
            fragment.only = TRUE, quiet = TRUE
          )
          address <- base::unique(base::paste(
            sendinglist$email, collapse = ","
          ))
          email <- gmailr::gm_mime() |>
            gmailr::gm_to(address) |>
            gmailr::gm_from(input$deffrom) |>
            gmailr::gm_subject(input$mailsubject) |>
            gmailr::gm_html_body(message)
          gmailr::gm_send_message(email)
          shinyalert::shinyalert(
            "Task complete!", "The report has been send to all recipents.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        }
      } else {
        if (input$slctcreds == ""){
          shinyalert::shinyalert(
            "Missing credentials!", "Please select credentials in the top-left box.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        } else {
          credentials <- base::paste0("6_reports/credentials/", input$slctcreds)
          sender <- base::readLines(credentials) |>
            stringr::str_extract(
              "([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9_-]+)"
            )
          if (input$sendingmethod == "individual"){
            msgnbr <- base::nrow(sendinglist)
            pgr <- 0
            shinybusy::show_modal_progress_circle(
              value = pgr, text = "Reporting progress:"
            )
            for (i in base::seq_len(msgnbr)){
              pgr <- pgr + 1/msgnbr
              address <- stringr::str_split(
                sendinglist$email[i], pattern = ",", simplify = TRUE
              )
              studentid <- sendinglist$student[i]
              teamid <- sendinglist$team[i]
              blastula::render_email(filepath) |>
                blastula::smtp_send(
                  from = sender,
                  to = address,
                  subject = input$mailsubject,
                  credentials = blastula::creds_file(credentials)
                )
              base::Sys.sleep(2)
              base::print(base::paste0("Report sent to ", address))
              shinybusy::update_modal_progress(pgr)
            }
            shinybusy::remove_modal_progress()
            shinyalert::shinyalert(
              "Task complete!", "A report has been sent to each recipient individually.",
              type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
            )
          } else {
            shinybusy::show_modal_spinner(
              spin = "orbit",
              text = "Please wait while the e-mail is sent..."
            )
            address <- base::unique(stringr::str_split(
              sendinglist$email, sep = ",", simplify = TRUE
            ))
            blastula::render_email(filepath) |>
              blastula::smtp_send(
                from = sender,
                to = base::unlist(address),
                subject = input$mailsubject,
                credentials = blastula::creds_file(credentials)
              )
            shinybusy::remove_modal_spinner()
            shinyalert::shinyalert(
              "Task complete!", "The report has been send to all recipents.",
              type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
            )
          }
        }
      }
    })
    
    
    
    
    
    # Data #####################################################################
    
    output$data_selection <- shiny::renderUI({
      shiny::req(!base::is.null(modrval$grades))
      shiny::req(!base::is.null(modrval$feedback))
      shiny::req(!base::is.null(modrval$students))
      shiny::req(!base::is.null(modrval$recipients))
      datasets <- base::character(0)
      if (base::nrow(modrval$grades) > 0) datasets <-c(datasets, "grades")
      if (base::nrow(modrval$feedback) > 0) datasets <-c(datasets, "feedback")
      if (base::nrow(modrval$students) > 0) datasets <-c(datasets, "students")
      if (base::nrow(modrval$recipients) > 0)
        datasets <-c(datasets, "recipients")
      shinyWidgets::radioGroupButtons(
        ns("slctdataset"), "Data to display:",
        choices = datasets, selected = datasets[1], size = "sm",
        checkIcon = base::list(yes = shiny::icon("check"))
      )
    })
    
    output$selected_data <- DT::renderDataTable({
      shiny::req(!base::is.null(input$slctdataset))
      modrval[[input$slctdataset]]
    }, options = base::list(pageLength = 20))
    
    output$display_data <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::uiOutput(ns("data_selection")),
          DT::dataTableOutput(ns("selected_data"))
        )
      )
    })
  })
}

