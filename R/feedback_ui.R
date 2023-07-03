#' @name feedback_ui
#' @title Edit feedbacks
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of individual feedback or course analyses.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the feedback in "9_feedback" and allows individualizing and sending it to a list of recipients.
#' @importFrom editR selection_ui
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny uiOutput
#' @importFrom shinydashboardPlus box
#' @export


feedback_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::uiOutput(ns("slctlanguage"))
      ),
      shiny::column(
        4,
        editR::selection_ui(ns("slctstudent"), "Student:")
      ),
      shiny::column(
        4,
        editR::selection_ui(ns("slctattempt"), "Attempt:")
      ),
      shiny::column(
        2,
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::actionButton(
              ns("newfeedback"), "New", icon = shiny::icon("wand-magic-sparkles"),
              style = "background-color:#000066;color:#FFF;width:100%;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("sendfeedback"), "Send to all", icon = shiny::icon("paper-plane"),
              style = "background-color:#330066;color:#FFF;width:100%;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("openfolder"), "Open folder", icon = shiny::icon("folder-open"),
              title = "Open the credentials folder to add or edit credentials.",
              style = "background-color:#660000;color:#FFF;width:100%;margin-bottom:10px;"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shinydashboardPlus::box(
          width = 12, title = "Edition", solidHeader = TRUE,
          status = "navy", collapsible = FALSE, collapsed = FALSE,
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                ns("feedbackinrstudio"), "RStudio",
                icon = shiny::icon("r-project"),
                style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              5,
              shiny::actionButton(
                ns("refreshfeedback"), "Refresh",
                icon = shiny::icon("rotate"),
                style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              3,
              shiny::actionButton(
                ns("savefeedback"), "Save",
                icon = shiny::icon("floppy-disk"),
                style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::uiOutput(ns("edit_feedback"))
            )
          )
        )
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("preview_feedback"))
      ),
      shiny::column(
        4,
        shinydashboardPlus::box(
          width = 12, title = "Mailing", solidHeader = TRUE,
          status = "maroon", collapsible = FALSE, collapsed = FALSE,
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::uiOutput(ns("slctcredentials")),
              shiny::tags$hr(),
              shiny::textAreaInput(
                ns("mailsubject"), "Subject of the message:",
                "Feedback on your test.",
                width = "100%"
              ),
              shiny::textInput(ns("emailtest"), "Test e-mail", width = "100%"),
              shiny::actionButton(
                ns("sendtestemail"), "Send test", icon = shiny::icon("envelope"),
                style = "background-color:#FF4500;color:#FFF;width:100%;margin-top:25px;"
              )
            )
          )
        )
      )
    )
  )
}

