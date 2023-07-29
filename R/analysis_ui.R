#' @name analysis_ui
#' @title Analyse data
#' @author Nicolas Mangin
#' @description Module allowing the user to analyse data about teaching, learning, and testing materials.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A quarto document which can be rendered in different formats.
#' @export


analysis_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        3,
        shinydashboardPlus::box(
          width = 12, title = "Selection", solidHeader = TRUE,
          status = "navy", collapsible = FALSE, collapsed = FALSE,
          height = "750px",
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::actionButton(
                ns("newanalysis"), label = "New",
                icon = shiny::icon("wand-magic-sparkles"),
                style = "background-color:#000066;color:#FFF;width:100%;margin-bottom:10px;"
              )
            ),
            shiny::column(
              6,
              shiny::actionButton(
                ns("refreshlist"), label = "Refresh",
                icon = shiny::icon("rotate"),
                style = "background-color:#006699;color:#FFF;width:100%;margin-bottom:10px;"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              7,
              shiny::selectInput(
                ns("slctanalysis"), "Select an analysis: ",
                choices = base::character(0), selected = base::character(0)
              )
            ),
            shiny::column(
              5,
              shinyWidgets::switchInput(
                inputId = ns("editionswitch"), label = "Edition mode", value = FALSE,
                onStatus = "success", offStatus = "danger", width = "100%",
              )
            )
          ),
          shiny::selectInput(
            ns("slcttree"), "Tree: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slctlanguage"), "Language: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slctlastname"), "Lastname: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slctfirstname"), "Firstname: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slctstudent"), "Student ID: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slcttest"), "Test: ",
            choices = "", selected = ""
          ),
          shiny::selectInput(
            ns("slctquestion"), "Question: ",
            choices = "", selected = ""
          ),
          shiny::actionButton(
            ns("applyselection"), label = "Apply selection",
            icon = shiny::icon("print"),
            style = "background-color:#990033;color:#FFF;width:100%;margin-bottom:10px;"
          )
        )
      ),
      shiny::column(
        9,
        shiny::uiOutput(ns("analysis"))
      )
    )
  )
}

