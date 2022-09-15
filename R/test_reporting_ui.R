#' @name test_reporting_ui
#' @title Edit reports
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of individual feedback or course analyses.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the report in "6_reports" and allows individualizing and sending it to a list of recipients.
#' @import shiny
#' @export


test_reporting_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    
    shinydashboard::tabBox(
      side = "left", width = "100%",
      
      
      
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("file-medical-alt"),"Report"),
        shiny::fluidRow(
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("reporttype"),
              label = "Type of report", 
              choices = c("feedback", "analysis"),
              selecte = "feedback",
              status = "danger", justified = TRUE, size = "sm",
              checkIcon = base::list(yes = shiny::icon("check"))
            )
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("select_tests"))
          ),
          shiny::column(
            3,
            shiny::uiOutput(ns("select_report"))
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("new_report"), "New", icon = shiny::icon("magic"),
              style = "background-color:#003366;color:#FFF;width:100%;
                margin-top:25px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::uiOutput(ns("select_recipient"))
          ),
          shiny::column(
            4,
            shiny::textInput(
              ns("mailsubject"), "Subject of the message:", "", width = "100%"
            )
          ),
          shiny::column(
            3,
            shiny::textInput(ns("emailtest"), "Test e-mail", width = "100%")
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("sendtestemail"), "Test",
              icon = shiny::icon("paper-plane"),
              style = "background-color:#660033;color:#FFF;
                width:100%;margin-top:25px;"
            )
          )
        ),
        
        
        shiny::fluidRow(
          shiny::column(
            1,
            shiny::actionButton(
              ns("previousstudent"), "",
              icon = shiny::icon("chevron-circle-left"),
              style = "background-color:#660000;color:#FFF;width:80px;
            height:75px;font-size:40px;text-align:center;"
            )
          ),
          shiny::column(
            10,
            shiny::uiOutput(ns("student_rank_selection"))
          ),
          shiny::column(
            1,
            shiny::actionButton(
              ns("nextstudent"), "",
              icon = shiny::icon("chevron-circle-right"),
              style = "background-color:#000066;color:#FFF;width:80px;
            height:75px;font-size:40px;text-align:center;"
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput(ns("viewreport"))),
          shiny::column(6, shiny::uiOutput(ns("editreport")))
          
        )
      ),
      
      
      
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("envelope-open-text"),"Mailing"),
        shiny::fluidRow(
          shiny::column(
            3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("sendingpkg"),
              label = "Package used to send e-mails", 
              choices = c("gmailr", "blastula"), selected = "gmailr",
              status = "danger", justified = TRUE, size = "sm",
              checkIcon = base::list(yes = shiny::icon("check"))
            )
          ),
          shiny::column(
            9,
            shiny::uiOutput(ns("defcredentials"))
          )
        ),
        
        shiny::tags$hr(),
        
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::fileInput(
              ns("recipientlisttoimport"), "Recipient list",
              accept = ".csv", width = "100%"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("importrecipientlist"), "Import",
              icon = shiny::icon("upload"),
              style = "background-color:#003366;color:#FFF;
                width:100%;margin-top:25px;"
            )
          )
        ),
        
        shiny::tags$hr(),
        
        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("sendingmethod"),
              label = "Sending method", 
              choices = c("individual","collective"), selected = "individual",
              status = "danger", justified = TRUE, size = "sm",
              checkIcon = base::list(yes = shiny::icon("check"))
            )
          ),
          shiny::column(
            2,
            shiny::textOutput(ns("recipientnbr")),
          ),
          shiny::column(
            2,
            shinyWidgets::switchInput(
              inputId = ns("sendbyteam"),
              label = "Group e-mails by team?",
              onLabel = "Yes", offLabel = "No",
              onStatus = "success", offStatus = "danger",
              value = FALSE,
              labelWidth = "200px", handleWidth = "50px"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("sendtoall"), "Send",
              icon = shiny::icon("envelope"),
              style = "background-color:#990033;color:#FFF;
                width:100%;margin-top:25px;"
            )
          )
        )
      ),
      
      
      
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("database"),"data"),
        shiny::uiOutput(ns("display_data"))
      )
      
      
      
    )
    
  )
}

