#' @name reports_feedback_on_open
#' @title Essay feedback
#' @author Nicolas Mangin
#' @description Function formatting the feedback about open-ended questions (essays) into a HTML table.
#' @param feedback Tibble. Feedback recorded for all versions of all questions.
#' @param studentid Character. ID of the student.
#' @param questionid Character. ID of the question.
#' @return HTML code for a properly formatted table showing the individual feedback.
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr case_when
#' @importFrom kableExtra kbl
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra spec_color
#' @export


reports_feedback_on_open <- function(feedback, studentid, questionid){
  
  question <- NULL
  item <- NULL
  checked <- NULL
  success <- NULL
  student <- NULL
  proposition <- NULL
  weight <- NULL
  earned <- NULL
  explanation <- NULL
  section <- NULL
  attempt <- NULL
  
  feedback_question <- feedback |>
    dplyr::filter(question == questionid)
  item_statistics <- feedback_question |>
    dplyr::group_by(item) |>
    dplyr::summarise(success = base::mean(checked)) |>
    dplyr::mutate(success = base::paste0(base::round(success*100,0), "%"))
  feedback_student <- feedback_question |>
    dplyr::filter(student == studentid) |>
    dplyr::left_join(item_statistics, by = "item")
  display_table <- feedback_student |>
    dplyr::select(
      Criteria = proposition,
      Points = weight,
      'Success rate' = success,
      Earned = earned,
      Explanation = explanation,
      'Go to the page' = section
    ) |>
    kableExtra::kbl(format = "html", align = "lccclr") |>
    kableExtra::kable_styling(bootstrap_options = c("striped")) |>
    kableExtra::column_spec(1, width = "25%") |>
    kableExtra::column_spec(2, width = "5%") |>
    kableExtra::column_spec(3, width = "5%") |>
    kableExtra::column_spec(
      4, width = "5%", bold = TRUE, color = "white",
      background = kableExtra::spec_color(
        feedback_student$earned/feedback_student$weight,
        option = "turbo", begin = 0.35, end = 0.9, direction = -1,
        scale_from = c(0,1)
      )
    ) |>
    kableExtra::column_spec(5, width = "40%")
  if (base::all(base::is.na(feedback_student$link))){
    display_table |>
      kableExtra::column_spec(6, width = "20%") 
  } else {
    display_table |>
      kableExtra::column_spec(
        6, width = "20%",
        link = feedback_student$link
      ) 
  }
}

