#' @name reports_feedback_on_mcq_num
#' @title MCQ feedback
#' @author Nicolas Mangin
#' @description Function formatting the feedback about multiple choice questions into a HTML table.
#' @param feedback Tibble. Feedback recorded for all versions of all questions.
#' @param studentid Character. ID of the student.
#' @return HTML code Individual feedback properly formatted
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


reports_feedback_on_mcq_num <- function(feedback, studentid){
  
  type <- NULL
  checked <- NULL
  item <- NULL
  earned <- NULL
  success <- NULL
  student <- NULL
  interrogation <- NULL
  weight <- NULL
  proposition <- NULL
  correct_text <- NULL
  explanation <- NULL
  section <- NULL
  attempt <- NULL
  question <- NULL
  
  feedback_mcq_num <- feedback |>
    dplyr::filter(type %in% c("Statements","Alternatives","Computation"))
  item_statistics <- feedback_mcq_num |>
    dplyr::filter(checked == 1) |>
    dplyr::group_by(item) |>
    dplyr::summarise(success = base::mean(earned > 0)) |>
    dplyr::mutate(success = base::paste0(base::round(success*100,0), "%"))
  feedback_student <- feedback_mcq_num |>
    dplyr::filter(student == studentid, checked == 1) |>
    dplyr::left_join(item_statistics, by = "item") |>
    dplyr::mutate(correct_text = dplyr::case_when(
      correct == 1 ~ "Right",
      TRUE ~ "Wrong"
    ))
  display_table <- feedback_student |>
    dplyr::select(
      Version = version,
      Question = interrogation,
      Points = weight,
      'You selected' = proposition,
      'Your answer was' = correct_text,
      'Success rate' = success,
      Earned = earned,
      Explanation = explanation,
      'Go to the page' = section
    ) |>
    kableExtra::kbl(format = "html", align = "llccccclr") |>
    kableExtra::kable_styling(bootstrap_options = c("striped")) |>
    kableExtra::column_spec(1, width = "5%%") |>
    kableExtra::column_spec(2, width = "15%") |>
    kableExtra::column_spec(3, width = "5%") |>
    kableExtra::column_spec(4, width = "20%") |>
    kableExtra::column_spec(
      5, width = "5%", bold = TRUE, color = "white",
      background = kableExtra::spec_color(
        feedback_student$correct,
        option = "turbo", begin = 0.35, end = 0.9, direction = -1,
        scale_from = c(0,1)
      )
    ) |>
    kableExtra::column_spec(6, width = "5%") |>
    kableExtra::column_spec(
      7, width = "5%", bold = TRUE, color = "white",
      background = kableExtra::spec_color(
        feedback_student$earned/feedback_student$weight,
        option = "turbo", begin = 0.35, end = 0.9, direction = -1,
        scale_from = c(-0.25,1)
      )
    ) |>
    kableExtra::column_spec(8, width = "20%")
  if (base::all(base::is.na(feedback_student$link))){
    display_table |>
      kableExtra::column_spec(9, width = "20%") 
  } else {
    display_table |>
      kableExtra::column_spec(
        9, width = "20%",
        link = feedback_student$link
      ) 
  }
}


