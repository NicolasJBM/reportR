#' @name provide_detailed_feeback
#' @title Provide detailed feedback.
#' @author Nicolas Mangin
#' @description Function formatting the feedback about any type of questions into a HTML table which can be embedded in an e-mail to be sent to the selected student.
#' @param details Tibble. Feedback recorded for all items of all versions of all questions.
#' @param studentid Character. ID of the student.
#' @param labels List. Names for axes and legends (for translations purposes)
#' @return HTML code of the Individual feedback properly formatted in a table.
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra kbl
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra spec_color
#' @importFrom purrr map_int
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr nest
#' @export


provide_detailed_feeback <- function(
    details, studentid,
    labels = base::list(
      values = c("Wrong","Missing","Partial","Correct"),
      table_names =c(
        "Proposition:","This proposition is:", "Therefore your answer is:", "And you earned:", "Explanation", "Topic"
      )
    )
){
  
  correct <- NULL
  grading <- NULL
  item_earned <- NULL
  page_link <- NULL
  page_title <- NULL
  partial_credits <- NULL
  penalty <- NULL
  question_grade <- NULL
  question_points <- NULL
  question_type <- NULL
  value_txt <- NULL
  student <- NULL
  checked <- NULL
  proposition <- NULL
  explanation <- NULL
  question <- NULL
  interrogation <- NULL
  item <- NULL
  question_rank <- NULL
  data <- NULL
  correct_txt <- NULL
  
  details_student <- details |>
    dplyr::filter(student == studentid) |>
    dplyr::mutate(
      value_num = dplyr::case_when(
        value == 0 ~ 0,
        value < 1 ~ 1,
        TRUE ~ 2
      ),
      value_txt = dplyr::case_when(
        value == 0 ~ labels$values[1],
        value < 1 ~ labels$values[3],
        TRUE ~ labels$values[4]
      ),
      correct_num = dplyr::case_when(
        correct < 0 ~ 0,
        question_type %in% c("Statements","Alternatives","Computation") & correct == 0 ~ 0,
        question_type %in% c("Essay","Problem") & correct == 0 ~ 1,
        correct > 0 & correct < 1 ~ 2,
        TRUE ~ 3
      ),
      correct_txt = dplyr::case_when(
        correct < 0 ~ labels$values[1],
        question_type %in% c("Statements","Alternatives","Computation") & correct == 0 ~ labels$values[1],
        question_type %in% c("Essay","Problem") & checked == 0 ~ labels$values[2],
        question_type %in% c("Essay","Problem") & correct == 0 ~ labels$values[2],
        correct > 0 & correct < 1 ~ labels$values[3],
        TRUE ~ labels$values[4]
      ),
      earned_num = dplyr::case_when(
        item_earned <= 0 ~ 0,
        item_earned < weight ~ 1,
        TRUE ~ 2
      )
    )
  
  display_table <- details_student |>
    dplyr::select(proposition, value_txt, correct_txt, item_earned, explanation, page_title)
  base::names(display_table) <- labels$table_names
  
  display_table <- display_table |>
    kableExtra::kbl(format = "html", align = "lcccll") |>
    kableExtra::kable_styling(bootstrap_options = "striped") |>
    kableExtra::column_spec(1, width = "30%") |>
    kableExtra::column_spec(2, width = "2%", bold = TRUE) |>
    kableExtra::column_spec(
      3, width = "2%", bold = TRUE, color = "white",
      background = kableExtra::spec_color(
        details_student$correct_num,
        option = "turbo", begin = 0.35, end = 0.9, direction = -1,
        scale_from = c(0,3)
      )
    ) |>
    kableExtra::column_spec(
      4, width = "2%", bold = TRUE, color = "white",
      background = kableExtra::spec_color(
        details_student$earned_num,
        option = "turbo", begin = 0.35, end = 0.9, direction = -1,
        scale_from = c(0,2)
      )
    ) |>
    kableExtra::column_spec(5, width = "50%")
  
  if (base::all(base::is.na(details_student$page_link))){
    display_table <- display_table |>
      kableExtra::column_spec(6, width = "14%") 
  } else {
    display_table <- display_table |>
      kableExtra::column_spec(
        6, width = "14%",
        link = details_student$page_link
      )
  }
  
  group_questions <- details_student |>
    dplyr::select(
      question, question_type, interrogation,
      question_points, question_grade,
      penalty, partial_credits,
      item
    ) |>
    tibble::rowid_to_column("item_rank") |>
    dplyr::mutate(grading = dplyr::case_when(
      penalty == 1 & partial_credits == 1 ~ " with both penalty and partial credits",
      penalty == 0 & partial_credits == 1 ~ " with no penalty but partial credits",
      penalty == 1 & partial_credits == 0 ~ " with penalty and no partial credits",
      TRUE ~ " with neither penalty nor partial credits"
    )) |>
    dplyr::group_by(
      question, question_type, interrogation,
      question_points, question_grade, grading
    ) |>
    tidyr::nest() |>
    tibble::rowid_to_column("question_rank") |>
    dplyr::mutate(
      text = base::paste0(
        "Question ", question_rank, ":\n",
        interrogation, "\n",
        question_type, grading,
        "\nYou earned ", question_grade, " out of ", question_points, " point(s)"
      ),
      f = purrr::map_int(data, function(x) base::min(x$item_rank)),
      l = purrr::map_int(data, function(x) base::max(x$item_rank))
    )
  for (i in base::seq_len(base::nrow(group_questions))){
    display_table <- display_table |>
      kableExtra::pack_rows(
        group_questions$text[i], group_questions$f[i], group_questions$l[i],
        label_row_css = "background-color:#666; color: #fff;"
      )
  }
  
  display_table
}


