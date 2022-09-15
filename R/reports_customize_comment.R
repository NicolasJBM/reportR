#' @name reports_customize_comment
#' @title Customized comments
#' @author Nicolas Mangin
#' @description Function selecting the appropriate comment to be addressed to a student based on her or his grade.
#' @param grade Numeric. Grade of the student.
#' @param thresholds Numeric vector. Grade at which the comment changes.
#' @param comments Character vector. Comment associated to each threshold.
#' @return character. Comments adapted to the grade obtained by the student.
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @export


reports_customize_comment <- function(grade, thresholds, comments){
  base::stopifnot(
    base::length(thresholds) == base::length(comments)
  )
  
  lower_bound <- NULL
  selected <- NULL
  
  selected <- tibble::tibble(
    lower_bound = thresholds,
    comments = comments
  ) |>
    dplyr::filter(lower_bound <= grade) |>
    dplyr::filter(lower_bound == base::max(lower_bound))
  return(selected$comments[1])
}
