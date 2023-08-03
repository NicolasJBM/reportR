#' @name customize_comment
#' @title Customized comments
#' @author Nicolas Mangin
#' @description Function selecting the appropriate comment to be addressed to a student based on her or his grade.
#' @param grade Numeric. Grade of the student.
#' @param structure Tibble. Variables: threshold, status, color, and comment. Comment is not necessary.
#' @return character. Comments adapted to the grade obtained by the student.
#' @importFrom dplyr filter
#' @export


customize_comment <- function(grade, structure){
  threshold <- NULL
  comment <- structure |>
    dplyr::filter(threshold > grade) |>
    dplyr::filter(threshold == base::min(threshold))
  return(comment$comment[1])
}
