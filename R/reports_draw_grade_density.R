#' @name reports_draw_grade_density
#' @title Grade density
#' @author Nicolas Mangin
#' @description Function drawing grade density and positioning the student relative to the mean and median.
#' @param grades Tibble. List of grades of all students.
#' @param studentid Character. ID of the student.
#' @return ggplot2 density graph.
#' @importFrom stats median
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 ylab
#' @export


reports_draw_grade_density <- function(grades, studentid){
  grade <- NULL
  avg <- base::mean(grades$grade)
  med <- stats::median(grades$grade)
  sg <- grades$grade[base::match(studentid, grades$student)]
  maxx <- base::max(grades$points)
  maxy <- stats::density(stats::na.omit(grades$grade))
  maxy <- base::max(maxy$y)*1.1
  grades |>
    ggplot2::ggplot(ggplot2::aes(x=grade)) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(xintercept = avg, color = "blue", size = 1.25) +
    ggplot2::geom_vline(xintercept = med, color = "forestgreen", size = 1.25) +
    ggplot2::geom_vline(xintercept = sg, color = "red", size = 1.25) +
    ggplot2::annotate("text", label = "Median", x = maxx*0.2, y = maxy*0.95, color = "forestgreen") +
    ggplot2::annotate("text", label = "Average", x = maxx*0.2, y = maxy*0.90, color = "blue") +
    ggplot2::annotate("text", label = "You", x = maxx*0.2, y = maxy*0.85, color = "red") +
    ggplot2::scale_x_continuous(breaks = base::seq(0,maxx,1)) +
    ggplot2::ylab("Count") +
    ggplot2::theme_light()
}
