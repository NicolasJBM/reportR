#' @name retrieve_data_for_analysis
#' @title Get analyzed data
#' @author Nicolas Mangin
#' @description Function copying and pasting required databases in the data folder of the selected analysis.
#' @param analysis_path Character. Path to the folder containing the analysis.
#' @param course_paths List. List of paths to the different folders and databases on local disk.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @export



retrieve_data_for_analysis <- function(analysis_path, course_paths){
  
  lines <- NULL
  type <- NULL
  use <- NULL
  
  qmdpath <- base::paste0(analysis_path, "/index.qmd")
  analysis <- base::readLines(qmdpath)
  metainfo <- tibble::tibble(
    lines = analysis[base::match('#DATA_TO_ANALYZE', analysis):base::length(analysis)][-1]
  ) |>
    dplyr::mutate(lines = stringr::str_remove_all(lines, "#")) |>
    dplyr::filter(base::nchar(lines) > 4) |>
    tidyr::separate(lines, into = c("name","use"), sep = ":") |>
    dplyr::filter(use == TRUE)
  databases <- metainfo$name
  for (d in databases){
    base::file.copy(
      from = course_paths$databases[[d]],
      to = base::paste0(analysis_path, "/data/", d, ".RData")
    )
  }
}
