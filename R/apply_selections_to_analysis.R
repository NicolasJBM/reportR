#' @name apply_selections_to_analysis
#' @title Change selections and filters
#' @author Nicolas Mangin
#' @description Function updating the relevant selections filters in the selected quarto document.
#' @param selections Character vector. Selected tree, language, student, test, and question.
#' @param analysis_path Character. Path to the folder containing the analysis.
#' @return Write the selections in the quarto document.
#' @export


apply_selections_to_analysis <- function(selections, analysis_path){
  
  value <- NULL
  variable <- NULL
  
  analysis <- base::readLines(analysis_path)
  
  section <- analysis |>
    stringr::str_locate('^treeid <- |^questionid <- ') |>
    base::as.data.frame() |>
    tibble::rowid_to_column("section") |>
    stats::na.omit() |>
    dplyr::select(section) |>
    base::unlist() |>
    base::as.vector()
  
  lines <- selections |>
    dplyr::mutate(lines = base::paste0(variable, ' <- "', value, '"')) |>
    dplyr::select(lines) |>
    base::unlist() |>
    base::as.vector()
  
  analysis[section[1]:section[2]] <- lines
  
  base::writeLines(analysis, analysis_path)
}
