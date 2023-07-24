



read_analysis_metainfo <- function(analysis_path, course_paths){
  
  lines <- NULL
  type <- NULL
  use <- NULL
  
  qmdpath <- base::paste0(analysis_path, "/index.qmd")
  analysis <- base::readLines(qmdpath)
  metainfo <- tibble::tibble(
    lines = analysis[base::match('#Meta-information', analysis):base::length(analysis)][-c(1,2)]
  ) |>
    dplyr::mutate(lines = stringr::str_remove_all(lines, "#")) |>
    dplyr::filter(base::nchar(lines) > 4) |>
    tidyr::separate(lines, into = c("type","name","use"), sep = "-") |>
    dplyr::filter(use == TRUE) |>
    dplyr::select(-use)
  
  databases <- dplyr::filter(metainfo, type == "dataset")$name
  datasets <- base::list()
  for (d in databases){
    base::file.copy(
      from = course_paths$databases[[d]],
      to = base::paste0(analysis_path, "/data/", d, ".RData")
    )
  }
  
  filters <- dplyr::filter(metainfo, type == "filter")$name
  
  return(filters)
}
