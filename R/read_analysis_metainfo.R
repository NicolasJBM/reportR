



read_analysis_metainfo <- function(analysis_path, course_data){
  
  #analysis_path <- "E:/Dropbox/5-Education/Courses/management_accounting/materials/9_analyses/student_profile"
  #course_paths <- list(databases = list())
  #course_paths$databases$tests <- "E:/Dropbox/5-Education/Courses/management_accounting/basis/4_analytics/2_data/tests.RData"
  #course_paths$databases$students <- "E:/Dropbox/5-Education/Courses/management_accounting/basis/4_analytics/2_data/students.RData"
  #course_paths$databases$results <- "E:/Dropbox/5-Education/Courses/management_accounting/basis/4_analytics/2_data/results.RData"
  
  
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
    base::save(
      course_data[[d]],
      file = base::paste0(analysis_path, "/data/", d, ".RData")
    )
  }
  
  filters <- dplyr::filter(metainfo, type == "filter")$name
  
  return(filters)
}