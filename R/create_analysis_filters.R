


create_analysis_filters <- function(filter, analysis_path, course_paths){
  
  
  
  
  
  
  
  
  analysis_path <- "E:/Dropbox/5-Education/Courses/management_accounting/materials/9_analyses/student_profile"
  course_paths <- list(databases = list())
  load("E:/Dropbox/5-Education/Courses/test.RData")
  
  
  
  
  
  qmdpath <- base::paste0(analysis_path, "/index.qmd")
  analysis <- base::readLines(qmdpath)
  
  if (filter == "tree"){
    
    trees <- course_data$courses$tree
    
  } else if (filter == "language"){
    
    course_data$languages
    
  } else if (filter == "student"){
    
    
    
  } else if (filter == "test"){
    
  } else { #question
    
  }
  
}
