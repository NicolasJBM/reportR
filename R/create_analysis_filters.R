


create_analysis_filters <- function(filter, course_data){
  
  if (filter == "tree"){
    
    trees <- course_data$courses$tree
    
  } else if (filter == "language"){
    
    course_data$languages
    
  } else if (filter == "student"){
    
    
    
  } else if (filter == "test"){
    
  } else { #question
    
  }
  
}
