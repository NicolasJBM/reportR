#' @name feedbacks_name_new_item
#' @title New item name
#' @author Nicolas Mangin
#' @description Function creating a name for a new item in feedbacks.
#' @param existing_names Character vector. Item names already assigned.
#' @return Character string. New item name.
#' @importFrom stringr str_remove_all
#' @export


feedbacks_name_new_item <- function(existing_names = NULL){
  base::stopifnot(!base::is.null(existing_names))
  
  existing_numbers <- base::as.numeric(
    stringr::str_remove_all(existing_names, "I")
  )
  
  maximum <- base::max(existing_numbers)
  assign <- maximum + 1
  
  newname <- base::paste(
    c(
      "I",
      base::paste(base::rep(0,9-base::nchar(assign)), collapse = ""),
      assign
    ),
    collapse = ""
  )
  
  return(newname)
}
