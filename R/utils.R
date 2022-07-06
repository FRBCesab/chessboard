#' Convert a character to an integer
#' 
#' @description
#' Converts a character to an integer by removing non-number characters 
#' (letters, punctuation, special characters, ...).
#' 
#' **Important:** Use this function with caution: `"AD11"` will result in `11` 
#' and `"A1D1"`, `"1A-D1"`, etc. will also result in `11`. 
#' 
#' @note This function is no more in use.
#' 
#' @param x a character vector. Must contain numbers.
#'
#' @return An integer vector of the same length as `x`.

char_to_int <- function(x) {
  
  as.numeric(gsub("[A-Z]|[a-z]|[[:punct:]]|\\s", "", x))
}



#' Create an unique node identifier
#'
#' @param data a `data.frame`. Each column will be used to create an unique 
#'   edge identifier.
#'
#' @note This function is no more in use.
#'
#' @return A character vector with nodes labels.

create_unique_key <- function(data) {
  
  data$"key" <- unlist(lapply(seq_len(nrow(data)), function(x) {
    paste(as.vector(data[x, ]), collapse = "__")
  }))
  
  data
}
