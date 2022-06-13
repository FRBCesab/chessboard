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
#' 
#' @export
#'
#' @examples
#' x <- paste0("A-D 0", 1:9)
#' char_to_int(x)

char_to_int <- function(x) {
  
  as.numeric(gsub("[A-Z]|[a-z]|[[:punct:]]|\\s", "", x))
}
