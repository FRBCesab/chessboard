#' Create an unique edge identifier
#' 
#' @description 
#' _Add an good description_
#'
#' @param data a `data.frame`. Each column will be used to create an unique 
#'   edge identifier.
#'
#' @return A character vector with edges labels.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## _Add an example_
#' }

recode_units <- function(data) {
  
  data$"key" <- unlist(lapply(seq_len(nrow(data)), function(x) {
    paste(as.vector(data[x, ]), collapse = "__")
  }))
  
  data
}
