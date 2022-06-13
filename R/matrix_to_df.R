#' Convert an adjacency matrix to a data frame
#' 
#' @description
#' Converts an adjacency matrix to a data frame
#' 
#' @param mat a `matrix` object. The adjacency table to be converted.
#' 
#' @param na_rm a logical value. If `TRUE` (default), removes `NA` vertices.
#'
#' @return A `data.frame` with three columns:
#'   - `from`: label of one of the two edges of the vertice (link)
#'   - `to`: label of the other edge of the vertice
#'   - `weight`: the presence or absence of vertice between two edges
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## _Add an example_
#' }

matrix_to_df <- function(mat, na_rm = TRUE) {
  
  mat <- as.data.frame(mat)
  mat <- data.frame("from" = rownames(mat), mat)
  rownames(mat) <- NULL
  
  mat <- tidyr::pivot_longer(mat, cols = -1, names_to = "to", 
                             values_to = "weight")
  
  mat <- as.data.frame(mat)
  
  if (na_rm) {
    mat[!is.na(mat$"weight"), ] 
  } else {
    mat
  }
}
