#' Create an adjacency matrix
#' 
#' @description
#' Converts an edges list to an adjacency matrix (also known as connectivity 
#' matrix).
#' 
#' @param data a `data.frame` with the following three columns: `from` (edge), 
#'   `to` (edge), and `weight` (value of the vertice).
#' 
#' @param lower a logical value. If `TRUE` (default), keep values in the lower 
#'   triangle of the matrix. Otherwise their will be replaced by `NA`.
#' 
#' @param upper a logical value. If `TRUE` (default), keep values in the upper 
#'   triangle of the matrix. Otherwise their will be replaced by `NA`.
#' 
#' @param diag a logical value. If `TRUE` (default), keep values in the 
#'   diagonal of the matrix. Otherwise their will be replaced by `NA`.
#'
#' @return An integer vector of the same length as `x`.
#' 
#' @export
#' 
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ## _Add an example_
#' }

adjacency_matrix <- function(data, lower = TRUE, upper = TRUE, diag = TRUE) {
  
  colnames(data) <- c("from", "to", "weight")
  mat <- tidyr::pivot_wider(data, names_from = .data$to, 
                            values_from = .data$weight, values_fn = ~.x)
  r_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- r_names
  
  if (!upper) mat[upper.tri(mat)] <- NA
  if (!lower) mat[lower.tri(mat)] <- NA
  if (!diag)  diag(mat) <- NA
  
  mat
}
