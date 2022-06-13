#' Find links between edges based on number of neighbors
#' 
#' @description
#' Finds links between edges based on number of neighbors. The edges (argument 
#' `x`) will be ordered to find links in a directional way (from upstream to
#' downstream). This order must be found in the edges labels.
#' 
#' @param x a character vector of edges labels.
#' 
#' @param level an integer. The number of neighbors used define vertices.
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

make_links <- function(x, level = 1) {
  
  if (level > (length(x) - 1)) {
    stop("Argument 'level' must be 'strictly < length(x)'")
  }
  
  x <- sort(x)
  dict <- data.frame(x, "x_int" = seq_len(length(x)))
  
  x <- expand.grid(x, x)
  colnames(x) <- c("from", "to")
  
  x <- x[order(x$"from"), ]
  
  x <- merge(x, dict, by.x = "to", by.y = "x")
  colnames(x)[ncol(x)] <- "to_int"
  
  x <- merge(x, dict, by.x = "from", by.y = "x")
  colnames(x)[ncol(x)] <- "from_int"
  
  x <- x[with(x, order(from, to)), ]
  
  x$"weight" <- 0
  x[which(abs(x$"from_int" - x$"to_int") <= level), "weight"] <- 1
  
  x[ , -c(3:4)]
}
