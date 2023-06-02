#' Convert an connectivity matrix to an edge list
#' 
#' @description
#' Converts a connectivity matrix to an edge list. This function allows to 
#' create the same edge list as the one obtained with [create_edge_list()].
#' 
#' @param x a `matrix` object. The connectivity matrix to be converted in an
#'   edge list.
#' 
#' @param all a `logical` value. If `FALSE` (default), removes missing edges.
#'
#' @return A `data.frame` with two (or three) columns:
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#'   - `edge`: 0 (no edge) or 1 (edge). This column is returned only if 
#'   `all = TRUE`.
#' 
#' @export
#'
#' @examples
#' library("chessboard")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' sites_infos
#' 
#' nodes <- create_node_labels(data     = sites_infos, 
#'                             transect = "transect", 
#'                             quadrat  = "quadrat")
#' 
#' edges <- create_edge_list(nodes, method = "pawn", directed = TRUE)
#' 
#' conn_matrix <- connectivity_matrix(edges)
#' 
#' # Convert back to edge list ----
#' new_edges <- matrix_to_edge_list(conn_matrix)
#' new_edges
#' 
#' # Check ----
#' identical(edges, new_edges)

matrix_to_edge_list <- function(x, all = FALSE) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.matrix(x)) {
    stop("Argument 'x' must be a matrix (connectivity matrix)", call. = FALSE)
  }
  
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric matrix (connectivity matrix)", 
         call. = FALSE)
  }
  
  if (nrow(x) != ncol(x)) {
    stop("Number of rows of 'x' must be equal to number of columns ", 
         "(connectivity matrix)", call. = FALSE)
  }
  
  if (is.null(rownames(x))) {
    stop("Row names of 'x' must contain nodes labels", call. = FALSE)
  }
  
  if (any(!(rownames(x) %in% colnames(x)))) {
    stop("Row names and column names of 'x' must be equal", call. = FALSE)
  }
  
  if (sum(x, na.rm = TRUE) == 0) {
    stop("Argument 'x' contains no edge", call. = FALSE)
  }
  
  
  ## Prepare for pivot ----
  
  x <- as.data.frame(x)
  x <- data.frame("from" = rownames(x), x)
  rownames(x) <- NULL
  colnames(x)[-1] <- x$"from"
  
  
  ## Pivot to longer format ----
  
  x <- tidyr::pivot_longer(x, cols = -1, names_to = "to", values_to = "edge")
  
  x <- as.data.frame(x)
  
  
  ## Remove non edges ----
  
  if (!all) {
    x <- x[which(!is.na(x$"edge") & x$"edge" != 0), ]
    # x <- x[ , -which(colnames(x) == "edge")]
  }

  rownames(x) <- NULL
  x
}
