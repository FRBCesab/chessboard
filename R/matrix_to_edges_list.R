#' Convert an adjacency matrix to an edges list
#' 
#' @description
#' Converts an adjacency matrix to an edges list. This function allows to create
#' the same edges list as the one obtained with [edges_list()].
#' 
#' @param x a `matrix` object. The adjacency matrix to be converted in edges 
#'   list.
#' 
#' @param all a logical value. If `FALSE` (default), removes missing edges.
#'
#' @return A `data.frame` with four columns:
#'   - `edge_id`: label of the edge
#'   - `edge`: 0 (no edge) or 1 (edge)
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "chessboard")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Retrieve nodes (from nodes vector) ----
#' adour_nodes <- nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood (undirected network) ----
#' adour_edges <- edges_list(adour_nodes)
#' adour_edges
#' 
#' # Get adjacency matrix ----
#' adour_adj_matrix <- adjacency_matrix(adour_edges)
#' adour_adj_matrix
#' 
#' # Convert back to edges list ----
#' new_edges <- matrix_to_edges_list(adour_adj_matrix)
#' new_edges
#' 
#' # Check ----
#' identical(adour_edges, new_edges)
#' }

matrix_to_edges_list <- function(x, all = FALSE) {
  
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
  
  if (sum(!is.na(x) & x != 0) == 0) {
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
    x <- x[ , -which(colnames(x) == "edge")]
  }

  rownames(x) <- NULL
  x
}
