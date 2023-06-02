#' Create a connectivity matrix from an edge list
#' 
#' @description
#' Converts an edge list to an connectivity matrix (also known as adjacency
#' matrix).
#' 
#' @param edges a `data.frame` with the following two columns: `from` (the 
#'   first node of the edge) and `to` (the second node of the edge). The output
#'   of the functions [create_edge_list()] or [append_edge_lists()].
#' 
#' @param lower a `logical` value. If `TRUE` (default), keep values in the 
#'   lower triangle of the matrix. Otherwise they will be replaced by `NA` 
#'   (or `0`).
#' 
#' @param upper a `logical` value. If `TRUE` (default), keep values in the 
#'   upper triangle of the matrix. Otherwise they will be replaced by `NA` 
#'   (or `0`).
#' 
#' @param diag a `logical` value. If `TRUE` (default), keep values in the 
#'   diagonal of the matrix. Otherwise they will be replaced by `NA` 
#'   (or `0`).
#'   
#' @param na_to_zero a `logical` value. If `TRUE` (default), missing edges are 
#'   coded as `0`. Otherwise they will be coded as `NA`.
#'
#' @return A connectivity matrix of dimensions `n x n`, where `n` is the number
#'   of nodes.
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
#'                             package = "chessboard")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Select first location ----
#' adour_sites <- adour_sites[adour_sites$"location" == 1, ]
#' 
#' # Create nodes labels ----
#' adour_nodes <- create_node_labels(data     = adour_sites, 
#'                                   location = "location", 
#'                                   transect = "transect", 
#'                                   quadrat = "quadrat")
#' 
#' # Find edges with 1 degree of neighborhood (pawn method) ----
#' adour_edges <- create_edge_list(adour_nodes, method = "pawn", 
#'                                 directed = TRUE)
#' 
#' # Get connectivity matrix ----
#' connectivity_matrix(adour_edges)
#' 
#' # Get connectivity matrix ----
#' connectivity_matrix(adour_edges, na_to_zero = FALSE)

connectivity_matrix <- function(edges, lower = TRUE, upper = TRUE, diag = TRUE,
                                na_to_zero = TRUE) {
  
  ## Check arguments ----
  
  check_edges_object(edges)
  check_logical_value(lower)
  check_logical_value(upper)
  check_logical_value(diag)
  check_logical_value(na_to_zero)
  
  
  ## Complete missing edges ----
  
  nodes <- get_sorted_nodes(edges)
  
  edges_all <- expand.grid(nodes, nodes, stringsAsFactors = FALSE)
  colnames(edges_all) <- c("from", "to")
  
  edges_all$"key" <- paste0(edges_all$"from", edges_all$"to")
  edges$"key"     <- paste0(edges$"from", edges$"to")
  edges$"edge"    <- 1 
  
  edges <- merge(edges_all, edges[ , c("edge", "key")], by = "key", all = TRUE)
  edges <- edges[ , c("from", "to", "edge")]
  
  
  ## Replace missing edges ----
  
  if (na_to_zero) {
    
    edges$"edge" <- ifelse(is.na(edges$"edge"), 0, edges$"edge") 
  
  } else {
    
    edges$"edge" <- ifelse(is.na(edges$"edge") | edges$"edge" == 0, NA, 
                           edges$"edge")
  }
  
  
  ## Pivot data frame ----
  
  mat <- tidyr::pivot_wider(edges, names_from = "to", 
                            values_from = "edge", values_fn = ~.x)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Order matrix ----
  
  mat <- mat[nodes, nodes]
  
  
  ## Apply filters ----
  
  if (!upper) mat[upper.tri(mat)] <- ifelse(na_to_zero, 0, NA)
  if (!lower) mat[lower.tri(mat)] <- ifelse(na_to_zero, 0, NA)
  if (!diag)  diag(mat)           <- ifelse(na_to_zero, 0, NA)
  
  mat
}
