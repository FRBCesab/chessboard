#' Create an adjacency matrix
#' 
#' @description
#' Converts an edges list to an adjacency matrix (also known as connectivity 
#' matrix).
#' 
#' @param edges a `data.frame` with the following three columns: `from` (the 
#'   first node of the edge), `to` (the second node of the edge), and `edge` 
#'   (presence/absence of the edge).
#' 
#' @param lower a logical value. If `TRUE` (default), keep values in the lower 
#'   triangle of the matrix. Otherwise they will be replaced by `NA`.
#' 
#' @param upper a logical value. If `TRUE` (default), keep values in the upper 
#'   triangle of the matrix. Otherwise they will be replaced by `NA`.
#' 
#' @param diag a logical value. If `TRUE` (default), keep values in the 
#'   diagonal of the matrix. Otherwise they will be replaced by `NA`.
#'   
#' @param na_to_zero a logical value. If `TRUE` (default), missing edges are 
#'   coded as `0`. Otherwise they will be coded as `NA`.
#'
#' @return An adjacency matrix of dimensions `n x n`, where `n` is the number 
#'   of nodes.
#' 
#' @export
#' 
#' @importFrom rlang .data
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Retrieve nodes (from nodes vector) ----
#' adour_nodes <- nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood (undirected) ----
#' adour_edges <- edges_list(adour_nodes)
#' 
#' # Get adjacency matrix ----
#' adjacency_matrix(adour_edges)
#' 
#' # Get adjacency matrix ----
#' adjacency_matrix(adour_edges, na_to_zero = FALSE)
#' 
#' # Find edges with 1 degree of neighborhood (directed) ----
#' adour_edges <- edges_list(adour_nodes, directed = TRUE)
#' 
#' # Get adjacency matrix ----
#' adjacency_matrix(adour_edges)
#' 
#' # Find edges with 1 degree of neighborhood (auto-links) ----
#' adour_edges <- edges_list(adour_nodes, self = TRUE)
#' 
#' # Get adjacency matrix ----
#' adjacency_matrix(adour_edges)
#' 
#' # Get adjacency matrix (remove upper triangle) ----
#' adjacency_matrix(adour_edges, upper = FALSE)
#' 
#' # Get adjacency matrix (remove lower triangle) ----
#' adjacency_matrix(adour_edges, lower = FALSE)
#' 
#' # Get adjacency matrix (remove diagonal) ----
#' adjacency_matrix(adour_edges, diag = FALSE)

adjacency_matrix <- function(edges, lower = TRUE, upper = TRUE, diag = TRUE,
                             na_to_zero = TRUE) {
  
  ## Check edges argument ----
  
  if (missing(edges)) {
    stop("Argument 'edges' is required", call. = FALSE)
  }
  
  if (!is.data.frame(edges)) {
    stop("Argument 'edges' must be a data.frame", call. = FALSE)
  }
  
  if (!("from" %in% colnames(edges))) {
    stop("The column 'from' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(edges))) {
    stop("The column 'to' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("edge" %in% colnames(edges))) {
    stop("The column 'edge' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("edge_id" %in% colnames(edges))) {
    stop("The column 'edge_id' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (nrow(edges) == 0) {
    stop("Argument 'edges' must have at least one row", call. = FALSE)
  }
  
  
  ## Complete missing edges ----
  
  nodes <- nodes_list(edges)
  edges_all <- expand.grid(nodes, nodes, stringsAsFactors = FALSE)
  colnames(edges_all) <- c("from", "to")
  
  edges_all$"key" <- paste0(edges_all$"from", edges_all$"to")
  edges$"key"     <- paste0(edges$"from", edges$"to")
  
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
  
  mat <- tidyr::pivot_wider(edges, names_from = .data$to, 
                            values_from = .data$edge, values_fn = ~.x)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Apply filters ----
  
  if (!upper) mat[upper.tri(mat)] <- ifelse(na_to_zero, 0, NA)
  if (!lower) mat[lower.tri(mat)] <- ifelse(na_to_zero, 0, NA)
  if (!diag)  diag(mat)           <- ifelse(na_to_zero, 0, NA)
  
  mat
}
