#' Find edges between nodes based on a degree of neighborhood along a linear 
#' shape
#' 
#' @description
#' Finds edges (links) between nodes (sites) based on a degree of neighborhood
#' and along a linear shape (e.g. river or road).
#' The nodes labels (argument `nodes`) will be ordered to find edges in a 
#' directional way (from upstream to downstream along a linear shape).
#' 
#' Note that the detection of edges is only based on the nodes labels 
#' (no explicit spatial detection). For instance, if nodes are labelled as 
#' **S-01**, **S-02**, ..., **S-10**, the most upstream site will be **S-01** 
#' and the most downstream **S-10**.
#' 
#' With a degree of neighborhood of 1, a node will be linked to the first 
#' previous node and also to the first next node (undirected network). So,
#' the same edges will be detected twice. If `directed = FALSE`, this node will 
#' be only linked to first next node (directed network from upstream to 
#' downstream).
#' 
#' @param nodes a `character` vector of nodes (sites) labels.
#' 
#' @param degree an `integer` of length 1. The number of neighbors used to 
#'   define edges.
#'
#' @param self a `logical` of length 1. If `TRUE`, a node can be linked to 
#'   itself. Default is `FALSE`.
#'
#' @param all a `logical` of length 1. If `TRUE`, the missing edges are also
#'   returned. Default is `FALSE`.
#'
#' @param directed a `logical` of length 1. If `FALSE` (default), symmetrical 
#'   edges (e.g. S01-S02 and S02-S01) are returned (undirected network). 
#'   Otherwise (directed network) only the first edge (e.g. S01-S02) is 
#'   returned (according to direction of the nodes labels).
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
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' adour_sites
#' 
#' # Find edges with 1 degree of neighborhood (undirected network) ----
#' edges_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood (directed network) ----
#' edges_list(adour_sites$"site", directed = TRUE)

edges_list <- function(nodes, degree = 1, self = FALSE, all = FALSE, 
                       directed = FALSE) {
  
  ## Check 'nodes' argument ----
  
  if (missing(nodes)) {
    stop("Argument 'nodes' is required", call. = FALSE)
  }
  
  if (!is.character(nodes)) {
    stop("Argument 'nodes' must be a character vector (sites labels)", 
         call. = FALSE)
  }
  
  
  ## Check 'degree' argument ----
  
  if (!is.numeric(degree) || length(degree) != 1) {
    stop("Argument 'degree' must be an integer of length 1", call. = FALSE)
  }
  
  if (degree > (length(nodes) - 1)) {
    stop("Argument 'degree' must be 'strictly < length(nodes)'")
  }
  
  
  ## Order nodes labels ----
  
  nodes <- sort(nodes)
  
  
  ## Create all possible edges ----
  
  edges <- expand.grid(nodes, nodes, stringsAsFactors = FALSE)
  colnames(edges) <- c("from", "to")
  
  edges <- edges[with(edges, order(from, to)), ]
  
  
  ## Create a sequence of nodes (order matters) ----
  
  nodes <- data.frame("node"      = nodes, 
                      "nodes_int" = seq_len(length(nodes)))
  
  
  ## Add information on nodes in edges list ----
  
  edges <- merge(edges, nodes, by.x = "from", by.y = "node")
  colnames(edges)[ncol(edges)] <- "from_int"
  
  edges <- merge(edges, nodes, by.x = "to", by.y = "node")
  colnames(edges)[ncol(edges)] <- "to_int"
  
  
  ## Sort edges list ----
  
  edges <- edges[with(edges, order(from, to)), c(2, 1, 3, 4)]
  
  
  ## Detect links ----
  
  edges$"edge" <- 0
  edges[which(abs(edges$"from_int" - edges$"to_int") <= degree), "edge"] <- 1
  
  
  ## Remove symmetrical edges (directed network) ----
  
  if (directed) edges[which(edges$"from_int" > edges$"to_int"), "edge"] <- 0
  

  ## Remove auto-links ----
  
  if (!self) edges[which(edges$"from" == edges$"to"), "edge"] <- 0
  
  
  ## Select columns ----
  
  edges <- edges[ , -c(3:4)]
  
  
  ## Sort edges list ----
  
  edges <- edges[with(edges, order(from, to)), ]
  
  
  ## Remove non-edges ----
  
  if (!all) edges <- edges[edges$"edge" == 1, ]
 
  
  ## Create edge ids ----
  
  create_edges_id(edges)
}
