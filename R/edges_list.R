#' Find edges between nodes based on a degree of neighborhood
#' 
#' @description
#' Finds edges (links) between nodes (sites) based on a degree of neighborhood.
#' The nodes labels (argument `nodes`) will be ordered to find edges in a 
#' directional way (from upstream to downstream along a linear shape).
#' 
#' Note that the detection of edges is only based on the nodes labels 
#' (no explicit spatial detection). For instance, if nodes are labelled as 
#' **S-01**, **S-02**, ..., **S-10**, the most upstream site will be **S-01** 
#' and the most downstream **S-10**.
#' 
#' With a degree of neighborhood of 1, a node will be linked to the first 
#' previous node (upstream) and also to the first next node (downstream). So,
#' two edges will be detected.
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
#' @return A `data.frame` with three columns:
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#'   - `edge`: 0 (no edge) or 1 (edge)
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # List of edges with 1 degree of neighborhood ----
#' edges_list(adour_sites$"site")

edges_list <- function(nodes, degree = 1, self = FALSE, all = FALSE) {
  
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
  
  edges <- expand.grid(nodes, nodes)
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


  ## Remove autolinks ----
  
  if (!self) edges[which(edges$"from" == edges$"to"), "edge"] <- 0
  
  
  ## Select columns ----
  
  edges <- edges[ , -c(3:4)]
  
  
  ## Sort edges list ----
  
  edges <- edges[with(edges, order(from, to)), ]
  
  
  ## Remove non-edges ----
  
  if (!all) edges <- edges[edges$"edge" == 1, ]
  
  
  ## Clean row names ----
  
  rownames(edges) <- NULL
  
  edges
}
