#' Retrieve and order nodes from an edges list
#' 
#' Creates labels for edges (non edges are ignored). If two edges are identical
#' (e.g. `"S01-S02"` and `"S02-S01"`), there will have the same label 
#' (`"E-01"`).
#' 
#' @param edges a `data.frame` with at least two columns:
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#' 
#' @return A `data.frame` with column, `node` containing unique ordered nodes 
#'   labels. 
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Find edges with 1 degree of neighborhood (undirected network) ----
#' edges <- edges_list(adour_sites$"site")
#' 
#' # Retrieve nodes ----
#' nodes_list(edges)

nodes_list <- function(edges) {
  
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
  
  
  ## Get nodes ----
  
  nodes <- c(edges$"from", edges$"to")
  
  data.frame("node" = sort(unique(as.character(nodes))))
}
