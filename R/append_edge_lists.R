#' Append several edge lists
#' 
#' @description 
#' Appends several edge lists created by [create_edge_list()]. Merged edges 
#' will be ordered and duplicates will be removed.
#' 
#' @param ... one or several edge lists `data.frame`. Outputs of the function
#'   [create_edge_list()].
#' 
#' @return A `data.frame` with `n` rows (where `n` is the total number of edges)
#' and the following two columns:
#' - `from`: the node label of one of the two endpoints of the edge
#' - `to`: the node label of the other endpoint of the edge
#' 
#' @export
#' 
#' @examples
#' library("chessboard")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_node_labels(data     = sites_infos, 
#'                             transect = "transect", 
#'                             quadrat  = "quadrat")
#'
#' edges_1 <- create_edge_list(nodes, method = "pawn", directed = TRUE)
#' edges_2 <- create_edge_list(nodes, method = "bishop", directed = TRUE)
#' 
#' edges <- append_edge_lists(edges_1, edges_2)

append_edge_lists <- function(...) {
  
  ## Catch arguments ----
  
  edges <- list(...)
  

  ## Check edge lists ----

  if (length(edges) == 0) {
   stop("Please provide at least one edge list", call. = FALSE) 
  }
  
  lapply(edges, check_edges_object)
  
  
  ## Append edge lists ----
  
  edges <- do.call(rbind.data.frame, edges)
  
  
  ## Clean output ----
  
  edges <- edges[which(!duplicated(paste(edges$"from", edges$"to"))), ]
  
  sort_edges(edges)
}
