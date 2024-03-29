#' Get the list of nodes
#'
#' @description 
#' Retrieves the node list by selecting and ordering the column `node` of the 
#' output of the function [create_node_labels()].
#' 
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output of the function 
#'   [create_node_labels()].
#' 
#' @return A vector of node labels.
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
#' get_node_list(nodes)

get_node_list <- function(nodes) {
  
  ## Check argument ----
  
  check_nodes_object(nodes)
  
  
  ## Extract node labels ----
  
  nodes <- nodes[with(nodes, order(location, transect, quadrat)), ]
  
  as.character(nodes$"node")
}
