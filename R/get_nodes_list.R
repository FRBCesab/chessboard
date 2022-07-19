#' Get the list of nodes
#'
#' @description 
#' Retrieves the nodes list by selecting and ordering the column `node` of the 
#' output of the function [create_nodes_labels()].
#' 
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output of the function 
#'   [create_nodes_labels()].
#' 
#' @return A vector of nodes labels.
#' 
#' @export
#' 
#' @examples 
#' library("bridge")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' get_nodes_list(nodes)

get_nodes_list <- function(nodes) {
  
  ## Check argument ----
  
  check_nodes_object(nodes)
  
  
  ## Extract nodes labels ----
  
  sort(as.character(nodes$"node"))
}
