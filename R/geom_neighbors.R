#' Highlight neighbors on a chessboard
#'
#' @description
#' Highlights neighbors (cells) on a chessboard plotted with [gg_chessboard()].
#'
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output if the function 
#'   [create_node_labels()].
#'
#' @param neighbors a `data.frame` with the following at least three columns: 
#'   `node`, `transect`, and `quadrats`. See [pawn()], [fool()], etc. for 
#'   further information.
#'
#' @return A `geom_point` that must be added to a `ggplot2` object.
#'
#' @export
#'
#' @examples 
#' library("chessboard")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_node_labels(data     = sites_infos, 
#'                             transect = "transect", 
#'                             quadrat  = "quadrat")
#' 
#' neighbors <- pawn(nodes, focus = "2-3")
#' 
#' gg_chessboard(nodes) +
#'   geom_node(nodes, "2-3") +
#'   geom_neighbors(nodes, neighbors)

geom_neighbors <- function(nodes, neighbors) {
  
  
  ## Check arguments ----
  
  check_nodes_object(nodes)
  check_neighbors_object(neighbors)
  
  
  ## Convert nodes to factor ----
  
  nodes <- convert_nodes_to_factor(nodes)
  
  neighbors <- nodes[nodes$"node" %in% neighbors$"node", ]
  
  
  ## ggplot2 feature ----
  
  ggplot2::geom_point(data  = neighbors, ggplot2::aes(.data$"transect", 
                                                      .data$"quadrat"), 
                      shape = 19, size = 2, color = "black")
}
