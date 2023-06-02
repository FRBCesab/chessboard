#' Link neighbors by arrow on a chessboard
#'
#' @description
#' Links neighbors (cells) on a chessboard plotted with [gg_chessboard()].
#'
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output if the function 
#'   [create_nodes_labels()].
#'
#' @param focus an `character` of length 1. The node label to be emphasized on
#'   the chessboard. Must exist in the `nodes` object.
#'
#' @param neighbors a `data.frame` with the following at least three columns: 
#'   `node`, `transect`, and `quadrats`. See [pawn()], [fool()], etc. for 
#'   further information.
#'
#' @return A `geom_segment` that must be added to a `ggplot2` object.
#'
#' @export
#'
#' @examples 
#' library("chessboard")
#' 
#' sites_infos <- expand.grid("transect" = 1:9, "quadrat" = 1:9)
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' focus <- "5-5"
#' 
#' neighbors <- wizard(nodes, focus = focus, degree = 4, directed = FALSE, 
#'                     reverse = TRUE)
#' 
#' gg_chessboard(nodes) +
#'   geom_neighbors(nodes, neighbors) +
#'   geom_edges(nodes, focus, neighbors) +
#'   geom_node(nodes, focus)

geom_edges <- function(nodes, focus, neighbors) {
  
  ## Check arguments ----
  
  check_nodes_object(nodes)
  check_focus_object(nodes, focus)
  check_neighbors_object(neighbors)
  
  
  ## Convert nodes to factor ----
  
  nodes <- convert_nodes_to_factor(nodes)
  
  neighbors <- nodes[nodes$"node" %in% neighbors$"node", ]
  focus     <- nodes[nodes$"node" %in% focus, ]
  
  
  ## ggplot2 feature ----
  
  ggplot2::geom_segment(data  = neighbors, 
    ggplot2::aes(x = focus$"transect", xend = .data$"transect", 
                 y = focus$"quadrat",  yend = .data$"quadrat"), 
    arrow = ggplot2::arrow(length = ggplot2::unit(0.25, 'cm'), type = 'closed'),
    linewidth = 0.25, color = "black") 
}
