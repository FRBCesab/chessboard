#' Highlight a node on a chessboard
#'
#' @description
#' Highlights a node (cell) on a chessboard plotted with [gg_chessboard()].
#' 
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output if the function 
#'   [create_nodes_labels()].
#'
#' @param focus an `character` of length 1. The node label to be emphasized on
#'   the chessboard. Must exist in the `nodes` object.
#'
#' @return A list of two `geom_point` that must be added to a `ggplot2` object.
#'
#' @export
#'
#' @examples 
#' library("chessboard")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' gg_chessboard(nodes) +
#'   geom_node(nodes, "2-3")
#'
#' # One-dimensional sampling (only transects) ----
#' sites_infos <- data.frame("transect" = 1:5)
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect")
#' 
#' gg_chessboard(nodes) +
#'   geom_node(nodes, "3-1")

geom_node <- function(nodes, focus) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  
  ## Check argument 'focus' ----
  
  check_focus_object(nodes, focus)
  
  
  ## Convert nodes to factor ----
  
  nodes <- convert_nodes_to_factor(nodes)
  
  
  ## Select focus node ----
  
  nodes <- nodes[nodes$"node" == focus, ]
  
  
  ## ggplot2 features ----
  
  list(
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"), 
                        shape = 19, size = 4, color = "white"), 
    
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"), 
                        shape = 21, size = 4), 
    
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"),
                        shape = 19, size = 2, color = "#990000") 
  )
}
