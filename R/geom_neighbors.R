#' Highlight neighbors on a chessboard
#'
#' @description
#' Highlights neighbors (cells) on a chessboard plotted with [gg_chessboard()].
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
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' neighbors <- pawn(nodes, focus = "2-3")
#' 
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, "2-3") +
#'   geom_neighbors(neighbors)

geom_neighbors <- function(neighbors) {
  
  
  ## Check argument ----
  
  check_neighbors_object(neighbors)
  
  
  ggplot2::geom_point(data  = neighbors, ggplot2::aes(.data$"transect", 
                                                      .data$"quadrat"), 
                      shape = 19, size = 2, color = "black")
}
