#' Find neighbors according to knight movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the knight movement.
#' This movement is derived from the chess game. The knight is the difference
#' between the [wizard()] and the [queen()]. 
#' 
#' The detection of neighbors using this method can only work with 
#' two-dimensional sampling (both **transects** and **quadrats**). 
#' For sampling of type **transects-only** or **quadrats-only**, 
#' please use the functions [fool()] or [pawn()], respectively.
#' 
#' @inheritParams bishop_left
#' 
#' @inherit bishop_left return details
#' 
#' @export
#'
#' @examples
#' library("chessboard")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:9, "quadrat" = 1:9)
#' 
#' nodes <- create_node_labels(data     = sites_infos, 
#'                             transect = "transect", 
#'                             quadrat  = "quadrat")
#' 
#' focus     <- "5-5"
#' 
#' # Default settings ----
#' neighbors <- knight(nodes, focus, degree = 2)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- knight(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- knight(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- knight(nodes, focus, degree = 3, directed = TRUE, 
#'                     reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)

knight <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
                   self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The knight movement is not designed to work through transects ", 
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The knight movement is not designed to work through quadrats ",
         "only. Please use pawn() instead.", call. = FALSE)
  }
  
  
  ## Check argument 'focus' ----
  
  check_focus_object(nodes, focus)
  
  
  ## Check argument 'degree' ----
  
  check_degree_value(degree)
  
  
  ## Check logical ----
  
  check_logical_value(directed)
  check_logical_value(reverse)
  check_logical_value(self)
  
  
  ## Detect neighbors ----
  
  nb_queen  <- queen(nodes, focus, degree, directed, reverse, self)
  nb_wizard <- wizard(nodes, focus, degree, directed, reverse, self)
  
  neighbors <- nb_wizard[!(nb_wizard$"node" %in% nb_queen$"node"), ]
  
  
  ## Remove auto-neighborhood ----
  
  if (!self) neighbors <- neighbors[!(neighbors$"node" %in% focus), ]
  
  
  ## Clean output ----
  
  neighbors <- neighbors[!duplicated(neighbors$"node"), ]
  neighbors <- neighbors[order(neighbors$"node"), ]
  rownames(neighbors) <- NULL
  
  neighbors
}
