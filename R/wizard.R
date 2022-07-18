#' Find neighbors according to wizard movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the wizard movement.
#' This movement is a combination of the [queen()] and the [knight()] pieces 
#' from the chess game. The wizard can move in all directions and in both 
#' orientations (default behavior). 
#' 
#' **Important:** Use the function [create_nodes_labels()] to create nodes 
#' labels.
#' 
#' The detection of neighbors using the bishop right method can only work with 
#' two-dimensional sampling (both **transects** and **quadrats**). 
#' For sampling of type **transects-only** or **quadrats-only**, please use the
#' functions [fool()] or [pawn()], respectively.
#' 
#' The argument `degree` controls for the degree of neighborhood.
#' If `degree = 2`, 24 neighbors will be identified (except if 
#' `directed = TRUE`).
#'  
#' If `directed = TRUE` and `reverse = FALSE`, only 14 nodes will be
#' considered as neighbors (remove the nodes at the bottom of the focus node).
#' 
#' If `directed = TRUE` and `reverse = TRUE`, only 14 nodes will be
#' considered as neighbors (remove the nodes at the top of the focus node).
#' 
#' @inheritParams bishop_left
#' 
#' @inherit bishop_left return details
#' 
#' @export
#'
#' @examples
#' library("bridge")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:9, "quadrat" = 1:9)
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' focus     <- "5-5"
#' 
#' # Default settings ----
#' neighbors <- wizard(nodes, focus)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- wizard(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- wizard(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- wizard(nodes, focus, degree = 3, directed = TRUE, 
#'                     reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)

wizard <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
                   self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The wizard movement is not designed to work through transects ",
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The wizard movement is not designed to work through quadrats ",
         "only. Please use pawn() instead.", call. = FALSE)
  }
  
  
  ## Check argument 'focus' ----
  
  check_focus_object(nodes, focus)
  
  
  ## Check argument 'degree' ----
  
  check_degree_value(degree)
  
  
  ## Get focus information ----
  
  tr_focus <- nodes[which(nodes$"node" == focus), "transect"]
  qu_focus <- nodes[which(nodes$"node" == focus), "quadrat"]
  
  
  ## Detect neighbors ----
  
  degrees   <- 0:degree
  neighbors <- NULL
  
  
  if (!directed) {
    
    nb_top_right <- nodes[which((nodes$"transect" %in% 
                                   c(tr_focus + degrees)) & 
                                  (nodes$"quadrat" %in% 
                                     c(qu_focus + degrees))), "node"]
    
    neighbors    <- c(neighbors, nb_top_right)
    
    
    nb_top_left  <- nodes[which((nodes$"transect" %in% 
                                   c(tr_focus - degrees)) & 
                                  (nodes$"quadrat" %in% 
                                     c(qu_focus + degrees))), "node"]
    
    neighbors    <- c(neighbors, nb_top_left)
    
    nb_bot_right <- nodes[which((nodes$"transect" %in% 
                                   c(tr_focus + degrees)) & 
                                  (nodes$"quadrat" %in% 
                                     c(qu_focus - degrees))), "node"]
    
    neighbors    <- c(neighbors, nb_bot_right)
    
    
    nb_bot_left  <- nodes[which((nodes$"transect" %in% 
                                   c(tr_focus - degrees)) & 
                                  (nodes$"quadrat" %in% 
                                     c(qu_focus - degrees))), "node"]
    
    neighbors    <- c(neighbors, nb_bot_left)
    
  } else {
    
    if (!reverse) {
      
      nb_top_right <- nodes[which((nodes$"transect" %in% 
                                     c(tr_focus + degrees)) & 
                                    (nodes$"quadrat" %in% 
                                       c(qu_focus + degrees))), "node"]
      
      neighbors    <- c(neighbors, nb_top_right)
      
      
      nb_top_left  <- nodes[which((nodes$"transect" %in% 
                                     c(tr_focus - degrees)) & 
                                    (nodes$"quadrat" %in% 
                                       c(qu_focus + degrees))), "node"]
      
      neighbors    <- c(neighbors, nb_top_left)
      
    } else {
      
      nb_bot_right <- nodes[which((nodes$"transect" %in% 
                                     c(tr_focus + degrees)) & 
                                    (nodes$"quadrat" %in% 
                                       c(qu_focus - degrees))), "node"]
      
      neighbors    <- c(neighbors, nb_bot_right)
      
      
      nb_bot_left  <- nodes[which((nodes$"transect" %in% 
                                     c(tr_focus - degrees)) & 
                                    (nodes$"quadrat" %in% 
                                       c(qu_focus - degrees))), "node"]
      
      neighbors    <- c(neighbors, nb_bot_left)
    }
  }
  
  
  ## Remove auto-neighborhood ----
  
  if (!self) neighbors <- neighbors[!(neighbors %in% focus)]
  
  
  ## Clean output ----
  
  neighbors <- nodes[nodes$"node" %in% unique(neighbors), ]
  neighbors <- neighbors[order(neighbors$"node"), ]
  rownames(neighbors) <- NULL
  
  neighbors
}
