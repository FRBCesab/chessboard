#' Find neighbors according to bishop movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the bishop movement.
#' This movement is derived from the chess game. The bishop can move along the 
#' two diagonals. 
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
#' neighbors <- bishop(nodes, focus)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- bishop(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- bishop(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- bishop(nodes, focus, degree = 3, directed = TRUE, 
#'                     reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)

bishop <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
                   self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The bishop movement is not designed to work through transects ",
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The bishop movement is not designed to work through quadrats ",
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
  
  
  ## Get focus information ----
  
  tr_focus <- nodes[which(nodes$"node" == focus), "transect"]
  qu_focus <- nodes[which(nodes$"node" == focus), "quadrat"]
  
  
  ## Detect neighbors ----
  
  degrees   <- 0:degree
  neighbors <- NULL
  
  
  if (!directed) {
    
    for (i in degrees) {
      
      nb_top_right <- nodes[which((nodes$"transect" == tr_focus + i) & 
                                    (nodes$"quadrat" == qu_focus + i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_top_right)
      
      
      nb_top_left  <- nodes[which((nodes$"transect" == tr_focus - i) & 
                                    (nodes$"quadrat" == qu_focus + i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_top_left)
      
      nb_bot_right <- nodes[which((nodes$"transect" == tr_focus + i) & 
                                    (nodes$"quadrat" == qu_focus - i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_bot_right)
      
      
      nb_bot_left  <- nodes[which((nodes$"transect" == tr_focus - i) & 
                                    (nodes$"quadrat" == qu_focus - i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_bot_left)
    }
    
  } else {
    
    if (!reverse) {
      
      for (i in degrees) {
      
        nb_top_right <- nodes[which((nodes$"transect" == tr_focus + i) & 
                                      (nodes$"quadrat" == qu_focus + i)), 
                              "node"]
        
        neighbors    <- c(neighbors, nb_top_right)
        
        
        nb_top_left  <- nodes[which((nodes$"transect" == tr_focus - i) & 
                                      (nodes$"quadrat" == qu_focus + i)), 
                              "node"]
        
        neighbors    <- c(neighbors, nb_top_left)
      }
      
    } else {
      
      for (i in degrees) {
        
        nb_bot_right <- nodes[which((nodes$"transect" == tr_focus + i) & 
                                      (nodes$"quadrat" == qu_focus - i)), 
                              "node"]
        
        neighbors    <- c(neighbors, nb_bot_right)
        
        
        nb_bot_left  <- nodes[which((nodes$"transect" == tr_focus - i) & 
                                      (nodes$"quadrat" == qu_focus - i)), 
                              "node"]
        
        neighbors    <- c(neighbors, nb_bot_left)
      }
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
