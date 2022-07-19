#' Find neighbors according to rook movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the rook movement.
#' This movement is derived from the chess game. The rook can move horizontally
#' and vertically. It's a combination of the [pawn()] and the [fool()] 
#' functions.
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
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' focus     <- "5-5"
#' 
#' # Default settings ----
#' neighbors <- rook(nodes, focus)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- rook(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- rook(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- rook(nodes, focus, degree = 3, directed = TRUE, reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)

rook <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
                 self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The rook movement is not designed to work through transects ",
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The rook movement is not designed to work through quadrats ",
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
    
    nb_forward  <- nodes[which((nodes$"transect" == tr_focus) & 
                                 (nodes$"quadrat" %in% 
                                    c(qu_focus + degrees))), "node"]
    
    neighbors   <- c(neighbors, nb_forward)
    
    
    nb_backward <- nodes[which((nodes$"transect" == tr_focus) & 
                                 (nodes$"quadrat" %in% 
                                    c(qu_focus - degrees))), "node"]
    
    neighbors   <- c(neighbors, nb_backward)
    
    nb_right  <- nodes[which((nodes$"transect" %in% c(tr_focus + degrees)) & 
                               (nodes$"quadrat" == qu_focus)), "node"]
    
    neighbors <- c(neighbors, nb_right)
    
    
    nb_left   <- nodes[which((nodes$"transect" %in% c(tr_focus - degrees)) & 
                               (nodes$"quadrat" == qu_focus)), "node"]
    
    neighbors <- c(neighbors, nb_left)
    
  } else {
    
    if (!reverse) {
      
      nb_forward  <- nodes[which((nodes$"transect" == tr_focus) & 
                                   (nodes$"quadrat" %in% 
                                      c(qu_focus + degrees))), "node"]
      
      neighbors   <- c(neighbors, nb_forward)
      
      nb_right  <- nodes[which((nodes$"transect" %in% c(tr_focus + degrees)) & 
                                 (nodes$"quadrat" == qu_focus)), "node"]
      
      neighbors <- c(neighbors, nb_right)
      
      
      nb_left   <- nodes[which((nodes$"transect" %in% c(tr_focus - degrees)) & 
                                 (nodes$"quadrat" == qu_focus)), "node"]
      
      neighbors <- c(neighbors, nb_left)
      
    } else {
      
      nb_backward <- nodes[which((nodes$"transect" == tr_focus) & 
                                   (nodes$"quadrat" %in% 
                                      c(qu_focus - degrees))), "node"]
      
      neighbors   <- c(neighbors, nb_backward)
      
      nb_right  <- nodes[which((nodes$"transect" %in% c(tr_focus + degrees)) & 
                                 (nodes$"quadrat" == qu_focus)), "node"]
      
      neighbors <- c(neighbors, nb_right)
      
      
      nb_left   <- nodes[which((nodes$"transect" %in% c(tr_focus - degrees)) & 
                                 (nodes$"quadrat" == qu_focus)), "node"]
      
      neighbors <- c(neighbors, nb_left)
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
