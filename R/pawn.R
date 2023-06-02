#' Find neighbors according to pawn movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the fool movement.
#' This movement is orthogonal to the [fool()] function. The pawn can only move
#' vertically, i.e. through a **transect**. 
#' 
#' The detection of neighbors using the fool method can work with 
#' two-dimensional sampling (both **transects** and **quadrats**) and 
#' one-dimensional sampling of type **quadrats-only**. 
#' For sampling of type **transects-only**, please use the function [fool()].
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
#' neighbors <- pawn(nodes, focus)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- pawn(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- pawn(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- pawn(nodes, focus, degree = 3, directed = TRUE, reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_node(nodes, focus) +
#'   geom_neighbors(nodes, neighbors)

pawn <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
                 self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The pawn movement is not designed to work through transects ",
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  
  ## Check argument 'focus' ----
  
  check_focus_object(nodes, focus)
  
  
  ## Check argument 'degree' ----
  
  check_degree_value(degree)
  
  
  ## Check logical ----
  
  check_logical_value(directed)
  check_logical_value(reverse)
  check_logical_value(self)
  
  
  ## Convert nodes to factor ----
  
  nodes_user <- nodes
  
  nodes <- convert_nodes_to_factor(nodes)
  
  
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
    
  } else {
    
    if (!reverse) {
      
      nb_forward  <- nodes[which((nodes$"transect" == tr_focus) & 
                                   (nodes$"quadrat" %in% 
                                      c(qu_focus + degrees))), "node"]
      
      neighbors   <- c(neighbors, nb_forward)
      
    } else {
      
      nb_backward <- nodes[which((nodes$"transect" == tr_focus) & 
                                   (nodes$"quadrat" %in% 
                                      c(qu_focus - degrees))), "node"]
      
      neighbors   <- c(neighbors, nb_backward)
    }
  }
  
  
  ## Remove auto-neighborhood ----
  
  if (!self) neighbors <- neighbors[!(neighbors %in% focus)]
  
  
  ## Clean output ----
  
  neighbors <- nodes_user[nodes_user$"node" %in% unique(neighbors), ]
  neighbors <- neighbors[order(neighbors$"node"), ]
  rownames(neighbors) <- NULL
  
  neighbors
}
