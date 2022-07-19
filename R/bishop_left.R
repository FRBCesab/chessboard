#' Find neighbors according to bishop left movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the bishop left movement.
#' This movement is derived from the [bishop()] method. The bishop left 
#' can move along the bottom-right to top-left diagonal in both orientations
#' (default behavior). 
#' 
#' **Important:** Use the function [create_nodes_labels()] to create nodes 
#' labels.
#' 
#' The detection of neighbors using the bishop left method can only work with 
#' two-dimensional sampling (both **transects** and **quadrats**). 
#' For sampling of type **transects-only** or **quadrats-only**, please use the 
#' functions [fool()] or [pawn()], respectively.
#' 
#' The argument `degree` controls for the degree of neighborhood.
#' If `degree = 2`, four neighbors will be identified (except if 
#' `directed = TRUE`): four nodes along the bottom-right to top-left diagonal.
#'  
#' If `directed = TRUE` and `reverse = FALSE`, only two nodes will be
#' considered as neighbors (remove the nodes at the bottom of the focus node).
#' 
#' If `directed = TRUE` and `reverse = TRUE`, only two nodes will be
#' considered as neighbors (remove the nodes at the top of the focus node).
#' 
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output if the function 
#'   [create_nodes_labels()].
#' 
#' @param focus an `character` of length 1. The node label for which the 
#'   neighbors must be found. Must exist in the `nodes` object.
#' 
#' @param degree an `integer` of length 1. The maximum number of neighbors to 
#'   search for in one direction.
#'
#' @param directed a `logical` of length 1. If `FALSE` (default), search for 
#'   neighbors in all directions (undirected network). Otherwise, the network 
#'   will be considered as directed according to the main direction of the 
#'   network (i.e. through quadrats).
#'
#' @param reverse a `logical` of length 1. If `TRUE`, change the orientation of
#'   the network (i.e. through quadrats). This argument is ignored if 
#'   `directed = FALSE`.
#'   
#' @param self a `logical` of length 1. If `TRUE`, a node can be its own 
#'   neighbor. Default is `FALSE`.
#' 
#' @return A subset of the `nodes` (`data.frame`) where each row is a neighbor
#'   of `focus`.
#' 
#' @details 
#' This function is internally called by [find_neighbors()] but it can be
#' directly used to 1) understand the neighbors detection method, and 2) to 
#' check detected neighbors for one particular node (`focus`).
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
#' neighbors <- bishop_left(nodes, focus)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'
#' # Higher degree of neighborhood ----
#' neighbors <- bishop_left(nodes, focus, degree = 3)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (default orientation) ----
#' neighbors <- bishop_left(nodes, focus, degree = 3, directed = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # Directed (reverse orientation) ----
#' neighbors <- bishop_left(nodes, focus, degree = 3, directed = TRUE, 
#'                          reverse = TRUE)
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)

bishop_left <- function(nodes, focus, degree = 1, directed = FALSE, 
                        reverse = FALSE, self = FALSE) {
  
  
  ## Check argument 'nodes' ----
  
  check_nodes_object(nodes)
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The bishop left movement is not designed to work through transects ", 
         "only. Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The bishop left movement is not designed to work through quadrats ",
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
      
      nb_top_left  <- nodes[which((nodes$"transect" == tr_focus - i) & 
                                    (nodes$"quadrat" == qu_focus + i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_top_left)
      
      nb_bot_right <- nodes[which((nodes$"transect" == tr_focus + i) & 
                                    (nodes$"quadrat" == qu_focus - i)), 
                            "node"]
      
      neighbors    <- c(neighbors, nb_bot_right)
    }
    
  } else {
    
    if (!reverse) {
      
      for (i in degrees) {
        
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
