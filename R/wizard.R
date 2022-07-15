#' Find neighbors according to wizard movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the wizard movement.
#' This movement is a combination of the [queen()] and the [knight()] pieces 
#' from the chess game. The wizard can move in all directions and in both 
#' orientations (default behavior). 
#' 
#' 
#' **Important:** Use the function [create_nodes_labels()] to create nodes 
#' labels.
#' 
#' 
#' The detection of neighbors using the wizard method can only work with 
#' two-dimensional sampling (both **transects** and **quadrats**) and 
#' one-dimensional sampling of type **quadrats**. For sampling of type 
#' **transects** or **quadrats**, please use the functions [fool()] or 
#' [pawn()], respectively.
#' 
#' The identification of neighbors is only based on the nodes labels (no 
#' explicit spatial detection). This means that labeling nodes is a 
#' **crucial step**.
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
#' 
#' @param nodes a `data.frame` with the following three columns: 
#'   `node`, `transect`, and `quadrats`. See [create_nodes_labels()] for 
#'   further information.
#' 
#' @param focus an `character` of length 1. The node label for which the 
#'   neighbors must be found. Must exist in the `nodes` object.
#' 
#' @param degree an `integer` of length 1. The maximum number of neighbors to 
#'   search for in one direction.
#'
#' @param directed a `logical` of length 1. If `FALSE` (default), search for 
#'   neighbors in all directions (undirected network). Otherwise, the network 
#'   will be considered as directed according the main direction of the system
#'   (i.e. through quadrats).
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
#' 
#' @note 
#' This function is called by [edges_list()] (if `type = "rook"`). It can be 
#' directly used only to 1) understand the neighbors detection method, 2) to 
#' check, and 3) visualize detected neighbors for one particular node.
#' 
#' 
#' @export
#'
#' @examples
#' library("bridge")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:9, "quadrat" = 1:9)
#' 
#' nodes <- create_nodes_labels(transects = sites_infos$"transect", 
#'                              quadrats  = sites_infos$"quadrat")
#' 
#' focus     <- "5-5"
#' 
#' # Default settings ----
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
  
  if (missing(nodes)) {
    stop("Argument 'nodes' is required ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!is.data.frame(nodes)) {
    stop("Argument 'nodes' must be a data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!("node" %in% colnames(nodes))) {
    stop("The column 'node' is absent from the 'nodes' data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!("transect" %in% colnames(nodes))) {
    stop("The column 'transect' is absent from the 'nodes' data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!("quadrat" %in% colnames(nodes))) {
    stop("The column 'quadrat' is absent from the 'nodes' data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (nrow(nodes) == 0) {
    stop("Argument 'nodes' must have at least two rows (nodes)", call. = FALSE)
  }
  
  if (!is.numeric(nodes$"transect")) {
    stop("The column 'transect' of the 'nodes' data.frame must be a numeric", 
         call. = FALSE)
  }
  
  if (!is.numeric(nodes$"quadrat")) {
    stop("The column 'quadrat' of the 'nodes' data.frame must be a numeric", 
         call. = FALSE)
  }
  
  if (!is.character(nodes$"node")) {
    stop("The column 'node' of the 'nodes' data.frame must be a character", 
         call. = FALSE)
  }
  
  if (length(unique(nodes$"quadrat")) == 1) {
    stop("The wizard movement is not designed to work through quadrats only. ",
         "Please use fool() instead.", call. = FALSE)
  }
  
  if (length(unique(nodes$"transect")) == 1) {
    stop("The wizard movement is not designed to work through transects only. ",
         "Please use pawn() instead.", call. = FALSE)
  }
  
  
  ## Check argument 'focus' ----
  
  if (missing(focus)) {
    stop("Argument 'focus' is required (node label)", call. = FALSE)
  }
  
  if (!is.character(focus)) {
    stop("Argument 'focus' must be a character (node label)", call. = FALSE)
  }
  
  if (length(focus) != 1) {
    stop("Argument 'focus' must be a character of length 1 (node label)", 
         call. = FALSE)
  }
  
  if (!(focus %in% nodes$"node")) {
    stop(paste0("The node '", focus, "' is absent from the nodes list ", 
                "(argument 'nodes')"), call. = FALSE)
  }
  
  
  ## Check argument 'degree' ----
  
  if (!is.numeric(degree)) {
    stop("Argument 'degree' must be a numeric", call. = FALSE)
  }
  
  if (length(degree) != 1) {
    stop("Argument 'degree' must be a numeric of length 1", call. = FALSE)
  }
  
  if (degree <= 0) {
    stop("Argument 'degree' must be strictly positive", call. = FALSE)
  }
  
  
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