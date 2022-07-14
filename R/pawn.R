#' Find neighbors according to pawn movement
#' 
#' @description
#' For one node (argument `focus`), finds neighbors among a list of nodes 
#' according to the pawn movement.
#' This movement is derived from the chess game. The pawn can move along one 
#' direction, i.e. through a **transect**, both backward and forward (default 
#' behavior). 
#' 
#' 
#' **Important:** Use the function [create_nodes_labels()] to create nodes 
#' labels.
#' 
#' 
#' The detection of neighbors using the pawn method can work with 
#' two-dimensional sampling (both **transects** and **quadrats**) and 
#' one-dimensional sampling of type **quadrats**. For sampling of type 
#' **transects**, please use the function [fool()].
#' 
#' The identification of neighbors is only based on the nodes labels (no 
#' explicit spatial detection). This means that labeling nodes is a 
#' **crucial step**.
#' 
#' The argument `degree` controls for the degree of neighborhood.
#' If `degree = 2`, four neighbors will be identified (except if 
#' `directed = TRUE`): the two previous nodes **and** the two next nodes.
#'  
#' If `directed = TRUE` and `reverse = FALSE`, only the two next nodes will be
#' considered as neighbors (directed network with default orientation).
#' 
#' If `directed = TRUE` and `reverse = TRUE`, only the two previous nodes will 
#' be considered as neighbors (directed network with reverse orientation).
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
#'   neighbors in both directions, i.e. before and after the focus node 
#'   (undirected network). Otherwise (directed network) search for neighbors
#'   after the focus node (`reverse = FALSE`) or before (`reverse = TRUE`).
#'
#' @param reverse a `logical` of length 1. If `TRUE`, change the orientation of
#'   the network. This argument is ignored if `directed = FALSE`.
#'   
#' @param self a `logical` of length 1. If `TRUE`, a node can be its own 
#'   neighbor. Default is `FALSE`.
#' 
#' @return A subset of the `nodes` (`data.frame`) where each row is a neighbor
#'   of `focus`.
#' 
#' 
#' @note 
#' This function is called by [edges_list()] (if `type = "pawn"`). It can be 
#' directly used only to 1) understand the neighbors detection method, 2) to 
#' check, and 3) visualize detected neighbors for one particular node.
#' 
#' 
#' @export
#'
#' @examples
#' library("bridge")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(transects = sites_infos$"transect", 
#'                              quadrats  = sites_infos$"quadrat")
#'
#' pawn(nodes, focus = "2-3", degree = 2)
#' pawn(nodes, focus = "2-3", degree = 2, self = TRUE)
#' pawn(nodes, focus = "2-3", degree = 2, directed = TRUE)
#' pawn(nodes, focus = "2-3", degree = 2, directed = TRUE, reverse = TRUE)
#' 
#' # Visualization ----
#' focus     <- "2-3"
#' neighbors <- pawn(nodes, focus = focus, degree = 1)
#' 
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)
#'   
#' # One-dimensional sampling (quadrats) ----
#' sites_infos <- 1:5
#' nodes <- create_nodes_labels(quadrats = sites_infos)
#' 
#' # Visualization ----
#' focus     <- "1-2"
#' neighbors <- pawn(nodes, focus = focus, degree = 2, directed = TRUE)
#' 
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, focus) +
#'   geom_neighbors(neighbors)

pawn <- function(nodes, focus, degree = 1, directed = FALSE, reverse = FALSE, 
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
    stop("The pawn movement is not designed to work through quadrats. ",
         "Please use fool() instead.", call. = FALSE)
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
  
  neighbors <- nodes[nodes$"node" %in% unique(neighbors), ]
  neighbors <- neighbors[order(neighbors$"node"), ]
  rownames(neighbors) <- NULL
  
  neighbors
}
