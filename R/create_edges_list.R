#' Create an edge list
#' 
#' @description 
#' Creates a list of edges (links) between nodes (sampling units) based on the
#' detection of neighbors and according to three neighborhood rules:
#' 
#' 1. **Degree of neighborhood** (argument `degree`): the number of adjacent 
#' nodes that will be used to create **direct** edges. If `degree = 1`, 
#' only nodes directly adjacent to the focal node will be considered as 
#' neighbors.
#' 2. **Orientation of neighborhood** (argument `method`): can neighbors be 
#' detecting horizontally and/or vertically and/or diagonally? The package 
#' `chessboard` implements all possible orientations derived from the chess 
#' game.
#' 3. **Direction of neighborhood** (arguments `directed` and `reverse`): does 
#' the sampling design has a direction? If so (`directed = TRUE`), the network 
#' will be considered as **directed** and the direction will follow the order 
#' of nodes labels in both axes (except if `reverse = TRUE`).
#' 
#' It's important to note that, even the package `chessboard` is designed to 
#' deal with spatial networks, this function does not explicitly use spatial 
#' coordinates to detect neighbors. Instead it uses the **node labels**. The 
#' function [create_node_labels()] must be used before this function to create 
#' node labels.
#' 
#' @param nodes a `data.frame` with (at least) the following three columns: 
#'   `node`, `transect`, and `quadrats`. Must be the output of the function 
#'   [create_node_labels()].
#' 
#' @param method a `character` of length 1. The method used to detect neighbors.
#'   One among `'pawn'`, `'fool'`, `'rook'`, `'bishop'`, `'bishop_left'`, 
#'   `'bishop_right'`, `'knight'`, `'knight_left'`, `'knight_right'`, 
#'   `'queen'`, `'wizard'`. For further information, see the functions of the 
#'   same name (i.e. [pawn()], [rook()], etc.).
#' 
#' @param degree an `integer` of length 1. The maximum number of neighbors to 
#'   search for.
#'
#' @param directed a `logical` of length 1. If `FALSE` (default), search for 
#'   neighbors in all directions (undirected network). Otherwise, the network 
#'   will be considered as directed according to the orientations of the 
#'   network. The default orientation follows the order of nodes labels in
#'   both axes.
#'
#' @param reverse a `logical` of length 1. If `TRUE`, change the default 
#'   orientation of the network. This argument is ignored if `directed = FALSE`.
#'   See examples for further detail.
#'   
#' @param self a `logical` of length 1. If `TRUE`, a node can be its own 
#'   neighbor. Default is `FALSE`.
#'   
#' @return A `data.frame` with `n` rows (where `n` is the number of edges) and
#' the following two columns:
#' - `from`: the node label of one of the two endpoints of the edge
#' - `to`: the node label of the other endpoint of the edge
#'
#' @export
#' 
#' @examples
#' library("chessboard")
#' 
#' # Two-dimensional sampling (only) ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_node_labels(data     = sites_infos, 
#'                             transect = "transect", 
#'                             quadrat  = "quadrat")
#'
#' edges <- create_edges_list(nodes, method = "pawn", directed = TRUE)
#' edges
#' 
#' edges <- create_edges_list(nodes, method = "bishop", directed = TRUE)
#' edges

create_edges_list <- function(nodes, method, degree = 1, directed = FALSE, 
                              reverse = FALSE, self = FALSE) {
  
  ## Check arguments ----
  
  check_nodes_object(nodes)
  check_neighbors_method(method)
  check_degree_value(degree)
  check_logical_value(directed)
  check_logical_value(reverse)
  check_logical_value(self)
  
  
  ## Catch detection function ----
  
  detection_method <- eval(parse(text = method))
  
  
  ## Get nodes list ----
  
  nodes_list <- get_nodes_list(nodes)
  
  
  ## Detect neighbors ----
  
  neighbors <- do.call(rbind.data.frame, lapply(nodes_list, function(x) {
    
    nb <- detection_method(nodes, x, degree, directed, reverse, self)
    
    if (nrow(nb) > 0) {
      nb <- data.frame("from" = x, "to" = nb[ , 1])
    } else {
      nb <- data.frame("from" = character(0), "to" = character(0))
    }
    
    nb
  }))
  
  
  ## Clean output ----
  
  if (nrow(neighbors) > 0) {
    
    neighbors <- neighbors[which(!duplicated(paste(neighbors$"from", 
                                                   neighbors$"to"))), ]
    neighbors <- sort_edges(neighbors)
  }
  
  neighbors
}
