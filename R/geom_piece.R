#' Highlight a node on a chessboard
#'
#' @description
#' Highlights a node (cell) on a chessboard plotted with [gg_chessboard()].
#'
#' @param nodes a `data.frame` with the following three columns: 
#'   `node`, `transect`, and `quadrats`. See [create_nodes_labels()] for 
#'   further information.
#'
#' @param focus an `character` of length 1. The node label to be emphasized on
#'   the chessboard. Must exist in the `nodes` object.
#'
#' @return A list of two `geom_point` that must be added to a `ggplot2` object.
#'
#' @export
#'
#' @examples 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(transects = sites_infos$"transect", 
#'                              quadrats  = sites_infos$"quadrat")
#' 
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, "2-3")
#'
#' # One-dimensional sampling (only transects) ----
#' sites_infos <- 1:5
#' 
#' nodes <- create_nodes_labels(transects = sites_infos)
#' 
#' gg_chessboard(nodes) +
#'   geom_piece(nodes, "3-1")

geom_piece <- function(nodes, focus) {
  
  
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
  
  
  nodes <- nodes[nodes$"node" == focus, ]
  
  list(
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"), 
                        shape = 21, size = 4), 
    
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"),
                        shape = 19, size = 2, color = "#990000") 
  )
}
