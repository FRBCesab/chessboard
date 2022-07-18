#' Check the structure of the data.frame nodes
#' 
#' @param nodes a `data.frame`. This output of [create_nodes_labels()].
#' 
#' @noRd

check_nodes_object <- function(nodes) {
  
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
  
  if (nrow(nodes) < 2) {
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
  
  invisible(NULL)
}



#' Check the structure of focal node and its presence in the data.frame nodes
#' 
#' @param nodes a `data.frame`. This output of [create_nodes_labels()].
#' 
#' @param focus an `character` of length 1. The node label for which the 
#'   neighbors must be found. Must exist in the `nodes` object.
#' 
#' @noRd

check_focus_object <- function(nodes, focus) {
  
  check_nodes_object(nodes)
  
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
  
  invisible(NULL)
}



#' Check the argument degree (of neighborhood)
#' 
#' @param degree an `integer` of length 1. The maximum number of neighbors to 
#'   search for in one direction.
#' 
#' @noRd

check_degree_value <- function(degree) {
  
  if (!is.numeric(degree)) {
    stop("Argument 'degree' must be a numeric", call. = FALSE)
  }
  
  if (length(degree) != 1) {
    stop("Argument 'degree' must be a numeric of length 1", call. = FALSE)
  }
  
  if (degree <= 0) {
    stop("Argument 'degree' must be strictly positive", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check the structure of the data.frame neighbors
#' 
#' @param neighbors a `data.frame`. This output of the functions [pawn()], 
#'   [fool()], etc.
#' 
#' @noRd

check_neighbors_object <- function(neighbors) {
  
  if (missing(neighbors)) {
    stop("Argument 'neighbors' is required ", 
         "(output of the functions pawn(), fool(), etc.)", call. = FALSE)
  }
  
  if (!is.data.frame(neighbors)) {
    stop("Argument 'neighbors' must be a data.frame ", 
         "(output of the functions pawn(), fool(), etc.", call. = FALSE)
  }
  
  if (!("node" %in% colnames(neighbors))) {
    stop("The column 'node' is absent from the 'neighbors' data.frame ", 
         "(output of the functions pawn(), fool(), etc.)", call. = FALSE)
  }
  
  if (!("transect" %in% colnames(neighbors))) {
    stop("The column 'transect' is absent from the 'neighbors' data.frame ", 
         "(output of the functions pawn(), fool(), etc.)", call. = FALSE)
  }
  
  if (!("quadrat" %in% colnames(neighbors))) {
    stop("The column 'quadrat' is absent from the 'neighbors' data.frame ", 
         "(output of the functions pawn(), fool(), etc.)", call. = FALSE)
  }
  
  if (nrow(neighbors) < 1) {
    stop("Argument 'neighbors' must have at least one row (neighbor)", 
         call. = FALSE)
  }
  
  if (!is.numeric(neighbors$"transect")) {
    stop("The column 'transect' of the 'neighbors' data.frame must be a ", 
         "numeric", call. = FALSE)
  }
  
  if (!is.numeric(neighbors$"quadrat")) {
    stop("The column 'quadrat' of the 'neighbors' data.frame must be a ", 
         "numeric", call. = FALSE)
  }
  
  if (!is.character(neighbors$"node")) {
    stop("The column 'node' of the 'neighbors' data.frame must be a ", 
         "character", call. = FALSE)
  }
  
  invisible(NULL)
}
