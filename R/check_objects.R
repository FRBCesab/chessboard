#' Check the structure of the data.frame nodes
#' 
#' @param nodes a `data.frame`. The output of [create_node_labels()].
#' 
#' @noRd

check_nodes_object <- function(nodes) {
  
  if (missing(nodes)) {
    stop("Argument 'nodes' is required ", 
         "(output of the function create_node_labels())", call. = FALSE)
  }
  
  if (!is.data.frame(nodes)) {
    stop("Argument 'nodes' must be a data.frame ", 
         "(output of the function create_node_labels())", call. = FALSE)
  }
  
  if (!("node" %in% colnames(nodes))) {
    stop("The column 'node' is absent from the 'nodes' data.frame ", 
         "(output of the function create_node_labels())", call. = FALSE)
  }
  
  if (!("transect" %in% colnames(nodes))) {
    stop("The column 'transect' is absent from the 'nodes' data.frame ", 
         "(output of the function create_node_labels())", call. = FALSE)
  }
  
  if (!("quadrat" %in% colnames(nodes))) {
    stop("The column 'quadrat' is absent from the 'nodes' data.frame ", 
         "(output of the function create_node_labels())", call. = FALSE)
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
  
  check_node_labels(nodes$"node")
  
  invisible(NULL)
}



#' Check the structure of focal node and its presence in the data.frame nodes
#' 
#' @param nodes a `data.frame`. The output of [create_node_labels()].
#' 
#' @param focus a `character` of length 1. The node label for which the 
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
    stop(paste0("The node '", focus, "' is absent from the node list ", 
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
  
  if (is.null(degree)) {
    stop("Argument 'degree' is required", call. = FALSE)
  }
  
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
#' @param neighbors a `data.frame`. The output of the functions [pawn()], 
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
  
  check_node_labels(neighbors$"node")
  
  invisible(NULL)
}



#' Check the name of the method to detect neighbors
#' 
#' @param method a `character` of length 1. One among `'pawn'`, `'fool'`, 
#'  `'rook'`, `'bishop'`, `'bishop_left'`, `'bishop_right'`, `'knight'`, 
#'  `'knight_left'`, `'knight_right'`, `'queen'`, `'wizard'`.
#' 
#' @noRd

check_neighbors_method <- function(method) {
  
  if (missing(method)) {
    stop("The argument 'method' is required", call. = FALSE)
  }
  
  if (is.null(method)) {
    stop("The argument 'method' cannot be NULL", call. = FALSE)
  }
  
  if (!is.character(method) || length(method) != 1) {
    stop("The argument 'method' must be a character of length 1", 
         call. = FALSE)
  }
  
  available_methods <- c("pawn", "fool", "rook", "bishop", "bishop_left", 
                         "bishop_right", "knight", "knight_left", 
                         "knight_right", "queen", "wizard")
  
  error_msg <- paste0(available_methods, collapse = ", ")
  
  if (!(method %in% available_methods)) {
    stop("Argument 'method' must be one of ", error_msg, call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for boolean values
#' 
#' @param boolean a `logical` value of length 1.
#' 
#' @noRd

check_logical_value <- function(boolean) {
  
  if (is.null(boolean)) {
    stop("The argument '", deparse(substitute(boolean)), "' cannot be NULL", 
         call. = FALSE)
  }

  if (!is.logical(boolean) || length(boolean) != 1) {
    stop("The argument '", deparse(substitute(boolean)), "' must be a ", 
         "logical (TRUE or FALSE) of length 1", call. = FALSE)
  }
  
  if (is.na(boolean)) {
    stop("The argument '", deparse(substitute(boolean)), "' cannot be NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for the data.frame edges (edge list)
#'
#' @param edges a `data.frame` with the column `from` and `to`. The output of 
#'   the function [create_edge_list()].
#'
#' @noRd

check_edges_object <- function(edges) {
  
  if (missing(edges)) {
    stop("Argument 'edges' is required ", 
         "(output of the function create_edge_list())", call. = FALSE)
  }
  
  if (!is.data.frame(edges)) {
    stop("Argument 'edges' must be a data.frame ", 
         "(output of the function create_edge_list())", call. = FALSE)
  }
  
  if (!("from" %in% colnames(edges))) {
    stop("The column 'from' is absent from the 'edges' data.frame ", 
         "(output of the function create_edge_list())", call. = FALSE)
  }
  
  if (!("to" %in% colnames(edges))) {
    stop("The column 'to' is absent from the 'edges' data.frame ", 
         "(output of the function create_edge_list())", call. = FALSE)
  }
 
  if (nrow(edges) == 0) {
    stop("Argument 'edges' must have at least one row (edge)", call. = FALSE)
  }
  
  if (!is.character(edges$"from")) {
    stop("The column 'from' of the 'edges' data.frame must be a character", 
         call. = FALSE)
  }
  
  if (!is.character(edges$"to")) {
    stop("The column 'to' of the 'edges' data.frame must be a character", 
         call. = FALSE)
  }
  
  check_node_labels(edges$"from")
  check_node_labels(edges$"to")

  invisible(NULL)  
}


check_sites_object <- function(sites) {
  
  if (missing(sites)) {
    stop("Argument 'sites' (spatial layer of sites) is required", 
         call. = FALSE)
  }
  
  if (!inherits(sites, "sf")) {
    stop("The object 'sites' must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(sites) < 2) {
    stop("Argument 'sites' should have at least two rows (sites)", 
         call. = FALSE)
  }
  
  if (ncol(sites) < 2) {
    stop("Argument 'sites' should have at least two columns: 'node' ",
         "and 'geometry'", call. = FALSE)
  }
  
  if (colnames(sites)[1] != "node") {
    stop("The first column of 'sites' must be named 'node' (nodes labels)",
         call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(sites) %>% as.character() %>% unique()
  
  if (length(geom) > 1) {
    stop("Argument 'sites' (spatial layer of sites) cannot contain different ", 
         "geometries", call. = FALSE)
  }
  
  if (!("POINT" %in% geom)) {
    stop("Sites geometry must be of type POINT", call. = FALSE)
  }
  
  if (any(duplicated(sites[ , 1, drop = TRUE]))) {
    stop("The argument 'sites' cannot contain duplicates", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check nodes labels
#'
#' @param nodes a `character` vector of nodes labels (e.g. 1-1, 01-10)
#'
#' @noRd

check_node_labels <- function(nodes) {
  
  pattern <- "^[0-9]{1,}-[0-9]{1,}$"
  
  if (length(grep(pattern, nodes)) == 0) {
    stop("Nodes labels have not the good form", call. = FALSE)
  }
  
  if (length(grep(pattern, nodes)) < length(nodes)) {
    stop("Some nodes labels are malformed", call. = FALSE)
  }
  
  invisible(NULL)
}
