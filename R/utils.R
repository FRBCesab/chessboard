#' Create labels for edges
#' 
#' Creates labels for edges (non edges are ignored). If two edges are identical
#' (e.g. `"S01-S02"` and `"S02-S01"`), there will have the same label 
#' (`"E-01"`).
#' 
#' @param edges a `data.frame` with three columns:
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#'   - `edge`: 0 (no edge) or 1 (edge)
#' 
#' @note For internal purpose only.
#' 
#' @noRd

create_edges_id <- function(edges) {
  
  if (missing(edges)) {
    stop("Argument 'edges' is required", call. = FALSE)
  }
  
  if (!is.data.frame(edges)) {
    stop("Argument 'edges' must be a data.frame", call. = FALSE)
  }
  
  if (!("from" %in% colnames(edges))) {
    stop("The column 'from' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(edges))) {
    stop("The column 'to' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("edge" %in% colnames(edges))) {
    stop("The column 'edge' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  
  ## Subset edges and non-edges ----
  
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  
  no_edges <- edges[edges$"edge" == 0, ]
  edges    <- edges[edges$"edge" == 1, ]
  
  
  ## Extract nodes ----
  
  nodes <- sort(unique(as.character(c(edges$"from", edges$"to"))))
  nodes <- data.frame("node"      = nodes, 
                      "nodes_int" = seq_len(length(nodes)))
  
  
  ## Add information on nodes in edges list ----
  
  edges <- merge(edges, nodes, by.x = "from", by.y = "node")
  colnames(edges)[ncol(edges)] <- "from_int"
  
  edges <- merge(edges, nodes, by.x = "to", by.y = "node")
  colnames(edges)[ncol(edges)] <- "to_int"
  
  edges <- edges[with(edges, order(from, to)), c(3, 2, 1, 4, 5)]
  
  
  ## Split edges according to direction ----
  
  edges_down <- edges[which(edges$"from_int" <= edges$"to_int"), ]
  edges_up   <- edges[which(edges$"from_int" >  edges$"to_int"), ]
  
  
  ## Sort edges list ----
  
  edges_down <- edges_down[with(edges_down, order(from, to)), ]
  
  
  ## Create edge ids for one direction ----
  
  edges_down$"edge_id" <- seq_len(nrow(edges_down))
  edges_down$"edge_id" <- format(edges_down$"edge_id")
  edges_down$"edge_id" <- paste0("E-", edges_down$"edge_id")
  edges_down$"edge_id" <- gsub("\\s", "0", edges_down$"edge_id")
  
  
  ## Add ID to edges for other direction ----
  
  edges_down$"key" <- paste0(edges_down$"from", edges_down$"to")
  edges_up$"key"   <- paste0(edges_up$"to", edges_up$"from")
  
  edges_up <- merge(edges_up, edges_down[ , c("edge_id", "key")], by = "key", 
                    all = FALSE)
  
  
  ## Merge subset ----
  
  edges <- rbind(edges_down[ , c("from", "to", "edge", "edge_id")],
                 edges_up[ , c("from", "to", "edge", "edge_id")])
  
  if (nrow(no_edges) > 0) {
    
    no_edges$"edge_id" <- ""
    
    edges <- rbind(edges[ , c("from", "to", "edge", "edge_id")],
                   no_edges[ , c("from", "to", "edge", "edge_id")])
  }
  
  
  ## Output ----
  
  edges <- edges[with(edges, order(from, to)), c(4:3, 1:2)]
  
  rownames(edges) <- NULL
  
  edges
}



#' Convert a LINESTRING to POINT
#' 
#' @description
#' Converts a spatial line of type `LINESTRING` into `POINT` by sampling points
#' on a linear geometry using the function [sf::st_line_sample()]. This 
#' function is used by the function [distance_along()].
#' 
#' @param x an `sf` object of type `LINESTRING`.
#' 
#' @inheritParams distance_along
#' 
#' @noRd
#' 
#' @examples
#' \dontrun{
#' # Import Adour river (LINESTRING) ----
#' path_to_file <- system.file("extdata", "adour_lambert93.gpkg", 
#'                             package = "chessboard")
#' adour_river <- sf::st_read(path_to_file)
#' 
#' # Sample points of the Adour river ----
#' adour_river_pts <- line_to_points(adour_river)
#' }

line_to_points <- function(x, density = 0.01, type = "regular", ...) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' (spatial layer of linear shape) is required", 
         call. = FALSE)
  }
  
  if (!inherits(x, "sf")) {
    stop("The object 'x' must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(x) != 1) {
    stop("Argument 'x' should have exactly one row", 
         call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(x) %>% as.character() %>% unique()
  
  if (!("LINESTRING" %in% geom)) {
    stop("Linear shape geometry must be of type LINESTRING", call. = FALSE)
  }
  
  
  ## Check 'type' argument ----
  
  type <- tolower(type)
  
  if (!(type %in% c("regular", "random"))) {
    stop("Argument 'type' must either 'regular' or 'random'")
  }
  
  
  ## Check density argument ----
  
  if (!is.numeric(density) || length(density) != 1) {
    stop("Argument 'density' must be a numeric of length 1", call. = FALSE)
  }
  
  if (density <= 0) {
    stop("Argument 'density' must be > 0", call. = FALSE)
  }
  
  
  ## Convert LINESTRING to POINT ----
  
  sampled_points <- sf::st_line_sample(x, density = density, type = type, ...)
  
  if (sf::st_is_empty(sampled_points)) {
    stop("Unable to sample points along the linear shape. Please increase ", 
         "the value of 'density'", call. = FALSE)
  }
  
  sampled_points <- sampled_points %>% 
    sf::st_sf() %>% 
    sf::st_cast("POINT") %>% 
    dplyr::mutate(id = 1:dplyr::n(), group = 1) %>%
    dplyr::select(.data$id, .data$group)
}



#' Convert POINT to a LINESTRING
#' 
#' @description
#' Converts spatial points of type `POINT` into a `LINESTRING`. This function
#' is used by the function [distance_along()] and is the opposite of 
#' [line_to_points()].
#' 
#' @param points_sf an `sf` object of type `POINT`. Typically points sampled on
#'   a LINESTRING as the output of [line_to_points()].
#' 
#' @param from an `integer` of length 1. The start of the segment (row number 
#'   in `points_sf`).
#'   
#' @param to an `integer` of length 1. The end of the segment (row number in 
#'   `points_sf`).
#' 
#' @noRd
#' 
#' @examples
#' \dontrun{
#' # Import Adour river (LINESTRING) ----
#' path_to_file <- system.file("extdata", "adour_lambert93.gpkg", 
#'                             package = "chessboard")
#' adour_river <- sf::st_read(path_to_file)
#' 
#' # Sample points of the Adour river ----
#' adour_river_pts <- line_to_points(adour_river)
#' 
#' # Convert back points to LINESTRING ----
#' adour_river_all <- points_to_line(adour_river_pts, 1, nrow(adour_river_pts))
#' 
#' # Extract on segment on the river ----
#' adour_river_seg <- points_to_line(adour_river_pts, 100, 350)
#' 
#' # Map ----
#' plot(sf::st_geometry(adour_river))
#' plot(sf::st_geometry(adour_river_all), add = TRUE, col = "red")
#' plot(sf::st_geometry(adour_river_seg), add = TRUE, col = "blue", lwd = 2)
#' }

points_to_line <- function(points_sf, from, to) {
  
  ## Check 'points_sf' argument ----
  
  if (missing(points_sf)) {
    stop("Argument 'points_sf' is required", call. = FALSE)
  }
  
  if (!inherits(points_sf, "sf")) {
    stop("The object 'points_sf' must be an 'sf' object", call. = FALSE)
  }
  
  if (nrow(points_sf) < 2) {
    stop("Argument 'points_sf' should have at least two rows", call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(points_sf) %>% as.character() %>% unique()
  
  if (length(geom) > 1) {
    stop("Argument 'points_sf' cannot contain different geometries", 
         call. = FALSE)
  }
  
  if (!("POINT" %in% geom)) {
    stop("Geometry of 'points_sf' must be of type POINT", call. = FALSE)
  }
  
  if (!("group" %in% colnames(points_sf))) {
    stop("The column 'group' is absent from 'points_sf'", call. = FALSE)
  }
  
  
  ## Check 'from' argument ----
  
  if (missing(from)) {
    stop("Argument 'from' is required", call. = FALSE)
  }
  
  if (!is.numeric(from) || length(from) != 1) {
    stop("Argument 'from' must be an integer of length 1", call. = FALSE)
  }
  
  if (!(from %in% 1:nrow(points_sf))) {
    stop("Argument 'from' must be between 1 and number of rows in 'points_sf'",
         call. = FALSE)
  }
  
  
  ## Check 'to' argument ----
  
  if (missing(to)) {
    stop("Argument 'to' is required", call. = FALSE)
  }
  
  if (!is.numeric(to) || length(to) != 1) {
    stop("Argument 'to' must be an integer of length 1", call. = FALSE)
  }
  
  if (!(to %in% 1:nrow(points_sf))) {
    stop("Argument 'to' must be between 1 and number of rows in 'points_sf'",
         call. = FALSE)
  }
  
  
  ## Extract segment ----
  
  points_sf[from:to, ] %>% 
    dplyr::group_by(.data$group) %>% 
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("LINESTRING") %>% 
    dplyr::ungroup()
}



#' Sort edges in natural ordering
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

sort_edges <- function(edges) {
  
  ## Check argument ----
  
  check_edges_object(edges)
  
  
  ## Split labels ----
  
  edges$"by_1" <- as.numeric(unlist(lapply(strsplit(edges$"from", "-"), 
                                           function(x) x[1])))
  edges$"by_2" <- as.numeric(unlist(lapply(strsplit(edges$"from", "-"), 
                                           function(x) x[2])))
  edges$"by_3" <- as.numeric(unlist(lapply(strsplit(edges$"to", "-"), 
                                           function(x) x[1])))
  edges$"by_4" <- as.numeric(unlist(lapply(strsplit(edges$"to", "-"), 
                                           function(x) x[2])))
  
  edges <- edges[with(edges, order(by_1, by_2, by_3, by_4)), 1:2]
  rownames(edges) <- NULL
  
  edges
}



#' Sort nodes labels in natural ordering
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

get_sorted_nodes <- function(edges) {
  
  ## Check argument ----
  
  check_edges_object(edges)
  
  
  ## Split labels ----
  
  nodes <- unique(c(edges$"from", edges$"to"))
  
  nodes <- data.frame(
    "node" = nodes,
    "by_1" = as.numeric(unlist(lapply(strsplit(nodes, "-"), 
                                      function(x) x[1]))),
    "by_2" = as.numeric(unlist(lapply(strsplit(nodes, "-"), 
                                           function(x) x[2]))))
  
  nodes[with(nodes, order(by_1, by_2)), "node", drop = TRUE]
}
