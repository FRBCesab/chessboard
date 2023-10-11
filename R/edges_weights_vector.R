#' Create an edges weights vector
#' 
#' @description
#' Creates an edges weights vector that can be used in `aem()` of the package 
#' `adespatial`. Resulting edges weights equal to 0 will be replaced by 
#' `4 x max(w)`, where `max(w)` is the maximal weight in the matrix.
#'
#' @param x a `list` of length 2. The nodes_by_edges matrix returned by 
#'   `nodes_by_edges_matrix()` (or `aem.build.binary()` of the package 
#'   `adespatial`).
#'   
#' @param y a `data.frame` with the following three columns: `from` 
#'   (the first node of the edge), `to` (the second node of the edge), and 
#'   `weight` (the weight of the edge between the two nodes, e.g. a distance).
#'
#' @return An edges weights `vector`.
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
#'                             package = "chessboard")
#' adour_sites <- read.csv(path_to_file)
#' 
#' # Select the 15 first sites ----
#' adour_sites <- adour_sites[1:15, ]
#' 
#' # Create node labels ----
#' adour_sites <- create_node_labels(adour_sites, 
#'                                   location = "location", 
#'                                   transect = "transect", 
#'                                   quadrat  = "quadrat")
#' 
#' # Convert sites to sf object (POINTS) ----
#' adour_sites_sf <- sf::st_as_sf(adour_sites, 
#'                                coords = c("longitude", "latitude"),
#'                                crs = "epsg:2154")
#'
#' # Create edges based on the pawn move (directed network) ----
#' adour_edges <- create_edge_list(adour_sites, method = "pawn", 
#'                                 directed = TRUE)
#' 
#' # Create nodes-by-edges matrix ----
#' adour_matrix <- nodes_by_edges_matrix(adour_edges)
#' 
#' # Compute Euclidean distances between pairs of sites ----
#' adour_dists <- distance_euclidean(adour_sites_sf)
#' 
#' # Create Edges weights vector ----
#' edges_weights_vector(adour_matrix, adour_dists)

edges_weights_vector <- function(x, y) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.list(x)) {
    stop("Argument 'x' must be a list (nodes-by-edges matrix)", call. = FALSE)
  }
  
  if (length(x) != 2) {
    stop("Argument 'x' must be a list (nodes-by-edges matrix) of length 2",
         call. = FALSE)
  }
  
  if (any(!(names(x) %in% c("se.mat", "edges")))) {
    stop("Argument 'x' must be a nodes-by-edges matrix as returned by ",
         "nodes_by_edges_matrix() or aem.build.binary() (adespatial)", 
         call. = FALSE)
  }
  
  
  ## Check 'y' argument ----
  
  if (missing(y)) {
    stop("Argument 'y' is required", call. = FALSE)
  }
  
  if (!is.data.frame(y)) {
    stop("Argument 'y' must be a data.frame", call. = FALSE)
  }
  
  if (!("from" %in% colnames(y))) {
    stop("The column 'from' is absent from the y data.frame", 
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(y))) {
    stop("The column 'to' is absent from the y data.frame", 
         call. = FALSE)
  }
  
  if (!("weight" %in% colnames(y))) {
    stop("The column 'weight' is absent from the y data.frame", 
         call. = FALSE)
  }
  
  if (nrow(y) == 0) {
    stop("Argument 'y' must have at least one row", call. = FALSE)
  }
  
  
  ## Output ----
  
  x$"edges"$"edge" <- rownames(x$"edges")
  weights <- merge(x$"edges", y, by = c("from", "to"), all.x = TRUE, 
                   all.y = FALSE)
  
  weights$"weight" <- ifelse((is.na(weights$"weight") | weights$"weight" == 0),
                             4 * max(weights$"weight", na.rm = TRUE), 
                             weights$"weight")
  
  row_names <- weights$"edge"
  weights <- weights[ , "weight", drop = TRUE]
  names(weights) <- row_names
  
  weights
}
