#' Compute the pairwise Euclidean distance
#' 
#' @description
#' Computes the Euclidean distance between two nodes using the function 
#' [sf::st_distance()]. If the CRS is not a Cartesian system, the Great Circle
#' distance will be used instead.
#' 
#' @param sites an `sf` object of type `POINT`. A spatial object
#'   containing coordinates of sites. Note that the first column must be the 
#'   node label created by the function [create_node_labels()].
#'   
#' @param ... other argument to pass to [sf::st_distance()].
#' 
#' @return A three-column `data.frame` with:
#'   - `from`, the first node
#'   - `to`, the second node
#'   - `weight`, the Euclidean distance between the two nodes
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
#' adour_sites <- sf::st_as_sf(adour_sites, coords = c("longitude", "latitude"),
#'                             crs = "epsg:2154")
#' 
#' # Compute distances between pairs of sites ----
#' weights <- distance_euclidean(adour_sites)
#' 
#' head(weights)

distance_euclidean <- function(sites, ...) {
  
  ## Check 'sites' argument ----
  
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
    stop("Argument 'sites' should have at least two columns: node label and ", 
         "geometry", call. = FALSE)
  }
  
  if (!("node" %in% colnames(sites))) {
    stop("The column 'node' is absent from the object 'site'", call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(sites) %>% as.character() %>% unique()
  
  if (length(geom) > 1) {
    stop("Argument 'sites' (spatial layer of sites) cannot contain different ", 
         "geometry types", call. = FALSE)
  }
  
  if (!("POINT" %in% geom)) {
    stop("Sites geometry must be of type POINT", call. = FALSE)
  }
  
  if (is.na(sf::st_crs(sites))) {
    stop("The 'sites' layer has not a valid CRS", call. = FALSE)
  }
  
  if (any(duplicated(sites[ , "node", drop = TRUE]))) {
    stop("The argument 'sites' cannot contain duplicated nodes", call. = FALSE)
  }
  
  
  ## Order sites ----
  
  sites <- sites[order(sites[ , "node", drop = TRUE]), ]
  
  
  ## Extract nodes ----
  
  nodes <- sites[ , "node", drop = TRUE]
  
  
  ## Compute Euclidean/Great Circle distance on each pairs of sites ----
  
  distances <- sf::st_distance(sites, ...)
  distances <- as.data.frame(distances)
  
  
  ## Convert object ----
  
  for (i in seq_len(ncol(distances))) 
    distances[ , i] <- as.numeric(distances[ , i])
  
  distances <- data.frame("from" = nodes, distances)
  colnames(distances)[-1] <- nodes
  
  
  ## Pivot to longer format ----
  
  distances <- tidyr::pivot_longer(distances, cols = -1, names_to = "to", 
                                   values_to = "weight")
  
  distances <- as.data.frame(distances)
  
  
  ## Order rows ----
  
  distances <- distances[with(distances, order(from, to)), ]
  rownames(distances) <- NULL
  
  distances
}
