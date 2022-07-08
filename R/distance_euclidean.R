#' Compute the Euclidean distance between two nodes
#' 
#' @description
#' Computes the Euclidean distance between two nodes using the function 
#' [sf::st_distance()]. If the CRS is not a Cartesian system, the Great Circle
#' distance will be used instead.
#' 
#' @param ... other argument to pass to [sf::st_distance()].
#' 
#' @inheritParams distance_along
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
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites <- read.csv(path_to_file)
#' 
#' # Convert sites to sf object (POINTS) ----
#' adour_sites <- sf::st_as_sf(adour_sites, coords = 2:3, crs = "epsg:2154")
#' 
#' # Compute distances between pairs of sites along the Adour river ----
#' distance_euclidean(adour_sites)

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
    stop("Argument 'sites' should have at least two columns: site label and ", 
         "geometry", call. = FALSE)
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
  
  
  ## Order sites ----
  
  sites <- sites[order(sites[ , 1, drop = TRUE]), ]
  
  
  ## Extract nodes ----
  
  nodes <- nodes_list(sites[ , 1, drop = TRUE])
  
  
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
