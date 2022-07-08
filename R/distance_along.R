#' Compute the distance between two nodes along a linear shape
#' 
#' @description
#' Computes distance between two nodes (sites) along a linear shape 
#' (e.g. a river, a road, etc.). This function uses parallel computing to speed
#' up the distance computation.
#' 
#' @param sites an `sf` object of type `POINT`. A spatial object
#'   containing coordinates of sites. Note that the first column must be the 
#'   site label.
#' 
#' @param along an `sf` object of type `LINESTRING`. A spatial object 
#'   containing coordinates of the linear shape (e.g. a river, a road, etc.)
#'   to follow while computing distances between two nodes (sites).
#' 
#' @param density a `numeric` of length 1. The density of points to sample on 
#'   the linear structure. See [sf::st_line_sample()] for further detail. 
#'   Default is `1/100`.
#' 
#' @param type a `character` of length 1. The method to sample points on the 
#'   linear shape. Either `regular` (default) or `random`.
#'   
#' @param mc.cores an `integer` of length 1. The number of cores to use 
#'   (must be lesser than the number of cores available on the machine obtained
#'   with [parallel::detectCores()]).
#' 
#' @param ... other argument to pass to  [sf::st_line_sample()].
#' 
#' @return A three-column `data.frame` with:
#'   - `from`, the first node
#'   - `to`, the second node
#'   - `weight`, the distance between the two nodes along the linear shape
#' 
#' @export
#'
#' @examples
#' # Import Adour river (LINESTRING) ----
#' path_to_file <- system.file("extdata", "adour_lambert93.gpkg", 
#'                             package = "bridge")
#' adour_river <- sf::st_read(path_to_file)
#' 
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites <- read.csv(path_to_file)
#' 
#' # Convert sites to sf object (POINTS) ----
#' adour_sites <- sf::st_as_sf(adour_sites, coords = 2:3, crs = "epsg:2154")
#' 
#' \dontrun{
#' # Compute distances between pairs of sites along the Adour river ----
#' dist_along_river <- distance_along(adour_sites, adour_river)
#' }

distance_along <- function(sites, along, density = 0.01, type = "regular", 
                           mc.cores = parallel::detectCores() - 1, ...) {

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
  
  
  ## Check 'along' argument ----
  
  if (missing(along)) {
    stop("Argument 'along' (spatial layer of linear shape) is required", 
         call. = FALSE)
  }
  
  if (!inherits(along, "sf")) {
    stop("The object 'along' must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(along) != 1) {
    stop("Argument 'along' (linear shape) should have exactly one row", 
         call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(along) %>% as.character() %>% unique()
  
  if (!("LINESTRING" %in% geom)) {
    stop("Linear shape geometry must be of type LINESTRING", call. = FALSE)
  }
  
  
  ## Check 'type' argument ----
  
  type <- tolower(type)
  
  if (!(type %in% c("regular", "random"))) {
    stop("Argument 'type' must either 'regular' or 'random'")
  }
  
  
  ## Convert LINESTRING to POINTS ----
  
  sampled_points <- line_to_points(along, density, type, ...)
  
  
  ## Find nearest points on line to each site ----
  
  nearest_points <- unlist(parallel::mclapply(1:nrow(sites), function(i) {
    which.min(sf::st_distance(sites[i, ], sampled_points))
  }, mc.cores = mc.cores))
  
  
  ## Create correspondence table ----
  
  nearest_points <- data.frame("id"   = nearest_points, 
                               "site" = sites[ , 1, drop = TRUE])
  
  
  ## Add origin site ----
  
  # nearest_points <- rbind(data.frame("id" = 1, "site" = "Origin"), 
  #                         nearest_points)
  
  
  ## Get all combination with two sites ----
  
  pairs_of_sites <- expand.grid(nearest_points$"site", nearest_points$"site", 
                                stringsAsFactors = FALSE)
  colnames(pairs_of_sites) <- c("from", "to")
  
  
  ## Create spatial segment between all sites pairs ----
  
  along_segments <- do.call(rbind.data.frame, 
                            parallel::mclapply(1:nrow(pairs_of_sites), 
                                               function(i) {
                                                 
    site_from <- which(nearest_points$"site" == pairs_of_sites[i, "from"])
    site_to   <- which(nearest_points$"site" == pairs_of_sites[i, "to"])
    
    points_to_line(points_sf = sampled_points,
                   from      = nearest_points[site_from, "id"],
                   to        = nearest_points[site_to, "id"]) %>% 
      
    dplyr::mutate(from = pairs_of_sites[i, "from"],
                  to   = pairs_of_sites[i, "to"]) %>% 
      
    dplyr::select(.data$from, .data$to)
    
  }, mc.cores = mc.cores))

  
  ## Compute distance between all sites pairs ----
  
  distance_along <- sf::st_length(along_segments) %>% as.numeric()
  
  
  ## Export final table ----
  
  along_segments <- along_segments %>% 
    dplyr::mutate(weight = distance_along) %>% 
    dplyr::select(1, 2, 4) %>% 
    sf::st_drop_geometry() %>% 
    as.data.frame()
  
  along_segments <- along_segments[with(along_segments, order(from, to)), ]
  rownames(along_segments) <- NULL
  
  along_segments
}
