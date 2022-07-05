#' Convert POINTS to a LINESTRING
#' 
#' @description
#' Converts spatial points of type `POINT` into a `LINESTRING`. This function
#' is used by the function [distance_along()] and is the opposite of 
#' [line_to_points()].
#' 
#' @param points_sf an `sf` object of type `POINT`. Typically points sampled on
#'   a LINESTRING as the output of [line_to_points()].
#' 
#' @param from an `integer` of length 1. The start of the segment (row number in 
#'   `points_sf`).
#'   
#' @param to an `integer` of length 1. The end of the segment (row number in 
#'   `points_sf`).
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ## Import Adour river (LINESTRING) ----
#' 
#' path_to_file <- system.file("extdata", "adour_lambert93.gpkg", 
#'                             package = "bridge")
#'                             
#' adour_river <- sf::st_read(path_to_file)
#' 
#' 
#' ## Sample points of the Adour river ----
#' 
#' adour_river_pts <- line_to_points(adour_river)
#' 
#' 
#' ## Convert back points to LINESTRING ----
#' 
#' adour_river_all <- points_to_line(adour_river_pts, 1, nrow(adour_river_pts))
#' 
#' 
#' ## Extract on segment on the river ----
#' 
#' adour_river_seg <- points_to_line(adour_river_pts, 100, 350)
#' 
#' 
#' ## Map ----
#' 
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
  
  
  ## Check 'from' and 'to' arguments ----
  
  if (from > to) {
    stop("Argument 'from' must be lesser than argument 'to'", call. = FALSE)
  }
  
  
  ## Extract segment ----
  
  points_sf[from:to, ] %>% 
    dplyr::group_by(.data$group) %>% 
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("LINESTRING") %>% 
    dplyr::ungroup()
}
