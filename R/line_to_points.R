#' Convert a LINESTRING to POINT
#' 
#' @description
#' Converts a spatial line of type `LINESTRING` into `POINT` by sampling points
#' on a linear geometry using the function [sf::st_line_sample()]. This function
#' is used by the function [distance_along()].
#' 
#' @param x an `sf` object of type `LINESTRING`.
#' 
#' @inheritParams distance_along
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
#' ## Sample points of the Adour river ----
#' 
#' adour_river_pts <- line_to_points(adour_river)
#' }

line_to_points <- function(x, density = 0.01, type = "regular", ...) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' (spatial coordinates of linear structure) is ", 
         "required", call. = FALSE)
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
  
  if (length(geom) > 1) {
    stop("Argument 'x' (linear structure) cannot contain different ", 
         "geometries", call. = FALSE)
  }
  
  if (!("LINESTRING" %in% geom)) {
    stop("Linear structure geometry must be of type LINESTRING", call. = FALSE)
  }
  
  
  ## Check 'type' argument ----
  
  if (!(type %in% c("regular", "random"))) {
    stop("Argument 'type' must either 'regular' or 'random'")
  }
  
  
  ## Convert LINESTRING to POINT ----
  
  sf::st_line_sample(x, density = density, type = type, ...) %>% 
    sf::st_sf() %>% 
    sf::st_cast("POINT") %>% 
    dplyr::mutate(id = 1:dplyr::n(), group = 1) %>%
    dplyr::select(.data$id, .data$group)
}
