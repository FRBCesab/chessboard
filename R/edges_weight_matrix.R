#' Create an edges weight matrix
#' 
#' @description
#' Creates an edges weight matrix from the output of [distance_euclidean()] and
#' [distance_along()].
#' 
#' @param distances a `data.frame` with the following three columns: `from` 
#'   (the first node of the edge), `to` (the second node of the edge), and 
#'   `weight` (the weight of the edge between the two nodes, e.g. a distance).
#' 
#' @param lower a logical value. If `TRUE` (default), keep values in the lower 
#'   triangle of the matrix. Otherwise they will be replaced by `NA`.
#' 
#' @param upper a logical value. If `TRUE` (default), keep values in the upper 
#'   triangle of the matrix. Otherwise they will be replaced by `NA`.
#' 
#' @param diag a logical value. If `TRUE` (default), keep values in the 
#'   diagonal of the matrix. Otherwise they will be replaced by `NA`.
#'
#' @return An edges weight matrix of dimensions `n x n`, where `n` is the 
#'   number of nodes (sites).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "chessboard")
#' adour_sites <- read.csv(path_to_file)
#' 
#' # Convert sites to sf object (POINTS) ----
#' adour_sites <- sf::st_as_sf(adour_sites, coords = 2:3, crs = "epsg:2154")
#' 
#' # Compute distances between pairs of sites along the Adour river ----
#' adour_dists <- distance_euclidean(adour_sites)
#' 
#' # Create Edges weight matrix ----
#' edges_weight_matrix(adour_dists)
#' 
#' # Create Edges weight matrix (with options) ----
#' edges_weight_matrix(adour_dists, lower = FALSE)
#' edges_weight_matrix(adour_dists, upper = FALSE)
#' edges_weight_matrix(adour_dists, diag = FALSE)
#' }

edges_weight_matrix <- function(distances, lower = TRUE, upper = TRUE, 
                                diag = TRUE) {
  
  
  ## Check distances argument ----
  
  if (missing(distances)) {
    stop("Argument 'distances' is required", call. = FALSE)
  }
  
  if (!is.data.frame(distances)) {
    stop("Argument 'distances' must be a data.frame", call. = FALSE)
  }
  
  if (!("from" %in% colnames(distances))) {
    stop("The column 'from' is absent from the distances data.frame", 
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(distances))) {
    stop("The column 'to' is absent from the distances data.frame", 
         call. = FALSE)
  }
  
  if (!("weight" %in% colnames(distances))) {
    stop("The column 'weight' is absent from the distances data.frame", 
         call. = FALSE)
  }
  
  if (nrow(distances) == 0) {
    stop("Argument 'distances' must have at least one row", call. = FALSE)
  }
  
  
  ## Pivot data frame ----
  
  mat <- tidyr::pivot_wider(distances, names_from = "to", 
                            values_from = "weight", values_fn = ~.x)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Apply filters ----
  
  if (!upper) mat[upper.tri(mat)] <- NA
  if (!lower) mat[lower.tri(mat)] <- NA
  if (!diag)  diag(mat) <- NA
  
  mat
}
