#' Find edges between nodes based on a degree of neighborhood
#' 
#' @description
#' Finds edges (links) between nodes (sites) based on a degree of neighborhood.
#' The nodes (argument `x`) will be ordered to find edges in a directional way 
#' (from upstream to downstream along a linear shape).
#' 
#' **IMPORTANT:** The order of sites must be found in the sites labels.
#' 
#' @param x a `character` vector of nodes (sites) labels.
#' 
#' @param level an `integer` of length 1. The number of neighbors used to 
#'   define vertices.
#'
#' @param self a `logical` of length 1. If `TRUE`, a site can be linked to 
#'   itself. Default is `FALSE`.
#'
#' @return A `data.frame` with three columns:
#'   - `from`: label of one of the two edges of the vertice (link)
#'   - `to`: label of the other edge of the vertice
#'   - `weight`: the presence or absence of vertice between two edges
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # List of edges with 1 degree of neighborhood ----
#' edges_list(adour_sites$"site")
#' }

edges_list <- function(x, level = 1, self = FALSE) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.character(x)) {
    stop("Argiment 'x' must be a character vector (sites labels)", 
         call. = FALSE)
  }
  
  
  ## Check 'level' argument ----
  
  if (!is.numeric(level) || length(level) != 1) {
    stop("Argument 'level' must be an integer of length 1", call. = FALSE)
  }
  
  if (level > (length(x) - 1)) {
    stop("Argument 'level' must be 'strictly < length(x)'")
  }
  
  
  ## Order sites labels ----
  
  x    <- sort(x)
  dict <- data.frame(x, "x_int" = seq_len(length(x)))
  
  x <- expand.grid(x, x)
  colnames(x) <- c("from", "to")
  
  x <- x[order(x$"from"), ]
  
  x <- merge(x, dict, by.x = "to", by.y = "x")
  colnames(x)[ncol(x)] <- "to_int"
  
  x <- merge(x, dict, by.x = "from", by.y = "x")
  colnames(x)[ncol(x)] <- "from_int"
  
  x <- x[with(x, order(from, to)), ]
  
  x$"weight" <- 0
  x[which(abs(x$"from_int" - x$"to_int") <= level), "weight"] <- 1
  
  x <- x[ , -c(3:4)]
  
  if (!self) {
    x[which(x$"from" == x$"to"), "weight"] <- 0
  }
  
  x
}
