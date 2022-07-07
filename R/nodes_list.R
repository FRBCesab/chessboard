#' Retrieve and order nodes from an edges list
#' 
#' Retrieves and orders nodes from an edges list or from a vector of nodes.
#' 
#' @param x either a `data.frame` with at least two columns: `from`, the label 
#'   of one of the two nodes of the edge and `to`, label of the other node of 
#'   the edge, or a `character` vector with nodes labels.
#' 
#' @return A `data.frame` with one column, `node` containing unique ordered 
#'   nodes labels. 
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Retrieve nodes (from nodes vector) ----
#' nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood (undirected network) ----
#' edges <- edges_list(adour_sites$"site")
#' 
#' # Retrieve nodes (from edges list) ----
#' nodes_list(edges)

nodes_list <- function(x) {
  
  ## Check x argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.data.frame(x) && !is.character(x)) {
    stop("Argument 'x' must be either a data.frame (edges list) or a ", 
         "character (vector of nodes)", call. = FALSE)
  }
  
  if (is.data.frame(x)) {
    
    if (!("from" %in% colnames(x))) {
      stop("The column 'from' is absent from the edges data.frame", 
           call. = FALSE)
    }
    
    if (!("to" %in% colnames(x))) {
      stop("The column 'to' is absent from the edges data.frame", 
           call. = FALSE)
    }
    
  } else {
    
    if (!is.null(dim(x))) {
      
      stop("Argument 'x' must be either a data.frame (edges list) or a ", 
           "character (vector of nodes)", call. = FALSE)
    }
  }
  
  
  ## Extract nodes ----
  
  if (is.data.frame(x)) nodes <- c(x$"from", x$"to") else nodes <- x
  
  
  ## Check for NA ----
  
  if (any(NA %in% nodes)) {
    stop("Argument 'x' cannot contain NA (unidentified nodes)", call. = FALSE)
  }
  
  
  ## Get unique labels and order ----
  
  data.frame("node" = sort(unique(as.character(nodes))))
}
