#' Create unique nodes labels 
#'
#' @description 
#' Creates unique nodes (sampling units) labels in (un)directed two-dimensional
#' (spatial) networks (i.e. regular grid). 
#' 
#' It's important to note that, even the package `bridge` is designed to deal
#' with spatial networks, it does not deal explicitly with spatial coordinates.
#' **But nodes labels matters!**
#' 
#'
#' **Convention**
#' 
#' The package `bridge` requires that the sampling has two dimensions: one 
#' from bottom to top (called **quadrats**), and one from left to right (called
#' **transects**). If the sampling has been conducted along one single 
#' dimension (**transects** or **quadrats**), this function will create a 
#' fictitious label for the missing dimension.
#' 
#' In other words, the package `bridge` can work with sampling designs such as 
#' regular grids (two dimensions), transects (one dimension), and quadrats 
#' (one dimension).
#' 
#' 
#' **Nodes labeling**
#' 
#' The nodes labels will be of the form: `1-2`, where `1` is the identifier of 
#' the transect (created by the function is missing), and `2`, the identifier 
#' of the quadrat (created by the function is missing).
#' 
#'
#' @param transects a `numeric` vector. The identifier of the transects. If 
#'   missing, a unique transect will be created and named `1` 
#'   (for the purpose of the package).
#' 
#' @param quadrats a `numeric` vector. The identifier of the quadrats. If 
#'   missing, a unique quadrat will be created and named `1`.
#'   (for the purpose of the package).
#'
#' @return A `data.frame` with the three following columns:
#' - `node`, the nodes label (created by the function)
#' - `transect`, the transects label (created by the function or provided)
#' - `quadrat`, the quadrats label (created by the function or provided)
#' 
#' @export
#' 
#' @examples 
#' library("bridge")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' sites_infos
#' 
#' nodes <- create_nodes_labels(transects = sites_infos$"transect", 
#'                              quadrats  = sites_infos$"quadrat")
#' nodes
#' 
#' gg_chessboard(nodes)
#' 
#' # One-dimensional sampling (only transects) ----
#' sites_infos <- 1:5
#' 
#' nodes <- create_nodes_labels(transects = sites_infos)
#' nodes
#' 
#' gg_chessboard(nodes)
#' 
#' # One-dimensional sampling (only quadrats) ----
#' sites_infos <- 1:5
#' 
#' nodes <- create_nodes_labels(quadrats = sites_infos)
#' nodes
#' 
#' gg_chessboard(nodes)

create_nodes_labels <- function(transects, quadrats) {
  
  if (missing(transects)) transects <- NULL
  if (missing(quadrats))  quadrats  <- NULL
  
  if (is.null(transects) && is.null(quadrats)) {
    stop("Please provide at least either 'transects' or 'quadrats'", 
         call. = FALSE)
  }
  
  if (!is.null(transects)) {
    if (!is.numeric(transects)) {
      stop("Argument 'transects' must be a numeric", call. = FALSE)
    }
  }
  
  if (!is.null(quadrats)) {
    if (!is.numeric(quadrats)) {
      stop("Argument 'quadrats' must be a numeric", call. = FALSE)
    }
  }
  
  if (is.null(transects)) {

    if (any(duplicated(quadrats))) {
      stop("As argument 'transects' is not provided, 'quadrats' cannot ", 
           "contain duplicates", call. = FALSE)
    }

    transects <- rep(1, length(quadrats))
  }

  if (is.null(quadrats)) {

    if (any(duplicated(transects))) {
      stop("As argument 'quadrats' is not provided, 'transects' cannot ", 
           "contain duplicates", call. = FALSE)
    }

    quadrats <- rep(1, length(transects))
  }


  if (length(transects) != length(quadrats)) {
    stop("Arguments 'transects' and 'quadrats' must have the same length", 
         call. = FALSE)
  }

  if (any(is.na(transects))) {
    stop("Argument 'transects' cannot contain NA", call. = FALSE)
  }
  
  if (any(is.na(quadrats))) {
    stop("Argument 'quadrats' cannot contain NA", call. = FALSE)
  }

  nodes <- data.frame("transect" = transects, "quadrat"  = quadrats)
  nodes <- nodes[with(nodes, order(transect, quadrat)), ]
  rownames(nodes) <- NULL

  keys <- paste0(nodes$"transect", "-", nodes$"quadrat")
  
  if (any(duplicated(keys))) {
    stop("Nodes labels cannot contain duplicates", call. = FALSE)
  }
  
  data.frame("node" = keys, nodes)
}
