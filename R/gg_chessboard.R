#' Plot a sampling as a chessboard
#'
#' @description
#' Plots a sampling as a chessboard of dimensions `t` x `q`, with 
#' `t`, the number of transects, and 
#' `q`, the number of quadrats.
#'
#' @param nodes a `data.frame` with the following three columns: 
#'   `node`, `transect`, and `quadrats`. See [create_nodes_labels()] for 
#'   further information.
#'
#' @param xlab a `character` of length 1. The title of the top axis. 
#'  Default is `'Transect'`.
#'
#' @param ylab a `character` of length 1. The title of the left axis.
#'  Default is `'Quadrat'`.
#'
#' @return A `ggplot2` object.
#'
#' @export
#'
#' @examples 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' 
#' nodes <- create_nodes_labels(transects = sites_infos$"transect", 
#'                              quadrats  = sites_infos$"quadrat")
#' 
#' gg_chessboard(nodes)
#'
#' # One-dimensional sampling (only transects) ----
#' sites_infos <- 1:5
#' 
#' nodes <- create_nodes_labels(transects = sites_infos)
#' 
#' gg_chessboard(nodes)
#' 
#' # One-dimensional sampling (only quadrats) ----
#' sites_infos <- 1:5
#' 
#' nodes <- create_nodes_labels(quadrats = sites_infos)
#' 
#' gg_chessboard(nodes)

gg_chessboard <- function(nodes, xlab = "Transect", ylab = "Quadrat") {
  
  
  ## Check argument 'nodes' ----
  
  if (missing(nodes)) {
    stop("Argument 'nodes' is required ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!is.data.frame(nodes)) {
    stop("Argument 'nodes' must be a data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!("transect" %in% colnames(nodes))) {
    stop("The column 'transect' is absent from the 'nodes' data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (!("quadrat" %in% colnames(nodes))) {
    stop("The column 'quadrat' is absent from the 'nodes' data.frame ", 
         "(output of the function create_nodes_labels())", call. = FALSE)
  }
  
  if (nrow(nodes) == 0) {
    stop("Argument 'nodes' must have at least two rows (nodes)", call. = FALSE)
  }
  
  if (!is.numeric(nodes$"transect")) {
    stop("The column 'transect' of the 'nodes' data.frame must be a numeric", 
         call. = FALSE)
  }
  
  if (!is.numeric(nodes$"quadrat")) {
    stop("The column 'quadrat' of the 'nodes' data.frame must be a numeric", 
         call. = FALSE)
  }


  ## Plot chessboard ----
  
  ggplot2::ggplot() + 
    
    ggplot2::geom_tile(data  = nodes, ggplot2::aes(.data$"transect", 
                                                   .data$"quadrat"), 
                       color = "lightgray", fill  = "white") + 
    
    ggplot2::geom_point(data  = nodes, ggplot2::aes(.data$"transect", 
                                                    .data$"quadrat"), 
                        shape = 19, size = 2, color = "lightgray") + 
    
    ggplot2::coord_fixed() +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    
    ggplot2::scale_x_discrete(
      position = "top", 
      limits   = as.factor(sort(unique(nodes$"transect")))) +
    
    ggplot2::scale_y_discrete(
      position = "left", 
      limits   = as.factor(sort(unique(nodes$"quadrat")))) +
    
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position  = "none",
      axis.line        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(
        family = "mono", size = 10),
      axis.title       = ggplot2::element_text(
        family = "mono", size = 12, face = "bold", 
        margin = ggplot2::margin(r = 10)),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = 10)),
      axis.title.y     = ggplot2::element_text(angle = 90))
}
