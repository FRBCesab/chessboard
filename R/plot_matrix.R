#' Plot a connectivity or a nodes-by-edges matrix
#' 
#' @description
#' Plots an adjacency or a nodes-by-edges matrix.
#' 
#' @param x a `matrix` object. An adjacency or nodes-by-edges matrix.
#' 
#' @param title a `character` of length 1. The caption of the figure.
#'
#' @return A `ggplot2` object.
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
#' adour_nodes <- nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood (undirected network) ----
#' adour_edges <- edges_list(adour_nodes, directed = TRUE)
#' 
#' # Get adjacency matrix ----
#' adour_adj_matrix <- adjacency_matrix(adour_edges)
#'
#' # Visualize matrix ----
#' plot_matrix(adour_adj_matrix, title = "Connectivity matrix")
#'
#' # Nodes by edges matrix ----
#' adour_edges_mat <- nodes_by_edges_matrix(adour_edges)
#'
#' # Visualize matrix ----
#' plot_matrix(adour_edges_mat$"se.mat", title = "Nodes-by-edges matrix")

plot_matrix <- function(x, title) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.matrix(x)) {
    stop("Argument 'x' must be a matrix (adjacency matrix)", call. = FALSE)
  }
  
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric matrix (adjacency matrix)", 
         call. = FALSE)
  }
  
  
  ## Check title argument ----
  
  if (missing(title)) title <- ""
  
  if (!is.character(title) || length(title) != 1) {
    stop("Argument 'title' must be a character of length 1", call. = FALSE)
  }
  
  
  ## Prepare to pivot ----
  
  x <- ifelse(is.na(x), 0, x)
  
  if (length(unique(as.vector(x))) > 2) {
    stop("This function only works on binary matrix", call. = FALSE)
  }
  
  x <- as.data.frame(x)
  x <- data.frame("from" = rownames(x), x)
  rownames(x) <- NULL
  colnames(x) <- gsub("\\.", "-", colnames(x))
  
  
  ## Pivot to longer format ----
  
  x <- tidyr::pivot_longer(x, cols = -1, names_to = "to", values_to = "edge")
  x <- as.data.frame(x)
  
  
  ## Order data ----
  
  nodes <- nodes_list(x$"from")
  nodes <- rev(nodes)
  edges <- nodes_list(x$"to")
  
  x$"edge" <- factor(x$"edge", levels = c(0, 1))
  x$"from" <- factor(x$"from", levels = nodes)
  x$"to"   <- factor(x$"to", levels = edges)
  
  ggplot2::ggplot(data = x) +
    ggplot2::geom_tile(ggplot2::aes(.data$to, .data$from, fill = .data$edge), 
                       color = "lightgray") +
    ggplot2::scale_fill_manual(values = c(`0` = "transparent", 
                                          `1` = "black")) +
    ggplot2::coord_fixed() +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme_classic() +
    ggplot2::labs(caption = title) +
    ggplot2::theme(legend.position = "none",
                   axis.line       = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.text       = ggplot2::element_text(family = "serif"),
                   # axis.text.x     = ggplot2::element_text(angle = 45, 
                   #                                         vjust = 1, 
                   #                                         hjust = 0),
                   plot.caption    = ggplot2::element_text(family = "serif", 
                                                           size   = 11))
}
