#' Plot a connectivity or a nodes-by-edges matrix
#' 
#' @description
#' Plots an connectivity or a nodes-by-edges matrix.
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
#' library("chessboard")
#' 
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
#'                             package = "chessboard")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Select first location ----
#' #adour_sites <- adour_sites[adour_sites$"location" == 1, ]
#' 
#' # Create nodes labels ----
#' adour_nodes <- create_nodes_labels(data     = adour_sites, 
#'                                    location = "location", 
#'                                    transect = "transect", 
#'                                    quadrat = "quadrat")
#' 
#' # Find edges with 1 degree of neighborhood (queen method) ----
#' adour_edges <- create_edges_list(adour_nodes, method = "queen", 
#'                                  directed = FALSE)
#' 
#' # Get connectivity matrix ----
#' adour_con_matrix <- connectivity_matrix(adour_edges)
#'
#' # Visualize matrix ----
#' gg_matrix(adour_con_matrix, title = "Connectivity matrix") +
#'   ggplot2::theme(axis.text = ggplot2::element_text(size = 6))

gg_matrix <- function(x, title) {
  
  ## Check 'x' argument ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!is.matrix(x)) {
    stop("Argument 'x' must be a matrix (connectivity or nodes-by-edges ", 
         "matrix)", call. = FALSE)
  }
  
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric matrix (connectivity or ", 
         "nodes-by-edges matrix)", call. = FALSE)
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
  colnames(x) <- gsub("\\.", "-",  colnames(x))
  colnames(x) <- gsub("[A-Z]", "", colnames(x))
  
  
  ## Pivot to longer format ----
  
  x <- tidyr::pivot_longer(x, cols = -1, names_to = "to", values_to = "edge")
  x <- as.data.frame(x)
  
  
  ## Order data ----
  
  nodes <- get_sorted_nodes(x)
  # edges <- nodes_list(x$"to")
  
  x$"edge" <- factor(x$"edge", levels = c(0, 1))
  x$"from" <- factor(x$"from", levels = rev(nodes))
  x$"to"   <- factor(x$"to", levels = nodes)
  
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
                   axis.text.x     = ggplot2::element_text(angle = 90,
                                                           vjust = 1,
                                                           hjust = 0),
                   plot.caption    = ggplot2::element_text(family = "serif", 
                                                           size   = 11))
}
