% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_matrix.R
\name{gg_matrix}
\alias{gg_matrix}
\title{Plot a connectivity or a nodes-by-edges matrix}
\usage{
gg_matrix(x, title)
}
\arguments{
\item{x}{a \code{matrix} object. An adjacency or nodes-by-edges matrix.}

\item{title}{a \code{character} of length 1. The caption of the figure.}
}
\value{
A \code{ggplot2} object.
}
\description{
Plots an connectivity or a nodes-by-edges matrix.
}
\examples{
library("chessboard")

# Import Adour sites ----
path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")
adour_sites  <- read.csv(path_to_file)

# Select first location ----
#adour_sites <- adour_sites[adour_sites$"location" == 1, ]

# Create node labels ----
adour_nodes <- create_node_labels(data     = adour_sites, 
                                  location = "location", 
                                  transect = "transect", 
                                  quadrat = "quadrat")

# Find edges with 1 degree of neighborhood (queen method) ----
adour_edges <- create_edge_list(adour_nodes, method = "queen", 
                                directed = FALSE)

# Get connectivity matrix ----
adour_con_matrix <- connectivity_matrix(adour_edges)

# Visualize matrix ----
gg_matrix(adour_con_matrix, title = "Connectivity matrix") +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 6))
}
