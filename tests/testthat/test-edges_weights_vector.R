# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")

sites <- read.csv(path_to_file)
sites <- sites[1:5, ]

sites <- create_nodes_labels(sites, 
                             location = "location", 
                             transect = "transect", 
                             quadrat  = "quadrat")

edges <- create_edges_list(sites, method = "pawn", directed = TRUE)
nodes_edges <- nodes_by_edges_matrix(edges)

nodes_edges_bad <- nodes_edges
names(nodes_edges_bad) <- c("xxx", "yyy")

sites_sf <- sf::st_as_sf(sites, coords = c("longitude", "latitude"),
                         crs = "epsg:2154")

dists <- distance_euclidean(sites_sf)


# Test for errors ----

test_that("edges_weights_vector() - Tests for errors", {
  
  expect_error(edges_weights_vector(),
               "Argument 'x' is required",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(letters),
               "Argument 'x' must be a list (nodes-by-edges matrix)",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(list(1, 2, 3)),
               paste0("Argument 'x' must be a list (nodes-by-edges matrix) of ",
                      "length 2"),
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges_bad),
               paste0("Argument 'x' must be a nodes-by-edges matrix as ", 
                      "returned by nodes_by_edges_matrix() or ", 
                      "adespatial::aem.build.binary()"),
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges),
               "Argument 'y' is required",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges, edges$"from"),
               "Argument 'y' must be a data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges, 
                                    data.frame("node" = edges$"from")),
               "The column 'from' is absent from the y data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges, 
                                    data.frame("from" = edges$"from", 
                                               "node" = edges$"from")),
               "The column 'to' is absent from the y data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges, 
                                    data.frame("from" = edges$"from",
                                               "to" = edges$"from")),
               "The column 'weight' is absent from the y data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_vector(nodes_edges, dists[0, ]),
               "Argument 'y' must have at least one row",
               fixed = TRUE)
})


# Test for success ----

test_that("edges_weights_vector() - Tests for success", {
  
  expect_silent({ check <- edges_weights_vector(nodes_edges, dists) })
  
  expect_equal(class(check), "numeric")
  expect_equal(names(check), c("E-1", "E-2", "E-3", "E-4", "E-5"))
})
