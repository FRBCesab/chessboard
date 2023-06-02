# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")

sites <- read.csv(path_to_file)
sites <- sites[1:5, ]

sites <- create_node_labels(sites, 
                             location = "location", 
                             transect = "transect", 
                             quadrat  = "quadrat")

sites_sf <- sf::st_as_sf(sites, coords = c("longitude", "latitude"),
                         crs = "epsg:2154")

dists <- distance_euclidean(sites_sf)


# Test for errors ----

test_that("edges_weights_matrix() - Tests for errors", {
  
  
  expect_error(edges_weights_matrix(),
               "Argument 'distances' is required",
               fixed = TRUE)
  
  expect_error(edges_weights_matrix(letters),
               "Argument 'distances' must be a data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_matrix(data.frame("node" = letters)),
               "The column 'from' is absent from the distances data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_matrix(data.frame("from" = letters, 
                                               "node" = letters)),
               "The column 'to' is absent from the distances data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_matrix(data.frame("from" = letters, 
                                               "to" = letters)),
               "The column 'weight' is absent from the distances data.frame",
               fixed = TRUE)
  
  expect_error(edges_weights_matrix(dists[0, ]),
               "Argument 'distances' must have at least one row",
               fixed = TRUE)
})


# Test for success ----

test_that("edges_weights_matrix() - Tests for success", {
  
  expect_silent({ check <- edges_weights_matrix(dists) })
  
  expect_equal(class(check), c("matrix", "array"))
  expect_equal(colnames(check), rownames(check))
  expect_equal(check[1, 1], 0L)
  expect_equal(round(check[1, 2]), 2500)
  expect_equal(round(check[2, 1]), 2500)
  
  expect_silent({ check <- edges_weights_matrix(dists, lower = FALSE) })
  
  expect_equal(class(check), c("matrix", "array"))
  expect_equal(colnames(check), rownames(check))
  expect_equal(check[1, 1], 0L)
  expect_equal(round(check[1, 2]), 2500)
  expect_true(is.na(check[2, 1]))
  
  expect_silent({ check <- edges_weights_matrix(dists, upper = FALSE) })
  
  expect_equal(class(check), c("matrix", "array"))
  expect_equal(colnames(check), rownames(check))
  expect_equal(check[1, 1], 0L)
  expect_true(is.na(check[1, 2]))
  expect_equal(round(check[2, 1]), 2500)
  
  expect_silent({ check <- edges_weights_matrix(dists, diag = FALSE) })
  
  expect_equal(class(check), c("matrix", "array"))
  expect_equal(colnames(check), rownames(check))
  expect_true(is.na(check[1, 1]))
  expect_equal(round(check[1, 2]), 2500)
  expect_equal(round(check[2, 1]), 2500)
})
