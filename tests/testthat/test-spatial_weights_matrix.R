# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")

sites <- read.csv(path_to_file)
sites <- sites[1:5, ]

sites <- create_nodes_labels(sites, 
                             location = "location", 
                             transect = "transect", 
                             quadrat  = "quadrat")

edges   <- create_edges_list(sites, method = "pawn", directed = FALSE)
mat_adj <- connectivity_matrix(edges)

sites_sf <- sf::st_as_sf(sites, coords = c("longitude", "latitude"),
                         crs = "epsg:2154")

dists <- distance_euclidean(sites_sf)
weights <- edges_weights_matrix(dists)


mat_bad_1 <- matrix(data = rep(c(0, 1, 0), 3), 
                    nrow = 3, 
                    ncol = 3)

colnames(mat_bad_1) <- c("1-1", "1-2", "1-3")
rownames(mat_bad_1) <- c("1-1", "1-2", "X-X")

mat_bad_2 <- matrix(data = rep(0, 9), 
                    nrow = 3, 
                    ncol = 3)

colnames(mat_bad_2) <- c("1-1", "1-2", "1-3")
rownames(mat_bad_2) <- c("1-1", "1-2", "1-3")

bad_1 <- matrix(1:4, ncol = 2)
colnames(bad_1) <- c("Z1", "Z2")
rownames(bad_1) <- c("Z1", "Z2")

bad_2 <- matrix(1:9, ncol = 3)
colnames(bad_2) <- c("Z1", "Z2", "Z3")
rownames(bad_2) <- c("Z1", "Z2", "Z3")

bad_3 <- matrix(1:9, ncol = 3)
colnames(bad_3) <- c("Z1", "Z2", "ZZ")
rownames(bad_3) <- c("Z1", "Z2", "ZZ")


# Test for errors ----

test_that("spatial_weights_matrix() - Tests for errors", {
  
  expect_error(spatial_weights_matrix(),
               "Argument 'x' is required",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(letters),
               "Argument 'x' must be a matrix (adjacency matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(data.frame(letters)),
               "Argument 'x' must be a matrix (adjacency matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(as.matrix(letters)),
               "Argument 'x' must be a numeric matrix (adjacency matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(as.matrix(1:10)),
               paste0("Number of rows of 'x' must be equal to number of ", 
                      "columns (adjacency matrix)"),
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(matrix(1:9, ncol = 3)),
               "Row names of 'x' must contain nodes labels",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_bad_1),
               "Row names and column names of 'x' must be equal",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_bad_2),
               "Argument 'x' contains no edge",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj),
               "Argument 'y' is required",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, letters),
               "Argument 'y' must be a matrix (edges weight matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, data.frame(letters)),
               "Argument 'y' must be a matrix (edges weight matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, as.matrix(letters)),
               "Argument 'y' must be a numeric matrix (edges weight matrix)",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, as.matrix(1:10)),
               paste0("Number of rows of 'y' must be equal to number of ", 
               "columns (edges weight matrix)"),
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, matrix(1:9, ncol = 3)),
               "Row names of 'y' must contain nodes labels",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, mat_bad_1),
               "Row names and column names of 'y' must be equal",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(mat_adj, mat_bad_2),
               "Argument 'y' contains no weight",
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(bad_1, bad_2),
               paste0("Number of nodes is not equal between 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"),
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(bad_2, bad_3),
               paste0("Nodes names do not match between 'x' (adjacency ", 
                      "matrix) and 'y' (edges weight matrix)"),
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(bad_2, bad_2[3:1, ]),
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"),
               fixed = TRUE)
  
  expect_error(spatial_weights_matrix(bad_2, bad_2[ , 3:1]),
               paste0("Nodes names are not in the same order in 'x' ", 
                      "(adjacency matrix) and 'y' (edges weight matrix)"),
               fixed = TRUE)
})


# Tests for success ----

test_that("spatial_weights_matrix() - Tests for errors", {
  
  expect_silent({ check <- spatial_weights_matrix(mat_adj, weights) })
  
  expect_equal(class(check), c("matrix", "array"))
  expect_equal(rownames(check), colnames(check))
})
