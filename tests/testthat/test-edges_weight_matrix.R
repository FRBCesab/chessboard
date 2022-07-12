# Data for tests ----

nodes     <- c("S01", "S02", "S11", "S21")
points_df <- data.frame("site" = nodes, "x" = 101:104, "y" = 1:4)
points_sf <- sf::st_as_sf(points_df, coords = 2:3)
sf::st_crs(points_sf) <- "epsg:4326"

dists <- distance_euclidean(points_sf)
dists_from   <- dists[ , -1]
dists_to     <- dists[ , -2]
dists_weight <- dists[ , -3]


# Test for errors ----

test_that("edges_weight_matrix() - Tests for wrong inputs", {
  
  ## Test edges argument ----
  
  expect_error(edges_weight_matrix(), 
               "Argument 'distances' is required", 
               fixed = TRUE)
  
  expect_error(edges_weight_matrix(nodes), 
               "Argument 'distances' must be a data.frame", 
               fixed = TRUE)
  
  expect_error(edges_weight_matrix(dists_from), 
               "The column 'from' is absent from the distances data.frame", 
               fixed = TRUE)
  
  expect_error(edges_weight_matrix(dists_to), 
               "The column 'to' is absent from the distances data.frame", 
               fixed = TRUE)
  
  expect_error(edges_weight_matrix(dists_weight), 
               "The column 'weight' is absent from the distances data.frame", 
               fixed = TRUE)

  expect_error(edges_weight_matrix(dists[0, ]), 
               "Argument 'distances' must have at least one row", 
               fixed = TRUE)
})


# Test for success ----

test_that("edges_weight_matrix() - Tests for good outputs", {
  
  ## Default settings (not all edges) ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists)
  })
  
  expect_equal(class(edge_weight_mat), c("matrix", "array"))
  expect_equal(length(edge_weight_mat), nrow(dists))
  expect_equal(nrow(edge_weight_mat), ncol(edge_weight_mat))
  expect_true(is.numeric(edge_weight_mat))
  expect_true(sum(edge_weight_mat) == sum(dists$"weight"))
  expect_equal(edge_weight_mat[2, 1], edge_weight_mat[1, 2])
  expect_equal(edge_weight_mat[2, 2], 0L)
  
  
  ## Upper is FALSE ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists, upper = FALSE)
  })
  
  expect_equal(edge_weight_mat[2, 1], 157225.7, tolerance = 0.0001)
  expect_true(is.na(edge_weight_mat[1, 2]))
  
  
  ## Lower is FALSE ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists, lower = FALSE)
  })
  
  expect_equal(edge_weight_mat[1, 2], 157225.7, tolerance = 0.0001)
  expect_true(is.na(edge_weight_mat[2, 1]))
  
  
  ## Upper and Lower are FALSE ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists, upper = FALSE, lower = FALSE)
  })
  
  expect_true(is.na(edge_weight_mat[1, 2])) 
  expect_true(is.na(edge_weight_mat[2, 1])) 
  
  
  ## Diag is FALSE ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists, diag = FALSE)
  })
  
  expect_true(is.na(edge_weight_mat[1, 1])) 
  expect_true(is.na(edge_weight_mat[3, 3])) 
  
  
  ## Upper, Lower, and Diag are FALSE ----
  
  expect_silent({
    edge_weight_mat <- edges_weight_matrix(dists, diag = FALSE, upper = FALSE, 
                                           lower = FALSE)
  })
  
  expect_true(sum(is.na(edge_weight_mat)) == length(edge_weight_mat)) 
})
