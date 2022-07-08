# Data for tests ----

nodes     <- c("S01", "S02", "S11", "S21")
points_df <- data.frame("site" = nodes, "x" = 1:4, "y" = 101:104)
points_sf <- sf::st_as_sf(points_df, coords = 2:3)

points_sf_mltpt <- sf::st_cast(points_sf, "MULTIPOINT")
points_sf_lnstr <- sf::st_cast(points_sf, "LINESTRING")
points_sf_lnstr <- rbind(points_sf, points_sf_lnstr)

points_sp <- points_df
sp::coordinates(points_sp) <- 2:3


# Test for errors ----

test_that("distance_euclidean() - Tests for wrong inputs", {
  
  ## Test sites argument ----
  
  expect_error(distance_euclidean(), 
               "Argument 'sites' (spatial layer of sites) is required", 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_df), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_sp), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_sf[1, ]), 
               "Argument 'sites' should have at least two rows (sites)", 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_sf[ , -1]), 
               paste0("Argument 'sites' should have at least two columns: ", 
                      "site label and geometry"), 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_sf_lnstr), 
               paste0("Argument 'sites' (spatial layer of sites) cannot ", 
                      "contain different geometries"), 
               fixed = TRUE)
  
  expect_error(distance_euclidean(points_sf_mltpt), 
               "Sites geometry must be of type POINT", 
               fixed = TRUE)
  
  expect_error(distance_euclidean(rbind(points_sf, points_sf)), 
               "The argument 'sites' cannot contain duplicates", 
               fixed = TRUE)
  
})


# Test for success ----

test_that("distance_euclidean() - Tests for good outputs", {
  
  expect_silent({
    dists <- distance_euclidean(points_sf)
  })
  
  expect_equal(class(dists), "data.frame")
  expect_equal(ncol(dists), 3L)
  expect_equal(nrow(dists), nrow(points_sf) ^ 2)
  expect_true(dists[1, 3] == 0L)
  expect_true(dists[2, 3] == dists[5, 3])
})

