# Data for tests ----

nodes     <- c("S01", "S02", "S11", "S21")
points_df <- data.frame("site" = nodes, "x" = 1:4, "y" = 101:104, "group" = 1)
points_sf <- sf::st_as_sf(points_df, coords = 2:3)
lnstrg_sf <- sf::st_linestring(sf::st_coordinates(points_sf[1:2, ]))
lnstrg_sf <- sf::st_as_sfc(list(lnstrg_sf))
lnstrg_sf <- sf::st_as_sf(lnstrg_sf)

lnstrg_sf_bad1 <- sf::st_cast(points_sf, "LINESTRING")
  
points_sf_mltpt <- sf::st_cast(points_sf, "MULTIPOINT")
points_sf_lnstr <- sf::st_cast(points_sf, "LINESTRING")
points_sf_lnstr <- rbind(points_sf, points_sf_lnstr)

points_sp <- points_df
sp::coordinates(points_sp) <- 2:3


# Test for errors ----

test_that("line_to_points() - Tests for wrong inputs", {
  
  ## Test points_sf argument ----
  
  expect_error(line_to_points(), 
               "Argument 'x' (spatial layer of linear shape) is required",
               fixed = TRUE)
  
  expect_error(line_to_points(points_df), 
               "The object 'x' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(line_to_points(lnstrg_sf_bad1), 
               "Argument 'x' should have exactly one row", 
               fixed = TRUE)
  
  expect_error(line_to_points(points_sf[1, ]), 
               "Linear shape geometry must be of type LINESTRING", 
               fixed = TRUE)
  
  
  ## Test type argument ----
  
  expect_error(line_to_points(lnstrg_sf, type = 1), 
               "Argument 'type' must either 'regular' or 'random'", 
               fixed = TRUE)
  
  
  ## Test density argument ----
  
  expect_error(line_to_points(lnstrg_sf, density = "all"), 
               "Argument 'density' must be a numeric of length 1", 
               fixed = TRUE)
  
  expect_error(line_to_points(lnstrg_sf, density = 2:3), 
               "Argument 'density' must be a numeric of length 1", 
               fixed = TRUE)
  
  expect_error(line_to_points(lnstrg_sf, density = -0.01), 
               "Argument 'density' must be > 0", 
               fixed = TRUE)
  
  expect_error(line_to_points(lnstrg_sf, density = 0), 
               "Argument 'density' must be > 0", 
               fixed = TRUE)
  
  expect_error(line_to_points(lnstrg_sf, density = 0.01), 
               paste0("Unable to sample points along the linear shape. ", 
                      "Please increase the value of 'density'"), 
               fixed = TRUE)
})


# Test for success ----

test_that("line_to_points() - Tests for good outputs", {
  
  expect_silent({
    ln_2_pt <- line_to_points(lnstrg_sf, density = 10)
  })
  
  expect_equal(class(ln_2_pt), c("sf", "data.frame"))
  expect_equal(sum(sf::st_is(ln_2_pt, "POINT")), nrow(ln_2_pt))
  expect_equal(ncol(ln_2_pt), 3L)
  expect_equal(nrow(ln_2_pt), 14L)
})
