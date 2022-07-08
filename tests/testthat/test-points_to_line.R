# Data for tests ----

nodes     <- c("S01", "S02", "S11", "S21")
points_df <- data.frame("site" = nodes, "x" = 1:4, "y" = 101:104, "group" = 1)
points_sf <- sf::st_as_sf(points_df, coords = 2:3)

points_sf_mltpt <- sf::st_cast(points_sf, "MULTIPOINT")
points_sf_lnstr <- sf::st_cast(points_sf, "LINESTRING")
points_sf_lnstr <- rbind(points_sf, points_sf_lnstr)

points_sp <- points_df
sp::coordinates(points_sp) <- 2:3


# Test for errors ----

test_that("points_to_line() - Tests for wrong inputs", {

  ## Test points_sf argument ----
  
  expect_error(points_to_line(), 
               "Argument 'points_sf' is required", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_df), 
               "The object 'points_sf' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sp), 
               "The object 'points_sf' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf[1, ]), 
               "Argument 'points_sf' should have at least two rows", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf_lnstr), 
               "Argument 'points_sf' cannot contain different geometries", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf_mltpt), 
               "Geometry of 'points_sf' must be of type POINT", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf[ , -2]),
               "The column 'group' is absent from 'points_sf'",
               fixed = TRUE)
  
  
  ## Test from argument ----
  
  expect_error(points_to_line(points_sf), 
               "Argument 'from' is required", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, from = "A"), 
               "Argument 'from' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, from = 2:3), 
               "Argument 'from' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, from = 201), 
               paste0("Argument 'from' must be between 1 and number of rows ", 
                      "in 'points_sf'"), 
               fixed = TRUE)
  
  
  ## Test to argument ----
  
  expect_error(points_to_line(points_sf, 1), 
               "Argument 'to' is required", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, 1, to = "A"), 
               "Argument 'to' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, 1, to = 2:3), 
               "Argument 'to' must be an integer of length 1", 
               fixed = TRUE)
  
  expect_error(points_to_line(points_sf, 1, to = 201), 
               paste0("Argument 'to' must be between 1 and number of rows ", 
                      "in 'points_sf'"), 
               fixed = TRUE)

})


# Test for success ----

test_that("points_to_line() - Tests for good outputs", {

  expect_silent({
    pt_2_ln <- points_to_line(points_sf, from = 1, to = 2)
  })
  
  expect_equal(class(pt_2_ln), c("sf", "tbl_df", "tbl", "data.frame"))
  expect_equal(sum(sf::st_is(pt_2_ln, "LINESTRING")), nrow(pt_2_ln))
  expect_equal(ncol(pt_2_ln), 2L)
  expect_equal(nrow(pt_2_ln), 1L)

})
