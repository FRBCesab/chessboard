# Data for tests ----

nodes     <- c("S01", "S02", "S11", "S21")
points_df <- data.frame("site" = nodes, "x" = 1:4, "y" = 101:104)
points_sf <- sf::st_as_sf(points_df, coords = 2:3)

points_sf_mltpt <- sf::st_cast(points_sf, "MULTIPOINT")
points_sf_lnstr <- sf::st_cast(points_sf, "LINESTRING")
points_sf_lnstr <- rbind(points_sf, points_sf_lnstr)

points_sp <- points_df
sp::coordinates(points_sp) <- 2:3

mat <- c(0.761131407374101, 0.960308003597122, 1.15948459982014, 
         1.37459532374101, 1.74108026079137, 2.01196043165468, 
         2.24300528327338, 2.51388545413669, 3.04767873201439, 
         4.21087005395683, 101.154021470324, 101.472704024281, 
         101.759518322842, 102.030398493705, 102.349081047662, 
         102.651829473921, 102.922709644784, 103.145787432554, 
         103.583975944245, 104.42848471223)
mat <- matrix(mat, byrow = FALSE, ncol = 2)
river <- sf::st_as_sf(sf::st_as_sfc(list(sf::st_linestring(mat))))


# Test for errors ----

test_that("distance_along() - Tests for wrong inputs", {
  
  ## Test sites argument ----
  
  expect_error(distance_along(), 
               "Argument 'sites' (spatial layer of sites) is required", 
               fixed = TRUE)
  
  expect_error(distance_along(points_df), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(distance_along(points_sp), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf[1, ]), 
               "Argument 'sites' should have at least two rows (sites)", 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf[ , -1]), 
               paste0("Argument 'sites' should have at least two columns: ", 
                      "site label and geometry"), 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf_lnstr), 
               paste0("Argument 'sites' (spatial layer of sites) cannot ", 
                      "contain different geometries"), 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf_mltpt), 
               "Sites geometry must be of type POINT", 
               fixed = TRUE)
  
  expect_error(distance_along(rbind(points_sf, points_sf)), 
               "The argument 'sites' cannot contain duplicates", 
               fixed = TRUE)
  
  
  ## Test along argument ----
  
  expect_error(distance_along(points_sf), 
               paste0("Argument 'along' (spatial layer of linear shape) is ", 
                      "required"), 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf, points_sp), 
               "The object 'along' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf, points_sf[1, ]), 
               "Linear shape geometry must be of type LINESTRING", 
               fixed = TRUE)
  
  expect_error(distance_along(points_sf, rbind(river, river)), 
               "Argument 'along' (linear shape) should have exactly one row", 
               fixed = TRUE)
  
  
  ## Test type argument ----
  
  expect_error(distance_along(points_sf, river, type = 1), 
               "Argument 'type' must either 'regular' or 'random'", 
               fixed = TRUE)
  
  
  ## Test density argument ----
  # ...
})


# Test for success ----

test_that("distance_along() - Tests for good outputs", {
  
  expect_silent({
    dists <- distance_along(points_sf, river, density = 10, mc.cores = 1)
  })
  
  expect_equal(class(dists), "data.frame")
  expect_equal(ncol(dists), 3L)
  expect_equal(nrow(dists), nrow(points_sf) ^ 2)
  expect_true(dists[1, 3] == 0L)
  expect_true(dists[2, 3] == dists[5, 3])
})

