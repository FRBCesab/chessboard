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

sites_sf_bad_0 <- sf::st_as_sf(sf::st_combine(sites_sf))
sites_sf_bad_0$"node"     <- "9-9"
sites_sf_bad_0$"location" <- "9"
sites_sf_bad_0$"transect" <- "9"
sites_sf_bad_0$"quadrat"  <- "9"
sites_sf_bad_0 <- sites_sf_bad_0[ , c(2:5, 1)]
sf::st_geometry(sites_sf_bad_0) <- "geometry"
sites_sf_bad_0 <- rbind(sites_sf, sites_sf_bad_0)

sites_sf_bad_1 <- sites_sf
sf::st_crs(sites_sf_bad_1) <- NA


# Tests for errors ----

test_that("distance_euclidean() - Tests for errors", {
  
  expect_error(distance_euclidean(),
               "Argument 'sites' (spatial layer of sites) is required",
               fixed = TRUE)
  
  expect_error(distance_euclidean(sites),
               "The object 'sites' must be an 'sf' object",
               fixed = TRUE)

  expect_error(distance_euclidean(sites_sf[1, ]),
               "Argument 'sites' should have at least two rows (sites)",
               fixed = TRUE)
  
  expect_error(distance_euclidean(sites_sf[ , 5]),
               paste0("Argument 'sites' should have at least two columns: ", 
                      "node label and geometry"),
               fixed = TRUE)
  
  expect_error(distance_euclidean(sites_sf[ , -1]),
               "The column 'node' is absent from the object 'site'",
               fixed = TRUE)
  
  expect_error(distance_euclidean(sites_sf_bad_0),
               paste0("Argument 'sites' (spatial layer of sites) cannot ", 
                      "contain different geometry types"),
               fixed = TRUE)
  
  expect_error(distance_euclidean(rbind(sites_sf_bad_0[6, ], 
                                        sites_sf_bad_0[6, ])),
               "Sites geometry must be of type POINT",
               fixed = TRUE)
  
  expect_error(distance_euclidean(sites_sf_bad_1),
               "The 'sites' layer has not a valid CRS",
               fixed = TRUE)
  
  expect_error(distance_euclidean(rbind(sites_sf, sites_sf)),
               "The argument 'sites' cannot contain duplicated nodes",
               fixed = TRUE)
})


# Tests for success ----

test_that("distance_euclidean() - Tests for success", {
  
  expect_silent({ check <- distance_euclidean(sites_sf) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(nrow(check), nrow(sites_sf)^2)
  expect_equal(ncol(check), 3L)
  
  expect_equal(colnames(check), c("from", "to", "weight"))
  
  expect_equal(min(check$"weight"), 0L)
  
  expect_equal(check[2, "from"], "1-1")
  expect_equal(check[2, "to"], "1-2")
  expect_equal(check[2, "weight"], 2500.469, tolerance = 0.0001)
})
