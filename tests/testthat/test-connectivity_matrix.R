# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")
adour_sites  <- read.csv(path_to_file)
adour_sites <- adour_sites[adour_sites$"location" == 1, ]
adour_nodes <- create_node_labels(data     = adour_sites, 
                                   location = "location", 
                                   transect = "transect", 
                                   quadrat  = "quadrat")
adour_edges <- create_edges_list(adour_nodes, method = "pawn", 
                                 directed = TRUE)

test_that("connectivity_matrix() - Tests for errors", {
  
  expect_error(connectivity_matrix(),
               paste0("Argument 'edges' is required (output of the function ", 
                      "create_edges_list())"),
               fixed = TRUE)
  
  expect_error(connectivity_matrix(adour_nodes),
               paste0("The column 'from' is absent from the 'edges' ", 
                      "data.frame (output of the function ", 
                      "create_edges_list())"),
               fixed = TRUE)
})

test_that("connectivity_matrix() - Tests for success", {
  
  ## Default settings ----
  
  expect_silent({ x <- connectivity_matrix(adour_edges) })
  
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(nrow(x), ncol(x))
  expect_equal(sum(x), nrow(adour_edges))
  expect_equal(sum(is.na(x)), 0L)
  
  
  ## Lower is false ----
  
  expect_silent({ x <- connectivity_matrix(adour_edges, lower = FALSE) })
  
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(nrow(x), ncol(x))
  expect_equal(sum(x), nrow(adour_edges))
  expect_equal(sum(is.na(x)), 0L)
  
  
  ## Upper is false ----
  
  expect_silent({ x <- connectivity_matrix(adour_edges, upper = FALSE) })
  
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(nrow(x), ncol(x))
  expect_equal(sum(x), 0L)
  expect_equal(sum(is.na(x)), 0L)
  
  
  ## Diag is false ----
  
  expect_silent({ x <- connectivity_matrix(adour_edges, diag = FALSE) })
  
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(nrow(x), ncol(x))
  expect_equal(sum(x), nrow(adour_edges))
  expect_equal(sum(is.na(x)), 0L)
  
  
  ## na_to_zero is false ----
  
  expect_silent({ x <- connectivity_matrix(adour_edges, na_to_zero = FALSE) })
  
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(nrow(x), ncol(x))
  expect_equal(sum(x, na.rm = TRUE), nrow(adour_edges))
  expect_equal(sum(is.na(x)), length(x) - sum(x, na.rm = TRUE))
})
