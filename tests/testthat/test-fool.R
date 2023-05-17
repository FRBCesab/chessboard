# Data for tests ----

sites <- expand.grid("transect" = 1:5, 
                     "quadrat"  = 1:5)

nodes <- create_nodes_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")

sites_qu <- expand.grid("transect" = 3, 
                        "quadrat"  = 1:5)

nodes_qu <- create_nodes_labels(data     = sites_qu, 
                                transect = "transect", 
                                quadrat  = "quadrat")

sites_tr <- expand.grid("transect" = 1:5, 
                        "quadrat"  = 3)

nodes_tr <- create_nodes_labels(data     = sites_tr, 
                                transect = "transect", 
                                quadrat  = "quadrat")

# Tests for errors ----

test_that("fool() - Tests for errors", {
  
  expect_error(fool(),
               paste0("Argument 'nodes' is required (output of the function ", 
                      "create_nodes_labels())"),
               fixed = TRUE)
  
  expect_error(fool(nodes_qu),
               paste0("The fool movement is not designed to work through ", 
                      "quadrats only. Please use pawn() instead."),
               fixed = TRUE)
})


# Tests for success ----

test_that("fool() - Tests for success (2D network)", {
  
  expect_silent({
    neighbors <- fool(nodes,
                      focus    = "3-3",
                      degree   = 1,
                      directed = TRUE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 1L)
  expect_equal(neighbors[1, "node"], "4-3")
  
  
  expect_silent({
    neighbors <- fool(nodes,
                      focus    = "3-3",
                      degree   = 2,
                      directed = TRUE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 2L)
  expect_equal(neighbors[ , "node"], c("4-3", "5-3"))
  
  
  expect_silent({
    neighbors <- fool(nodes,
                      focus    = "3-3",
                      degree   = 1,
                      directed = TRUE,
                      reverse  = TRUE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 1L)
  expect_equal(neighbors[1, "node"], "2-3")
  
  
  expect_silent({
    neighbors <- fool(nodes,
                      focus    = "3-3",
                      degree   = 1,
                      directed = FALSE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 2L)
  expect_equal(neighbors[ , "node"], c("2-3", "4-3"))
  
  
  expect_silent({
    neighbors <- fool(nodes,
                      focus    = "3-3",
                      degree   = 1,
                      directed = FALSE,
                      reverse  = FALSE, 
                      self     = TRUE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 3L)
  expect_equal(neighbors[ , "node"], c("2-3", "3-3", "4-3"))
})


test_that("fool() - Tests for success (1D network)", {
  
  expect_silent({
    neighbors <- fool(nodes_tr,
                      focus    = "3-3",
                      degree   = 1,
                      directed = TRUE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 1L)
  expect_equal(neighbors[1, "node"], "4-3")
  
  
  expect_silent({
    neighbors <- fool(nodes_tr,
                      focus    = "3-3",
                      degree   = 2,
                      directed = TRUE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 2L)
  expect_equal(neighbors[ , "node"], c("4-3", "5-3"))
  
  
  expect_silent({
    neighbors <- fool(nodes_tr,
                      focus    = "3-3",
                      degree   = 1,
                      directed = TRUE,
                      reverse  = TRUE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 1L)
  expect_equal(neighbors[1, "node"], "2-3")
  
  
  expect_silent({
    neighbors <- fool(nodes_tr,
                      focus    = "3-3",
                      degree   = 1,
                      directed = FALSE,
                      reverse  = FALSE, 
                      self     = FALSE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 2L)
  expect_equal(neighbors[ , "node"], c("2-3", "4-3"))
  
  
  expect_silent({
    neighbors <- fool(nodes_tr,
                      focus    = "3-3",
                      degree   = 1,
                      directed = FALSE,
                      reverse  = FALSE, 
                      self     = TRUE)
  })
  
  expect_equal(class(neighbors), "data.frame")
  expect_equal(ncol(neighbors), 4L)
  expect_equal(nrow(neighbors), 3L)
  expect_equal(neighbors[ , "node"], c("2-3", "3-3", "4-3"))
})
