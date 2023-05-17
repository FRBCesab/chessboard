# Data for tests ----

sites <- expand.grid("transect" = 1:5, 
                     "quadrat"  = 1:5)

nodes <- create_nodes_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")


# Tests for success ----

test_that("create_edges_list() - Tests for success (pawn)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "pawn",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 20L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "pawn",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 35L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "pawn",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 20L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "pawn",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 40L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "pawn",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 45L)
})


test_that("create_edges_list() - Tests for success (fool)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "fool",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 20L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "fool",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 35L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "fool",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 20L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "fool",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 40L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "fool",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 45L)
})


test_that("create_edges_list() - Tests for success (rook)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "rook",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 60L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "rook",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 105L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "rook",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 60L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "rook",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 80L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "rook",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 85L)
})


test_that("create_edges_list() - Tests for success (bishop)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 32L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 50L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 32L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 64L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 57L)
})


test_that("create_edges_list() - Tests for success (bishop_left)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_left",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 16L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_left",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 25L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_left",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 16L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_left",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 32L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_left",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 41L)
})


test_that("create_edges_list() - Tests for success (bishop_right)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_right",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 16L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_right",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 25L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_right",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 16L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_right",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 32L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "bishop_right",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 41L)
})


test_that("create_edges_list() - Tests for success (knight)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 0L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 48L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 48L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight",
                               degree   = 2,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 96L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 48L)
})


test_that("create_edges_list() - Tests for success (knight_left)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_left",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 0L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_left",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_left",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_left",
                               degree   = 2,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 48L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_left",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
})


test_that("create_edges_list() - Tests for success (knight_right)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_right",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 0L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_right",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_right",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_right",
                               degree   = 2,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 48L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "knight_right",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 24L)
})


test_that("create_edges_list() - Tests for success (queen)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "queen",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 92L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "queen",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 155L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "queen",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 92L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "queen",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 144L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "queen",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 117L)
})


test_that("create_edges_list() - Tests for success (wizard)", {
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "wizard",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 92L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "wizard",
                               degree   = 2,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 203L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "wizard",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = TRUE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 92L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "wizard",
                               degree   = 1,
                               directed = FALSE,
                               reverse  = FALSE, 
                               self     = FALSE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 144L)
  
  expect_silent({
    edges <- create_edges_list(nodes,
                               method   = "wizard",
                               degree   = 1,
                               directed = TRUE,
                               reverse  = FALSE, 
                               self     = TRUE)
  })
  
  expect_equal(class(edges), "data.frame")
  expect_equal(ncol(edges), 2L)
  expect_equal(nrow(edges), 117L)
})
