# Data for success ----

tr_qu        <- expand.grid("transect" = 1:3, 
                            "quadrat"  = 1:5)

loc_tr_qu    <- expand.grid("location" = 1, 
                            "transect" = 1:3, 
                            "quadrat"  = 1:5)

tr_only      <- expand.grid("transect" = 1:5)

loc_tr_only  <- expand.grid("location" = 1, 
                            "transect" = 1:5)

qu_only      <- expand.grid("quadrat"  = 1:5)

loc_qu_only  <- expand.grid("location" = 1, 
                            "quadrat"  = 1:5)

tr_qu_w_col  <- expand.grid("transect"  = 1:3, 
                            "quadrat"   = 1:5)
tr_qu_w_col$"longitude" <- rnorm(15)
tr_qu_w_col$"latitude"  <- rnorm(15)


# Data for errors ----

tr_qu_bad_row <- tr_qu[0, ]

tr_qu_bad_tr  <- expand.grid("transect" = paste0("tr", 1:3), 
                             "quadrat"  = 1:5)

tr_qu_bad_qu  <- expand.grid("transect" = 1:3, 
                             "quadrat"  = paste0("qu", 1:5))

loc_tr_qu_bad_loc <- expand.grid("location" = "L1", 
                                 "transect" = 1:3, 
                                 "quadrat"  = 1:5)

tr_qu_na_tr <- tr_qu
tr_qu_na_tr[1, 1] <- NA

tr_qu_na_qu <- tr_qu
tr_qu_na_qu[1, 2] <- NA

tr_qu_gap_tr <- expand.grid("transect" = seq(2, 6, by = 2), 
                            "quadrat"  = 1:5)

tr_qu_gap_qu <- expand.grid("transect" = 1:3, 
                            "quadrat"  = c(1:4, 8))

tr_qu_dup <- rbind(tr_qu, tr_qu[1, ])


# Test for errors ----

test_that("create_nodes_labels() - Tests for wrong inputs", {
  
  # Argument 'data' ----
  
  expect_error(create_nodes_labels(),
               "The argument 'data' is required",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(data.matrix(tr_qu)),
               "The argument 'data' must be data.frame",
               fixed = TRUE)
  
  
  # Argument 'transect' or 'quadrat' ----
  
  expect_error(create_nodes_labels(tr_qu),
               "Please provide at least either 'transect' or 'quadrat'",
               fixed = TRUE)
  
  
  # Argument 'location' ----
  
  expect_error(create_nodes_labels(loc_tr_qu, 
                                   location = loc_tr_qu$"location",
                                   transect = "transect"),
               paste0("Argument 'location' must be a character of length 1 ", 
                      "(column name of the locations)"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(loc_tr_qu, 
                                   location = "locationS",
                                   transect = "transect"),
               "The column 'locationS' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(loc_tr_qu_bad_loc, 
                                   location = "location",
                                   transect = "transect"),
               "The column 'location' must be a numeric",
               fixed = TRUE)
  
  
  # Argument 'transect' ----
  
  expect_error(create_nodes_labels(tr_qu, transect = tr_qu$"transect"),
               paste0("Argument 'transect' must be a character of length 1 ", 
                      "(column name of the transects)"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu, transect = "transectS"),
               "The column 'transectS' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_bad_tr, transect = "transect"),
               "The column 'transect' must be a numeric",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu, transect = NULL, quadrat = "quadrat"),
               paste0("As argument 'transect' is not provided, the column ", 
                      "'quadrat' cannot contain duplicated value"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_na_tr, 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               "The column 'transect' cannot contain NA",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_gap_tr, 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               paste0("Labels of transects must increased by 1 ", 
                      "(no gap allowed in labels)"),
               fixed = TRUE)
  
  
  # Argument 'quadrat' ----
  
  expect_error(create_nodes_labels(tr_qu, quadrat = tr_qu$"quadrat"),
               paste0("Argument 'quadrat' must be a character of length 1 ", 
                      "(column name of the quadrats)"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu, quadrat = "quadratS"),
               "The column 'quadratS' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_bad_qu, quadrat = "quadrat"),
               "The column 'quadrat' must be a numeric",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu, transect = "transect", quadrat = NULL),
               paste0("As argument 'quadrat' is not provided, the column ", 
                      "'transect' cannot contain duplicated value"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_na_qu, 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               "The column 'quadrat' cannot contain NA",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_gap_qu, 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               paste0("Labels of quadrats must increased by 1 ", 
                      "(no gap allowed in labels)"),
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu_dup, 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               "Nodes labels cannot contain duplicates",
               fixed = TRUE)
  
  expect_error(create_nodes_labels(tr_qu[-1, ], 
                                   transect = "transect", 
                                   quadrat  = "quadrat"),
               paste0("The package 'chessboard' is not designed to work with ", 
                      "irregular grids"),
               fixed = TRUE)
})


test_that("create_nodes_labels() - Tests for success", {
  
  # Transects AND Quadrat NOT Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(tr_qu, 
                                 transect = "transect", 
                                 quadrat  = "quadrat")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 15L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(tr_qu$"transect", tr_qu$"quadrat", sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[15, "node"], "3-5")
  
  
  # Transects AND Quadrat AND Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(loc_tr_qu, 
                                 location = "location",
                                 transect = "transect", 
                                 quadrat  = "quadrat")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 15L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(tr_qu$"transect", tr_qu$"quadrat", sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[15, "node"], "3-5")
  
  
  # Transects NOT Quadrat NOT Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(tr_only, transect = "transect")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 5L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(tr_only$"transect", 1, sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[5, "node"], "5-1")
  
  
  # Transects NOT Quadrat AND Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(loc_tr_only, 
                                 location = "location",
                                 transect = "transect")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 5L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(loc_tr_only$"transect", 1, sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[5, "node"], "5-1")
  
  
  # Quadrat NOT Transects NOT Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(qu_only, quadrat = "quadrat")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 5L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(1, qu_only$"quadrat", sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[5, "node"], "1-5")
  
  
  # Quadrat NOT Transects AND Location ----
  
  expect_silent({
    nodes <- create_nodes_labels(loc_qu_only, quadrat = "quadrat")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 5L)
  expect_equal(ncol(nodes), 4L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(1, loc_qu_only$"quadrat", sep = "-")))
  
  expect_equal(nodes[1, "node"], "1-1")
  expect_equal(nodes[5, "node"], "1-5")
  
  
  # Keep all columns ----
  
  expect_silent({
    nodes <- create_nodes_labels(tr_qu_w_col, 
                                 transect = "transect",
                                 quadrat  = "quadrat")
  })
  
  expect_true(class(nodes) == "data.frame")
  
  expect_equal(nrow(nodes), 15L)
  expect_equal(ncol(nodes),  6L)
  
  expect_equal(colnames(nodes), c("node", "location", "transect", "quadrat", 
                                  "longitude", "latitude"))
  
  expect_equal(sort(nodes$"node"), 
               sort(paste(tr_qu_w_col$"transect", loc_qu_only$"quadrat", 
                          sep = "-")))
  
  expect_equal(nodes[ 1, "node"], "1-1")
  expect_equal(nodes[15, "node"], "3-5")
})