# Data for tests ----

good_labels  <- c("1-1", "1-2", "2-1", "2-2")
bad_labels_1 <- c("11", "12", "21", "22")
bad_labels_2 <- c("1_1", "1_2", "21", "2_2")
bad_labels_3 <- c("1-1", "1-2", "2-1", "22")

test_that("check_nodes_labels() - Tests for wrong inputs", {
  
  expect_error(check_node_labels(bad_labels_1),
               "Node labels have not the good form",
               fixed = TRUE)
  
  expect_error(check_node_labels(bad_labels_2),
               "Node labels have not the good form",
               fixed = TRUE)
  
  expect_error(check_node_labels(bad_labels_3),
               "Some node labels are malformed",
               fixed = TRUE)
})


test_that("check_nodes_labels() - Tests for success", {
  
  expect_silent({ check <- check_node_labels(good_labels) })
  
  expect_null(check)
})
