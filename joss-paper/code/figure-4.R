library("chessboard")
library("ggplot2")

focus <- "2-3"

nodes <- expand.grid("transect" = 1:5, 
                     "quadrat"  = 1:5)

nodes <- create_node_labels(nodes,
                            transect = "transect",
                            quadrat  = "quadrat")

edges <- create_edge_list(nodes    = nodes,
                          method   = "bishop",
                          degree   = 2, 
                          directed = FALSE)

mat <- connectivity_matrix(edges)

plots <- gg_matrix(mat)

ggsave(here::here("joss-paper", "figures", "figure-4.png"), plots, 
       width = 9, height = 9, units = "in", dpi = 300, scale = 0.65)

