library("chessboard")
library("ggplot2")

focus <- "2-3"

nodes <- expand.grid("transect" = 1:5, 
                     "quadrat"  = 1:5)

nodes <- create_node_labels(nodes,
                            transect = "transect",
                            quadrat  = "quadrat")

nb_bishop <- bishop(nodes    = nodes, 
                    focus    = focus, 
                    degree   = 2, 
                    directed = FALSE)

plots <- gg_chessboard(nodes) +
  geom_edges(nodes, focus, nb_bishop) +
  geom_neighbors(nodes, nb_bishop) +
  geom_node(nodes, focus)

ggsave(here::here("joss-paper", "figures", "figure-3.png"), plots, 
       width = 9, height = 9, units = "in", dpi = 300, scale = 0.65)

