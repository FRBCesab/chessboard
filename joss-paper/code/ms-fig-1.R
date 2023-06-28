library("chessboard")
library("ggplot2")
library("patchwork")

focus <- "5-5"

nodes <- expand.grid("transect" = 1:9, 
                     "quadrat"  = 1:9)

nodes <- create_node_labels(nodes,
                            transect = "transect",
                            quadrat  = "quadrat")


## DEFAULT SETTINGS ----

pawn_0 <- gg_chessboard(nodes, xlab = "Pawn", ylab = "default settings") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus))

fool_0 <- gg_chessboard(nodes, xlab = "Fool", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus))

rook_0 <- gg_chessboard(nodes, xlab = "Rook", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus))

bishop_0 <- gg_chessboard(nodes, xlab = "Bishop", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus))

knight_0 <- gg_chessboard(nodes, xlab = "Knight", ylab = "") +
  geom_node(nodes, focus) #+
  #geom_neighbors(nodes, knight(nodes, focus))

queen_0 <- gg_chessboard(nodes, xlab = "Queen", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus))

wizard_0 <- gg_chessboard(nodes, xlab = "Wizard", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus))


## ARGUMENT DEGREE ----

pawn_1 <- gg_chessboard(nodes, xlab = "", ylab = "degree = 3") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 3))

fool_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 3))

rook_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 3))

bishop_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 3))

knight_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 3))

queen_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 3))

wizard_1 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 3))


## ARGUMENT DIRECTED ----

pawn_2 <- gg_chessboard(nodes, xlab = "", ylab = "directed = TRUE") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 3, directed = TRUE))

fool_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 3, directed = TRUE))

rook_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 3, directed = TRUE))

bishop_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 3, directed = TRUE))

knight_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 3, directed = TRUE))

queen_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 3, directed = TRUE))

wizard_2 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 3, directed = TRUE))




## ARGUMENT DIRECTED ----

pawn_3 <- gg_chessboard(nodes, xlab = "", ylab = "reverse = TRUE") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, pawn(nodes, focus, degree = 3, directed = TRUE,
                             reverse = TRUE))

fool_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, fool(nodes, focus, degree = 3, directed = TRUE,
                             reverse = TRUE))

rook_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, rook(nodes, focus, degree = 3, directed = TRUE,
                             reverse = TRUE))

bishop_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, bishop(nodes, focus, degree = 3, directed = TRUE,
                               reverse = TRUE))

knight_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, knight(nodes, focus, degree = 3, directed = TRUE,
                               reverse = TRUE))

queen_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, queen(nodes, focus, degree = 3, directed = TRUE,
                              reverse = TRUE))

wizard_3 <- gg_chessboard(nodes, xlab = "", ylab = "") +
  geom_node(nodes, focus) +
  geom_neighbors(nodes, wizard(nodes, focus, degree = 3, directed = TRUE,
                               reverse = TRUE))


## FINAL FIGURE ----

plots <- 
  (pawn_0 | fool_0 | rook_0 | bishop_0 | queen_0 | knight_0 | wizard_0) /
  (pawn_1 | fool_1 | rook_1 | bishop_1 | queen_1 | knight_1 | wizard_1) /
  (pawn_2 | fool_2 | rook_2 | bishop_2 | queen_2 | knight_2 | wizard_2) /
  (pawn_3 | fool_3 | rook_3 | bishop_3 | queen_3 | knight_3 | wizard_3)


ggsave(here::here("joss-paper", "figures", "ms-fig-1.png"), plots, 
       width = 15, height = 9, units = "in", dpi = 300)

  
