---
title: 'chessboard: An R package for creating network connections based on chess moves'
tags:
- r
- package
- networks
- neighbors
- edges
date: "13 July 2023"
output: pdf_document
authors:
- name: Nicolas Casajus
  orcid: "0000-0002-5537-5294"
  equal-contrib: no
  corresponding: yes
  affiliation: 1
- name: Érica Rievrs Borges
  orcid: "0000-0001-7751-6265"
  equal-contrib: no
  corresponding: no
  affiliation: 1
- name: Éric Tabacchi
  orcid: "0000-0001-7729-4439"
  equal-contrib: no
  corresponding: no
  affiliation: 2
- name: Guillaume Fried
  orcid: "0000-0002-3653-195X"
  equal-contrib: no
  corresponding: no
  affiliation: 3
- name: Nicolas Mouquet
  orcid: "0000-0003-1840-6984"
  equal-contrib: no
  corresponding: no
  affiliation: "1, 4"
bibliography: paper.bib
affiliations:
- name: "FRB-CESAB, Montpellier, France"
  index: 1
- name: "CNRS, Toulouse, France"
  index: 2
- name: "ANSES, Montpellier, France"
  index: 3
- name: MARBEC, Univ Montpellier, CNRS, IFREMER, IRD, Montpellier, France
  index: 4
---


# Summary

`chessboard` aims to facilitate the creation of connectivity matrices for 
sampling networks designed as regular grids. It can handle directed (asymmetric)
and undirected (symmetrical) spatial (or not spatial) networks connection. 
`chessboard` offers various methods to detect neighbors, all based on the chess 
game, allowing the creation of complex connectivity scenarios.



# Statement of need

The analysis of network connections is present in many research fields, whether 
the network represents spatial connections or not, and is based on different 
proposed methods for defining  neighbors. In ecology, spatial network analyses 
are widely used to investigate spatial patterns of organisms’ distribution and 
provide important information about the underlying ecological processes leading 
to these patterns (@legendre-2012, @pilosof-2017). Such methods rely on building
a connectivity matrix (also known as adjacency matrix) among sampling units 
(hereafter 'nodes'), which consists of a square matrix of dimensions n x n 
(where n is the total number of nodes). This connectivity matrix thus represents
the presence or absence of a link (hereafter 'edge') between each pair of nodes. 
When building a connectivity matrix, for each node, the detection of neighbors 
(new edges) will depend on the type of sampling design used (surface area, 
transect, regular grid, irregular grid, @dray-2006).

Here, we introduce `chessboard`, an open-source R package (@r-2023), dedicated 
to facilitating the process of creating connectivity matrices for samplings 
designed as regular grids (i.e. the distance between each pair of nodes along 
one axis is always the same). `chessboard` identifies neighbors only based on 
node position on a two-dimension non-spatial referential. It can handle spatial 
networks, but it does not explicitly use geographical coordinates to find 
neighbors (it is not based on spatial distance). `chessboard` can handle 
directed (asymmetric) and undirected (symmetrical) spatial (or not) networks.

Different methods are available in `chessboard` to detect neighbors, all based 
on the chess game as proposed in many R packages like 
[`spdep`](https://r-spatial.github.io/spdep/) (@bivand-2018), but with the 
advantage of several additional chess movements which flexibilizes the user's 
experience and allows the creation of complex connectivity scenarios 
(\autoref{fig:fig1}). `chessboard` implements the following rules to detect 
neighbors and to create edges:

- the **degree** of neighborhood: the number of adjacent nodes that will be used
to create edges;
- the **orientation** of neighborhood: can neighbors be detected horizontally, 
vertically and/or diagonally?
- the **direction** of neighborhood: does the sampling have a main direction? 
This can be particularly relevant for directed networks (e.g. rivers).


![Overview of methods available in `chessboard` to detect neighbors. Red dots locate the node of interest (5-5) and black dots correspond to detected neighbors. Each column corresponds to a specific method derived from the chess game. Each row illustrates the use of one argument (row 1: default settings; row 2: use of the argument `degree`; row 3: use of the argument `directed`; row 4: use of the argument `reverse`)\label{fig:fig1}](figures/figure-1.png){ width=100% }


`chessboard` provides simple visualization functions and generates outputs that 
are compatible and reusable with various other R packages: 
[`adespatial`](https://sdray.github.io/adespatial/) (@dray-2023), 
[`spdep`](https://r-spatial.github.io/spdep/) (@bivand-2018), 
[`igraph`](https://r.igraph.org/) (@csardi-2006), 
[`sf`](https://r-spatial.github.io/sf/) (@pebesma-2018) and 
[`ggplot2`](https://ggplot2.tidyverse.org) (@wickham-2016).



# Main features


This section is an overview of the main features of `chessboard` 
(\autoref{fig:fig2}).


![Main features and usage of the R package `chessboard`.\label{fig:fig2}](figures/figure-2.png){ width=100% }


To illustrate the package, let’s create a fictitious sampling design of 
dimensions 5 x 5 (25 nodes in total). By convention, we will name the x-axis 
**_transects_** and the y-axis **_quadrats_**.


```r
# Fictitious sampling (non spatial) ----
sampling <- expand.grid("transect" = 1:5,
                        "quadrat"  = 1:5)
```


When working with `chessboard`, the first step is to create labels for nodes 
with the function `create_node_labels()`.


```r
# Create node labels ----
nodes <- create_node_labels(data     = sampling,
                            transect = "transect",
                            quadrat  = "quadrat")
```


Node labels are a combination of the transect identifier (i.e. the position of 
the node on the x-axis of the grid) and the quadrat identifier (i.e. the 
position of the node on the y-axis of the grid).

Then we implement a connectivity scenario where we would like to connect nodes 
according to the 'bishop' move , i.e. diagonally, with a degree of neighborhood 
of 2 for an undirected network. The function `bishop()` (and all other 
functions named after the chess game) returns a subset of the nodes object 
(`data.frame`) containing the neighbors of the focus node. 


```r
# Find neighbors according to the bishop move (for one node) ----
nb <- bishop(nodes    = nodes,
             focus    = "2-3",
             degree   = 2,
             directed = FALSE)
```


Plotting functions available in `chessboard` can be used to inspect the results
(\autoref{fig:fig3}):


```r
gg_chessboard(nodes) +
  geom_edges(nodes, "2-3", nb) +
  geom_neighbors(nodes, nb) +
  geom_node(nodes, "2-3")
```


![Neighbors and edges detected for one node using the bishop method and a degree of neighborhood of 2 (undirected network). The red dot locates the node of interest (2-3) on the chessboard. Black dots correspond to detected neighbors and arrows are corresponding edges.\label{fig:fig3}](figures/figure-3.png){ width=50% }


The [Chess pieces](https://frbcesab.github.io/chessboard/articles/chess-pieces.html) 
vignette details all possible moves implemented in `chessboard` and the effects 
of the arguments `degree`, `directed`, `reverse` and `self`.

The function `create_edge_list()` will detect the neighbors for the 25 nodes:


```r
# Create edges according to the bishop move (for all nodes) ----
edges <- create_edge_list(nodes    = nodes,
                      	  method   = "bishop",
                      	  degree   = 2,
                      	  directed = FALSE)
```


This function returns an **edge list**, i.e. a two-column `data.frame` where a 
row corresponds to an edge between two nodes.

The function `connectivity_matrix()` computes the connectivity matrix of this 
undirected network.


```r
# Build connectivity matrix ----
mat <- connectivity_matrix(edges)
```


Finally, the function `gg_matrix()` can be used to plot the resulting matrix 
(\autoref{fig:fig4}).

```r
# Plot connectivity matrix ----
gg_matrix(mat)
```


![Connectivity matrix of a 5 transects x 5 quadrats network when edges (black squares) are created using the bishop method and a degree of neighborhood of 2 (undirected network). Each row and each column of this square matrix corresponds to a node of the network.\label{fig:fig4}](figures/figure-4.png){ width=50% }


`chessboard` provides three long-form documentations to learn more about the 
package:

- a [Get started](https://frbcesab.github.io/chessboard/articles/chessboard.html) 
vignette describing the core features of the package;
- a [Chess pieces](https://frbcesab.github.io/chessboard/articles/chess-pieces.html) 
vignette detailing the different methods available to detect neighbors;
- a [Visualization tools](https://frbcesab.github.io/chessboard/articles/visualization-tools.html) 
vignette describing the plotting functions available in `chessboard`.



# Acknowledgements

This work is a product of the 
[BRIDGE](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/bridge/) 
research project funded by the French national research program 
[ITTECOP](https://ittecop.fr/en/) (CILB, MTE, ADEME and OFB) and the 
French Foundation for Research on Biodiversity 
([FRB](https://www.fondationbiodiversite.fr/en/)) through its Centre for Synthesis 
and Analysis of Biodiversity data 
([CESAB](https://www.fondationbiodiversite.fr/en/about-the-foundation/le-cesab/)).



# References
