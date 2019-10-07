library(igraph)
# install.packages("CINNA")
library(CINNA)
g <- make_empty_graph(n=6, directed = FALSE) %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5, 1,5)) %>%
  set_edge_attr("color", value = "green") %>%
  add_edges(c(5,6, 5,1), color = "yellow") %>%
  add_vertices(nv = 2) %>%
  add_edges(c(5,3, 6,7), color = "black") %>%
  add_edges(c(7,8, 6,8), color = "red")
summary(g)
# Layout for the graph
layout <- layout.kamada.kawai(g)
E(g)[[]]
plot(g, layout = layout)

# ?make_graph look at the terminal
g <- make_graph("Zachary")
# data(g)
# adjacent_vertices(g, c(1,34))
all_simple_paths(g, 1, 3)
layout=layout.auto(g)
plot(g, layout=layout)

pr_cent<-proper_centralities(g)

# list of all centrality
calculate_centralities(g, include = pr_cent[1:5]) %>%
  pca_centralities(scale.unit = TRUE)

centr_betw(g,directed = FALSE)

centr_clo(g, mode = "all") 

centr_eigen(g, directed = FALSE)$centralization

centr_degree(g)$centralization

# connectivity
g1 <- make_full_graph(5, directed = FALSE)
plot(g1)
cliques(g1)
max_cliques(g1)

closeness(g1, mode = 'all', normalized = TRUE)

eb <-edge_betweenness(g1)
eb

# random graphs
g2 <- barabasi.game(100, m=2)
cords<- layout_components(g2)

pg <- page_rank(g2, algo = "arpack", vids = V(g2), directed = TRUE, damping = 0.85)$vector

plot(g2, layout=layout.circle, vertex.size=pg*100,
     vertex.label.dist=0.1, vertex.color="red", edge.arrow.size=0.25)

g <- make_(ring(10), with_vertex_(name = LETTERS[1:10]))

g$name <- "10 ring"
d <- get_diameter(g)

plot(g, path = d)

g2 <- read_graph("/*download the file and insert the path*/", format = "gml")
plot(g2)

write_graph(g2, "/Users/sudhigopal/Desktop/OU_Courses/Fall18/Network\ Science/barbasiR.graphml", format = c("graphml"))
