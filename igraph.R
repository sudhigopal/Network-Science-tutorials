G <- make_empty_graph(n=5) %>%
  add_edges(c(1,2, 2,3, 3,4, 4,5 )) %>%
  set_edge_attr("color", value="blue") %>%
  add_vertices(1)%>%
  add_edges(c(1,6))

E(g)
plot(G)




  