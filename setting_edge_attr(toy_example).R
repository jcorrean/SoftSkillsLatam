bip_edge_df <- data.frame(
  actor = c("a", "a", "b", "b", "c", "d", "d", "e"),
  event = c("e1", "e2", "e1", "e3", "e3", "e2", "e3", "e1"),
  actor_enjoyed_event = rep(c(TRUE, FALSE), 4),
  stringsAsFactors = FALSE)

bip_node_df <- data.frame(
  node_id = c("a", "e1", "b", "e2", "c", "e3", "d", "e"),
  node_type = c(
    "person", "event", "person", "event", "person",
    "event", "person", "person"),
  color = c(
    "red", "blue", "red", "blue", "red", "blue",
    "red", "red"),
  stringsAsFactors = FALSE)

#* bipartite networks with isolates ===================================================
bip_nodes_with_isolates <- rbind(
  bip_node_df,
  data.frame(
    node_id = c("f", "e4"),
    node_type = c("person", "event"),
    color = c("red", "blue"),
    stringsAsFactors = FALSE
  )
)
# indicate which vertices are actors via a column named `"is_actor"`
bip_nodes_with_isolates$is_actor <- bip_nodes_with_isolates$node_type == "person"
bip_nodes_with_isolates

library(network)
pave2 <- as.network(bip_edge_df,
           directed = FALSE,
           vertices = bip_nodes_with_isolates,
           bipartite = TRUE
)
pave2