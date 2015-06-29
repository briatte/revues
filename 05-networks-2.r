#
# 05 -- networks of shared authors between journals, with proportional weights
#

r = list.files("csv", full.names = TRUE)
r = lapply(r, read_csv, col_types = "ccc")
r = filter(bind_rows(r), revue %in% unique(d$revue))

cat("Edge list:", nrow(r), "rows\n")

#
# journal-specific edge lists
#

a = strsplit(r$auteurs, ";")

b = group_by(r, revue) %>%
  summarise(articles_j = n(),
            auteurs_j = paste0(auteurs, collapse = ";")) %>%
  rename(j = revue)

b$auteurs_j = sapply(b$auteurs_j, function(x) {
  n_distinct(unlist(strsplit(x, ";")))
})

M = list()
for(i in unique(r$revue)) {
  M[[i]] = unique(unlist(strsplit(r$auteurs[ r$revue == i ], ";")))
}

df = expand.grid(i = unique(r$revue), j = unique(r$revue))
df = unique(apply(df, 1, function(x) { paste0(sort(x), collapse = "///") }))
df = data_frame(i = gsub("(.*)///(.*)", "\\1", df),
                j = gsub("(.*)///(.*)", "\\2", df)) %>%
  filter(i != j)

df$n = NA # number of authors shared by journals (i, j)
df$p = NA # number of authors published by journals (i, j)

for(k in 1:nrow(df)) {

  p = c(M[[ df$i[ k ] ]], M[[ df$j[ k ] ]])
  df$n[ k ] = sum(duplicated(p))
  df$p[ k ] = n_distinct(p)

}

df$w = df$n / df$p # fraction of shared authors
df = filter(df, w > 0) %>%
  arrange(-w)

# dimensions

table(cut(df$w, c(0, 0.01, 0.025, 0.05, max(df$w)), right = FALSE))

sapply(c(0, 0.01, 0.025, 0.05), function(x)
  network.size(network(df[ df$w > x, 1:2 ])))

sapply(c(0, 0.01, 0.025, 0.05), function(x)
  network.edgecount(network(df[ df$w > x, 1:2 ])))

round(sapply(c(0, 0.01, 0.025, 0.05), function(x)
  network.density(network(df[ df$w > x, 1:2 ]))), 2)

# small plots

for(k in c(0.01, 0.025, 0.05)) {

  n = network(df[ df$w > k , 1:2], directed = FALSE)
  set.edge.attribute(n, "w", df$w[ df$w > k ])
  l = network.layout.fruchtermanreingold(n, NULL)

  png(paste0("plots/network_", gsub("\\.", "", k), ".png"))
  plot(n, coord = l, vertex.border = "white", vertex.col = "grey25",
       vertex.cex = log(1 + degree(n, cmode = "indegree")), edge.col = "grey50",
       edge.lwd = as.numeric(cut(n %e% "w", quantile(n %e% "w"),
                                 include.lowest = TRUE)))
  dev.off()

  pdf(paste0("plots/network_", gsub("\\.", "", k), ".pdf"))
  plot(n, coord = l, vertex.border = "white", vertex.col = "grey25",
       vertex.cex = log(1 + degree(n, cmode = "indegree")), edge.col = "grey50",
       edge.lwd = as.numeric(cut(n %e% "w", quantile(n %e% "w"),
                                 include.lowest = TRUE)))
  dev.off()

}

#
# weighted network measures
#

n = network(df[ , 1:2], directed = FALSE)
set.edge.attribute(n, "w", df$w)

w = as.tnet(as.matrix(n, attr = "w"), type = "weighted one-mode tnet")
w = data_frame(
  revue = network.vertex.names(n),
  degree = degree_w(w)[, 2],
  distance = colMeans(distance_w(w), na.rm = TRUE),
  closest = network.vertex.names(n)[ apply(distance_w(w), 2, which.min) ]
) %>%
  arrange(distance)

round( range(w$distance), 1)
round(  mean(w$distance), 1)

# ranking by smallest average distance

qplot(data = w, x = reorder(revue, -distance), y = distance) +
  coord_flip() +
  labs(x = NULL, y = "\nDistance moyenne aux autres revues de l'échantillon") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/network_distance_BW.png", width = 10, height = 16)
ggsave("plots/network_distance_BW.pdf", width = 10, height = 16)

# strongest network ties, with journal labels

n = network(df[ df$w > 0.05, 1:2], directed = FALSE)
set.edge.attribute(n, "w", df$w[ df$w > 0.05 ])

n = graph.adjacency(as.matrix(n), mode = "undirected") # igraph
l = layout.fruchterman.reingold(n)
o = sapply(1:5, function(x) which(membership(optimal.community(n)) == x))

png("plots/network_005_labels.png", width = 800, height = 800)
plot(n, layout = l,
     mark.groups = o, mark.col = "grey90", mark.border = "grey80",
     vertex.size = 10, vertex.color = "white", vertex.frame.color = "white",
     vertex.label = V(n)$vertex.names,
     vertex.label.family = "Helvetica", vertex.label.color = "black")
dev.off()
pdf("plots/network_005_labels.pdf")
plot(n, layout = l,
     mark.groups = o, mark.col = "grey90", mark.border = "grey80",
     vertex.size = 10, vertex.color = "white", vertex.frame.color = "white",
     vertex.label = V(n)$vertex.names,
     vertex.label.family = "Helvetica", vertex.label.color = "black")
dev.off()
