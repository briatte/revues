# setwd("revues-indexes")

r = html("html/revues-2015.html")
r = html_nodes(r, ".revue a") %>%
  html_attr("href") %>%
  unique %>%
  gsub("^\\./", "", .) %>%
  data.frame(revue = ., stringsAsFactors = FALSE)

for(i in dir("html", pattern = "revues-\\w+-\\d+", full.names = TRUE)) {

  y = html(i)
  y = html_nodes(y, ".revue a") %>%
    html_attr("href") %>%
    unique

  r[, gsub("html/revues-|-\\d+\\.html", "", i) ] = as.numeric(r$revue %in% y)

}

r$revue = gsub("revue-|\\.htm", "", r$revue)
r$n = apply(r[, -1], 1, sum)
r = filter(r, n > 0)

r$discipline = apply(r[, -c(1, 16) ], 1, function(x) which(x == 1))
r$discipline = sapply(r$discipline, names)
r$discipline = sapply(r$discipline, function(x) ifelse(length(x) > 1, "Mixte", x))

r$combined = apply(r[, -c(1, 16:17) ], 1, function(x) which(x == 1))
r$combined = sapply(r$combined, names)
r$combined = sapply(r$combined, paste0, collapse = ",")

table(r$combined[ r$n > 1 ])[ table(r$combined[ r$n > 1 ]) > 5 ]

R = r[, -c(1, 16:18) ]

# -arts and -lettres
R = R[, !colnames(R) %in% c("arts", "lettres") ]

#
# PCA
#

p = prcomp(R)
autoplot(p, loadings = TRUE, data = r, colour = "discipline", loadings.label = TRUE) +
  geom_vline(x = 0, lty = "dashed") +
  geom_hline(x = 0, lty = "dashed") +
  scale_color_discrete("") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key = element_blank())

ggsave("plots/clusters_pca.pdf", width = 9, height = 9)

autoplot(p, data = r) +
  geom_hline(y = 0, lty = "dotted") +
  geom_vline(x = 0, lty = "dotted") +
  facet_wrap(~ discipline) +
  theme_bw()

autoplot(clara(R, 4), frame = TRUE, frame.type = "norm") +
  geom_vline(x = 0, lty = "dashed") +
  geom_hline(x = 0, lty = "dashed") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key = element_blank())

ggsave("plots/clusters_clara.pdf", width = 9, height = 9)

#
# dendogram
#

p = hclust(dist(r[, -1]))
p = as.dendrogram(p)

r$n = apply(r[, -c(1, 16:18), ], 1, sum)

r = gather(r, key, dummy, -revue, -n, -discipline, -combined)

qplot(data = filter(r, n > 0), y = reorder(revue, n), x = key, fill = factor(dummy), geom = "tile") +
  scale_fill_manual(values = c("0" = "grey90", "1" = "grey10")) +
  guides(fill = FALSE) +
  labs(y = NULL, x = NULL) +
  theme_bw(12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("plots/disciplines.pdf", height = 10, width = 8)
