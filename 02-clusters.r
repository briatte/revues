#
# 02 -- cluster analysis of disciplinary affiliations
#

a = html("html/revues-2015.html")
a = html_nodes(a, ".revue a") %>%
  html_attr("href") %>%
  unique %>%
  gsub("^\\./", "", .) %>%
  data_frame(revue = .)

for(i in list.files("html", pattern = "revues-\\w+-\\d+", full.names = TRUE)) {

  y = html(i)
  y = html_nodes(y, ".revue a") %>%
    html_attr("href") %>%
    unique

  a[, gsub("html/revues-|-\\d+\\.html", "", i) ] = as.numeric(a$revue %in% y)

}

a$revue = gsub("revue-|\\.htm", "", a$revue)
a$n = apply(a[, -1], 1, sum)
a = filter(a, n > 0)

a$discipline = apply(a[, -c(1, 16) ], 1, function(x) which(x == 1))
a$discipline = sapply(a$discipline, names)
a$discipline = sapply(a$discipline, function(x) ifelse(length(x) > 1, "mixte", x))

a$combined = apply(a[, -c(1, 16:17) ], 1, function(x) which(x == 1))
a$combined = sapply(a$combined, names)
a$combined = sapply(a$combined, paste0, collapse = ",")

# most frequent cross-disciplinary ties
table(a$combined[ a$n > 1 ])[ table(a$combined[ a$n > 1 ]) > 5 ]

# most active disciplines in cross-disciplinary ties
t1 = table(unlist(str_split(names(table(a$combined[ a$n > 1 ])), ",")))
t1 = as.data.frame(t1) %>% arrange(-Freq)
head(t1)

# percentage of cross-disciplinary journals
t2 = round(100 * table(unlist(str_split(names(table(a$combined[ a$n > 1 ])), ","))) /
             colSums(a[, -c(1, 16:18) ]))
t2 = as.data.frame(t2) %>% arrange(-Freq)
head(t2)

qplot(data = full_join(t1, t2, "Var1"), x = Freq.x, y = Freq.y,
      label = Var1, geom = "text") +
  geom_vline(x = mean(t1$Freq), lty = "dashed") +
  geom_hline(y = mean(t2$Freq), lty = "dashed") +
  labs(y = "% de revues trans-disciplinaires\n",
       x = "\nNombre de revues trans-disciplinaires") +
  theme_bw(14) +
  theme(panel.grid = element_blank())

ggsave("plots/disciplines_trans.pdf", height = 7, width = 7)

R = a[, -c(1, 16:18) ]

# -arts and -lettres
R = R[, !colnames(R) %in% c("arts", "lettres") ]

#
# PCA
#

p = prcomp(R)

autoplot(p, loadings = TRUE, data = a, colour = "discipline", loadings.label = TRUE) +
  geom_vline(x = 0, lty = "dashed") +
  geom_hline(x = 0, lty = "dashed") +
  scale_color_discrete("") +
  labs(y = "PC2\n", x  = "\nPC1") +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank())

ggsave("plots/disciplines_pca.pdf", width = 9, height = 9)

autoplot(p, data = a) +
  geom_hline(y = 0, lty = "dotted") +
  geom_vline(x = 0, lty = "dotted") +
  facet_wrap(~ discipline) +
  labs(y = "PC2\n", x  = "\nPC1") +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"))

ggsave("plots/disciplines_pca_facets.pdf", width = 9, height = 9)

autoplot(clara(R, 4), frame = TRUE, frame.type = "norm") +
  geom_vline(x = 0, lty = "dashed") +
  geom_hline(x = 0, lty = "dashed") +
  labs(y = "PC2\n", x  = "\nPC1") +
  theme_bw(14) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank())

ggsave("plots/disciplines_clara.pdf", width = 9, height = 9)

#
# disciplines matrix
#

a$n = apply(a[, -c(1, 16:18), ], 1, sum)

a = gather(a, key, dummy, -revue, -n, -discipline, -combined)

qplot(data = filter(a, n > 0), y = reorder(revue, n), x = key,
      fill = factor(dummy), geom = "tile") +
  scale_fill_manual(values = c("0" = "grey90", "1" = "grey10")) +
  guides(fill = FALSE) +
  labs(y = NULL, x = NULL) +
  theme_bw(12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("plots/disciplines_matrix.pdf", height = 10, width = 8)
