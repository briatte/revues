#
# shared authors between journals, weighted by articles
# (run revues-data.r first)
#

k = paste0("revue-", unique(d$revue))

for(j in k) {

  n = dir("html/num", pattern = paste0("^", j, "-\\d{4}"), full.names = TRUE)
  n = n[ file.info(n)$size > 0 ]

  f = paste0("csv/", j, ".csv")
  if(!file.exists(f)) {

    cat("\n", j, ":", length(n), "issue(s)\n")

    r = data.frame()
    for(i in rev(n)) {

      cat(sprintf("%3.0f", which(n == i)), i)

      h = html(i)

      # unique authors per article
      a = html_nodes(h, ".list_articles .authors")
      a = sapply(a, function(x) html_nodes(x, "a") %>%
                   html_attr("href") %>%
                   unique %>%
                   paste0(., collapse = ";"))

      # simplify URLs
      a = gsub("publications-de-|\\.htm", "", a[ a != "" ])
      cat(":", length(a), "articles(s)\n")

      # add to dataset
      if(length(a))
        r = rbind(r, data.frame(numero = i,
                                revue = gsub("html/num/revue-|-\\d{4}-(.*)\\.htm", "", i),
                                annee = str_extract(i, "[0-9]{4}"),
                                auteurs = a,
                                stringsAsFactors = FALSE))

    }

    write.csv(r, f, row.names = FALSE)

  }

}

r = dir("csv", pattern = "^revue-", full.names = TRUE)
r = lapply(r, read.csv, stringsAsFactors = FALSE)
r = bind_rows(r)

#
# journal-specific edge lists
#

a = strsplit(r$auteurs, ";")
e = data.frame()
for(i in unique(r$revue)) {

  y = unique(unlist(strsplit(r$auteurs[ r$revue == i ], ";")))
  cat(i, ":", length(y), "authors ")

  y = r[ which(sapply(a, function(x) any(x %in% y))), ]
  cat(nrow(y), "articles", n_distinct(y$revue), "journals\n")

  y = as.data.frame(table(y$revue))
  y$Freq = y$Freq / y$Freq[ y$Var1 == i ]
  y = y[ y$Var1 != i, ]

  e = rbind(e, data.frame(i, j = y$Var1, w = y$Freq, stringsAsFactors = FALSE))

}

#
# weighted edge list
#

n = apply(e[, 1:2 ], 1, function(x) paste0(sort(x), collapse = "///"))
n = data.frame(n, w = e$w, stringsAsFactors = FALSE)

e = aggregate(w ~ n, sum, data = n)
e = data.frame(i = gsub("(.*)///(.*)", "\\1", e$n),
               j = gsub("(.*)///(.*)", "\\2", e$n),
               w = e$w)

e = mutate(e, ecdf = cume_dist(w))

qplot(data = e, x = w, y = ecdf, color = I("grey")) +
  scale_x_log10() +
  labs(y = "Fréquence cumulée\n", x = "\nIntensité des liens (logarithme base 10)") +
  theme_bw()

ggsave("plots/ecdf.png", width = 9, height = 9)

e = filter(e, w < 1) %>% mutate(ecdf = cume_dist(w))

plot_ecdf = function(x) {

  g = qplot(data = subset(e, i != x & j != x),
            x = w, y = ecdf, color = I("grey")) +
    geom_point(data = subset(e, (i == x | j == x)), color = "black") +
    geom_rug(data = subset(e, (i == x | j == x)), sides = "b") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
    scale_x_log10() +
    labs(y = "Fréquence cumulée\n", x = "\nIntensité des liens (logarithme base 10)") +
    guides(color = FALSE) +
    theme_bw()

  print(g)

}

plot_ecdf("geneses")
ggsave("plots/ecdf_geneses.png", width = 9, height = 9)

plot_ecdf("societes")
plot_ecdf("sociologie")

#
# animated networks
#

for(x in c("societes", "sociologie")) {

  saveGIF({
    for(q in c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9,
               .91, .92, .93, .94, .95, .96, .97, .98, .99)) {

      t = quantile(e$w, q)
      n = network(e[ e$w > t, 1:2 ], directed = FALSE)
      print(n)
      set.edge.attribute(n, "weight", e[ e$w > t, 3 ])

      n %v% "size" = 1 + as.numeric(cut(degree(n), unique(quantile(degree(n))), include.lowest = TRUE))
      n %v% "id" = ifelse(network.vertex.names(n) == x, "S", "A")

      colors = c("A" = "black", "S" = "red")
      colors = colors[ names(colors) %in% unique(n %v% "id") ]

      n %e% "alpha" = as.numeric(cut(n %e% "weight", unique(quantile(n %e% "weight")), include.lowest = TRUE))
      n %e% "alpha" = n %e% "alpha" / max(n %e% "alpha")

      g = ggnet(n, size = 3,
                node.group = n %v% "id", node.color = colors,
                segment.alpha = n %e% "alpha", segment.color = "grey33",
                label.size = 6) + # label.size = n %v% "size",
        ggtitle(paste("seuil =", q, "\n", network.size(n), "revues")) +
        guides(color = FALSE)

      print(g)

    }
  }, movie.name = paste0("network_", x, ".gif"))

}

#
# relative centrality at each threshold level
#

rel_degree <- function(x) {

  wd = data.frame()
  for(q in seq(0, .99, .01)) {

    t = quantile(e$w, q)
    t = e[ e$w > t, 1:3 ]
    t$i = as.character(t$i)
    t$j = as.character(t$j)
    l = unique(c(t$i, t$j))
    t$i = as.numeric(factor(t$i, levels = l))
    t$j = as.numeric(factor(t$j, levels = l))
    t = symmetrise_w(as.tnet(t, type = "weighted one-mode tnet"))
    y = which(l == x)
    if(!length(y))
      wd = rbind(wd, data.frame(x, q, reldegree = 0, stringsAsFactors = FALSE))
    else
      wd = rbind(wd, data.frame(x, q,
                                reldegree = degree_w(t)[ y, 2 ] / max(degree_w(t)[, 2]),
                                stringsAsFactors = FALSE))

  }
  wd

}

wd = data.frame()
j = unique(r$revue)
for(i in rev(j)) {

  cat(sprintf("%3.0f", which(j == i)), i, "\n")
  wd = rbind(wd, rel_degree(i))

}

qplot(data = wd, y = reldegree, x = q, group = x,
      alpha = I(.5), color = I("grey"), geom = "line") +
  geom_line(data = subset(wd, x %in% c("recherche-en-soins-infirmiers",
                                       "actes-de-la-recherche-en-sciences-sociales",
                                       "societes",
                                       "sociologie",
                                       "francaise-de-sociologie",
                                       "reseaux",
                                       "geneses")),
            aes(color = x), alpha = 1, size = 1) +
  scale_color_brewer("", palette = "Set1") +
  labs(y = "Degré de centralité relatif\n", x = "\nSeuil d'intensité des liens entre les revues") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("plots/degree_all.png", width = 11, height = 8)
