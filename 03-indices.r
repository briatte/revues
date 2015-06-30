#
# 03 -- concentration indices
#

# by journal:
# number of issues and unique authors
by_j = group_by(d, revue) %>%
  summarise(min = min(annee), max = max(annee),
            numeros = n_distinct(numero),
            auteurs = n_distinct(auteur)) %>%
  mutate(annees = as.numeric(max) - as.numeric(min) + 1) %>%
  arrange(-annees) #%>%
  # head(., 5)
  # tail(., 5)
  # comparable to 'Sociétés'
  # filter(annees %in% 12:16 & auteurs %in% 443:463)

# by journal issue:
# average number of authors and articles
by_i = group_by(d, numero) %>%
  mutate(auteurs = n_distinct(auteur)) %>%
  select(numero, revue, articles, auteurs) %>%
  unique %>%
  group_by(revue) %>%
  summarise(mu_articles = mean(articles), mu_auteurs = mean(auteurs))

#
# journals per author
#

dd = d %>%
  group_by(auteur) %>%
  mutate(n = n_distinct(revue)) %>%
  group_by(revue) %>%
  summarise(mu = mean(n), sd = sd(n), n = n()) %>%
  arrange(-mu) %>%
  mutate(rank = row_number(), rankp = percent_rank(mu))

# some example journals (sociology)
dd$tagged = NA
dd$tagged[ dd$revue == "societes" ] = "Sociétés"
dd$tagged[ dd$revue == "sociologie" ] = "Sociologie"
dd$tagged[ dd$revue == "carnet-de-notes-sur-les-maltraitances-infantiles" ] =
  "Carnet de notes sur les maltraitances infantiles"
dd$tagged[ dd$revue == "reseaux" ] = "Réseaux"
dd$tagged[ dd$revue == "population" ] = "Population"

qplot(data = dd, y = rankp, x = mu, geom = "step") +
  geom_point(data = subset(dd, !is.na(tagged))) +
  geom_segment(data = subset(dd, !is.na(tagged)),
               aes(xend = mu + .125, yend = rankp), lty = "dotted") +
  geom_text(data = subset(dd, !is.na(tagged)),
            aes(x = mu + .15, label = tagged), hjust = 0, fontface = "italic") +
  xlim(1, 4.25) +
  scale_y_continuous(label = percent_format()) +
  labs(x = "\nNombre moyen de revues par auteur", y = "Fréquence cumulée\n") +
  theme_bw(14) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_ecdf.png", width = 10, height = 9)
ggsave("plots/revues_ecdf.pdf", width = 10, height = 9)

# more comparable journals
dd$tagged = NA
dd$tagged[ dd$revue == "societes" ] = "Sociétés"
dd$tagged[ dd$revue == "geneses" ] = "Genèses"
dd$tagged[ dd$revue == "cahiers-d-etudes-africaines" ] = "Cahiers d'études africaines"

qplot(data = dd, y = rankp, x = mu, geom = "step") +
  geom_point(data = subset(dd, !is.na(tagged))) +
  geom_segment(data = subset(dd, !is.na(tagged)),
               aes(xend = mu + .3, yend = rankp), lty = "dotted") +
  geom_text(data = subset(dd, !is.na(tagged)),
            aes(x = mu + .3, label = tagged), hjust = 0, fontface = "italic") +
  xlim(1, 3.75) +
  scale_y_continuous(label = percent_format()) +
  labs(x = "\nNombre moyen de revues par auteur", y = "Fréquence cumulée\n") +
  theme_bw(14) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_ecdf_comparables.png", width = 10, height = 9)
ggsave("plots/revues_ecdf_comparables.pdf", width = 10, height = 9)

# sd increases with mean
qplot(data = dd, y = mu, x = sd) +
  labs(y = "Nombre moyen de revues par auteur\n", x = "\nÉcart-type") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw(14) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_mu_sd.png", width = 10, height = 9)
ggsave("plots/revues_mu_sd.pdf", width = 10, height = 9)

#
# example: 'Sociétés' journal
#

# other journals with same authors
t = unique(d[ d$revue == "societes", ]$auteur)
t = as.data.frame(table(d[ d$auteur %in% t, "revue" ]))
t = arrange(t, -Freq)
filter(t, Freq > 9)

# percentage of other journals
(sum(t$Freq) - t$Freq[1]) / sum(t$Freq)

#
# concentration indexes
#

# number of articles per author
n_a = group_by(d, auteur) %>%
  summarise(n_articles = n())

# author concentration index
h_a = d[, c("auteur", "revue") ] %>%
  group_by(auteur, revue) %>%
  # number of articles in each journal
  summarise(n = n()) %>%
  group_by(auteur) %>%
  # journal shares by author
  mutate(s = n / sum(n)) %>%
  # Herfindahl–Hirschman Index
  summarise(hhi = sum(s^2), n_revues = n())

# join
a = full_join(n_a, h_a, by = "auteur") %>%
  arrange(hhi)

# HHI will vary between 1 / max(a$n_revues) and 1
summary(a$n_revues)
summary(a$hhi)

# example authors with tons of articles in just one journal
filter(a, hhi == 1 & n_articles > 25)

# concentration decreases with number of journals per author
qplot(data = a, y = hhi, x = factor(ifelse(n_revues < 9, n_revues, "9+")),
      geom = "boxplot") +
  labs(y = "Indice de Herfindahl-Hirschman\n",
       x = "\nNombre de revues par auteur") +
  theme_bw(14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_hhi.png", width = 10, height = 9)
ggsave("plots/revues_hhi.pdf", width = 10, height = 9)

# percentage of single-journal authors + unweighted and weighted journal HHI
# weighted HHI is weighted by inverse frequency of author by journal
h = full_join(d[, c("revue", "auteur") ] %>%
                group_by(revue, auteur) %>%
                mutate(weight = n()) %>%
                unique %>%
                group_by(revue) %>%
                mutate(weight = weight / sum(weight)),
              a[, c("auteur", "hhi", "n_revues") ], by = "auteur") %>%
                group_by(revue) %>%
                summarise(
                  n_auts = n(),
                  p_mono = sum(n_revues == 1) / n(),
                  wt_hhi = weighted.mean(hhi, weight),
                  mu_hhi = mean(hhi),
                  sd_hhi = sd(hhi),
                  min_hhi = min(hhi),
                  max_hhi = max(hhi)) %>%
                arrange(mu_hhi)

# weighting produces almost no correction
cor(h$mu_hhi, h$wt_hhi)

# ordered HHI
qplot(data = h, x = reorder(revue, -mu_hhi),
      y = mu_hhi, ymin = mu_hhi - sd_hhi, ymax = mu_hhi + sd_hhi,
      geom = "pointrange") +
  coord_flip() +
  labs(x = NULL, y = "\nIndice de Herfindahl-Hirschman moyen ± 1 écart-type") +
  theme_bw(14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_hhi_rank.png", width = 10, height = 16)
ggsave("plots/revues_hhi_rank.pdf", width = 10, height = 16)

# correlation between HHI and mono-journal author percentage
with(h, cor(p_mono, wt_hhi))
with(h, cor(p_mono, mu_hhi))

# ordered mono-journal author percentage
qplot(data = h, x = reorder(revue, -p_mono),
      y = p_mono) +
  coord_flip() +
  scale_y_continuous(label = percent_format()) +
  labs(x = NULL, y = "\nPourcentage d'auteurs ayant publié dans cette seule revue") +
  theme_bw(14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_mono_rank.png", width = 10, height = 16)
ggsave("plots/revues_mono_rank.pdf", width = 10, height = 16)

# 'blind' percentage of editorial concentration, by journal

e = data_frame()
for(i in unique(d$revue)) {

  t = unique(filter(d, revue == i)$auteur)
  t = as.data.frame(table(d[ d$auteur %in% t, ]$revue))
  t = 1 - (sum(t$Freq) - t$Freq[ t$Var1 == i ]) / sum(t$Freq)
  e = rbind(e, data_frame(revue = i, t))

}
e = full_join(h, arrange(e, -t), by = "revue")

# correlation to HHI and mono-journal author percentage
with(e, cor(p_mono, t))
with(e, cor(mu_hhi, t))

# ordered editorial concentration rate
qplot(data = e, x = reorder(revue, -t),
      y = t) +
  coord_flip() +
  scale_y_continuous(label = percent_format()) +
  labs(x = NULL, y = "\nTaux de concentration de la production écrite des auteurs") +
  theme_bw(14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("plots/revues_conc_rank.png", width = 10, height = 16)
ggsave("plots/revues_conc_rank.pdf", width = 10, height = 16)
