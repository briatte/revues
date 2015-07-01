#
# 05 -- linear regressions
#

qplot(data = w, x = distance, geom = "density")
qplot(data = w, x = degree, geom = "density")

# regress issue-level characteristics

by_i = left_join(by_i, w, by = "revue")

I1 = lm(distance ~ mu_articles + mu_auteurs, data = by_i)
I2 = lm(degree ~ mu_articles + mu_auteurs, data = by_i)

screenreg(list(I1, I2),
          custom.model.names = c("Distance", "Degree"),
          include.rsquared = FALSE)

# regress journal-level characteristics

by_j = left_join(by_j, w, by = "revue") %>%
  left_join(group_by(unique(select(d, revue, numero, articles)), revue) %>%
              summarise(articles = sum(articles)),
            by = "revue")

J1 = lm(distance ~ numeros + auteurs, data = by_j)
J2 = lm(degree ~ numeros + auteurs, data = by_j)
J3 = lm(distance ~ numeros + auteurs + articles, data = by_j)
J4 = lm(degree ~ numeros + auteurs + articles, data = by_j)
J5 = lm(distance ~ numeros + auteurs + articles + annees, data = by_j)
J6 = lm(degree ~ numeros + auteurs + articles + annees, data = by_j)

screenreg(list(J1, J2, J3, J4, J5, J6),
          custom.model.names = rep(c("Distance", "Degree"), 3),
          include.rsquared = FALSE)
