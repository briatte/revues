#
# 01 -- get journal listings and issues
#

if(!file.exists(data)) {

  dir.create("html/rev", showWarnings = FALSE)
  dir.create("html/num", showWarnings = FALSE)

  r = "http://www.cairn.info/"

  if(!file.exists(html))
    download.file(paste0(r, "discipline.php?POS=11&TITRE=ALL"),
                  html, mode = "wb", quiet = TRUE)

  y = html(html)
  y = html_nodes(y, ".revue a") %>%
    html_attr("href") %>%
    unique

  for(i in y) {

    u = paste0(r, i)
    f = paste0("html/rev/", i)

    # first page
    cat("Parsing:", f, "\n")

    if(!file.exists(f))
      download.file(u, f, mode = "wb", quiet = TRUE)

    h = html(f)

    # possible additional pages
    p = html_nodes(h, ".pager .nb a") %>%
      html_attr("href")

    for(j in p) {

      u = paste0(r, j)
      f = gsub("\\.htm$", paste0("-", which(p == j) + 1, ".htm"), i)
      f = paste0("html/rev/", f)

      cat("Parsing:", f, "\n")

      if(!file.exists(f))
        download.file(u, f, mode = "wb", quiet = TRUE)

    }

  }

  d = data_frame()
  y = paste0("html/rev/", y)

  for(i in y) {

    h = html(i)

    # issues
    n = h %>%
      html_nodes(".list_numeros a") %>%
      html_attr("href") %>%
      unique

    cat("\n", i, ":", length(n), "issue(s)\n")

    for(j in rev(gsub("\\s", "%20", n))) {

      cat(sprintf("%3.0f", which(y == i)),
          sprintf("%3.0f", which(n == j)), j)

      u = paste0(r, j)
      f = paste0("html/num/", gsub("^\\./", "", j))

      # some issues are missing
      if(!file.exists(f))
        h = try(download.file(u, f, mode = "wb", quiet = TRUE), silent = TRUE)

      if("try-error" %in% class(h) | !file.info(f)$size) {

        cat(": failed\n")

      } else {

        h = html(f)

        # unique authors
        a = h %>%
          html_nodes(".list_articles .authors a") %>%
          html_attr("href") %>%
          unique

        # simplify URLs
        a = gsub("publications-de-|\\.htm$", "", a[ a != "" ])

        # unique articles
        l = html_nodes(h, ".list_articles .authors") %>%
          html_text() %>%
          str_trim

        l = length(l[ l!= "" ])

        cat(":", l, "articles(s)", length(a), "author(s)\n")

				# # article UIDs
        # uid = h %>%
        #   html_nodes(".list_articles .article") %>%
        #   html_attr("id")

        # add to dataset
        if(length(a))
          d = rbind(d, data_frame(numero = gsub("html/num/(\\./)?revue-|\\.htm", "", f),
                                  revue = gsub("html/num/(\\./)?revue-|(\\d)?-\\d{4}-(.*)\\.htm", "", f),
                                  annee = str_extract(f, "[0-9]{4}"),
                                  auteur = gsub("\\n|\\t|\\s|,", "", a),
                                  articles = l))

      }

    }

    n = paste0("html/num/", n)
    n = n[ !is.na(file.info(n)$size) ]
    n = n[ file.info(n)$size > 0 ]

    f = gsub("html/rev/", "csv/", gsub("htm$", "csv", i))
    if(!file.exists(f)) {

      cat("... saving authors of", length(n), "issue(s) to", f, "\n")

      r = data_frame()
      for(k in rev(n)) {

        h = html(k)

        # unique authors per article
        a = html_nodes(h, ".list_articles .authors")
        a = sapply(a, function(x) {
          html_nodes(x, "a") %>%
            html_attr("href") %>%
            unique %>%
            paste0(., collapse = ";")
        })

        # simplify URLs
        a = gsub("publications-de-|\\.htm", "", a[ a != "" ])

        # add to dataset
        if(length(a))
          r = rbind(r, data_frame(numero = gsub("html/num/(\\./)?revue-|\\.htm", "", k),
                                  revue = gsub("html/num/(\\./)?revue-|(\\d)?-\\d{4}-(.*)\\.htm", "", k),
                                  auteurs = gsub("\\n|\\t|\\s|,", "", a)))

      }

      regex = ifelse(n_distinct(r$revue) > 1,
                     paste0("(", paste0(unique(r$revue), collapse = "|"), ")"),
                     unique(r$revue))

      r$numero = gsub(paste0("^", regex, "(\\d)?-"), "", r$numero)

      # save edge list
      write_csv(r, f)

    }

  }

  # save authors list
  write_csv(d, data)

}

d = read_csv(data)

# recodings
d$revue[ d$revue == "clio" ] = "clio-femmes-genre-histoire"
d$revue[ d$revue %in% c("culture-chiffres", "culture-etudes", "culture-methodes",
                        "culture-prospective") ] = "culture-chiffres-etudes-methodes-prospective"

total = group_by(d, numero) %>%
  summarise(sum = unique(articles))

cat(data, "\nauthors:", n_distinct(d$revue), "journals",
    sum(total$sum), "articles",
    n_distinct(d$numero), "issues",
    n_distinct(d$auteur), "authors\n")

r = list.files("csv", full.names = TRUE) %>%
  lapply(., read_csv, col_types = "ccc") %>%
  bind_rows

# recodings
r$revue[ r$revue == "clio" ] = "clio-femmes-genre-histoire"
r$revue[ r$revue %in% c("culture-chiffres", "culture-etudes", "culture-methodes",
                        "culture-prospective") ] = "culture-chiffres-etudes-methodes-prospective"

stopifnot(unique(d$revue) %in% unique(r$revue))

cat("network:", n_distinct(r$revue),
    "journals", nrow(r), "articles",
    n_distinct(paste(r$numero, r$revue)), "issues",
    n_distinct(unlist(strsplit(r$auteurs, ";"))), "authors\n")

#
# breakdown by year of publication
#

table(d$annee)

mutate(d, y5 = cut(annee, c(1990, 1995, 2000, 2005, 2010, 2016))) %>%
  group_by(y5) %>%
  summarise(n_revues = n_distinct(revue),
            n_numeros = n_distinct(numero)) %>%
  mutate(cumsum = cumsum(n_numeros))

uniq = select(d, numero, annee, articles) %>%
  unique

table(uniq$annee >= 2005)
table(uniq$annee >= 2005) / nrow(uniq)

sum(uniq$articles)
sum(uniq$articles[ uniq$annee >= 2005]) / sum(uniq$articles)

#
# number of articles per author
#

auts = data.frame(table(d$auteur))
summary(auts$Freq) # range 1-52

sum(auts$Freq == 1) # with only one article
sum(auts$Freq == 1) / nrow(auts)

sum(auts$Freq > 1) # with over one article
summary(auts$Freq[ auts$Freq > 1 ])

filter(auts, Freq > 30) %>%
  arrange(-Freq) %>%
  head

#
# number of authors per journal
#

auts = group_by(d, revue) %>%
  summarise(mu = n_distinct(auteur))

summary(auts$mu)

# journals with smallest pools of authors (38)
filter(auts, mu == min(mu))

group_by(d, revue) %>%
  summarise(numeros = n_distinct(numero)) %>%
  filter(revue %in% c("carnet-de-notes-sur-les-maltraitances-infantiles",
                      "societe-droit-et-religion"))

# journal with largest pool of authors (1696)
filter(auts, mu == max(mu))

group_by(d, revue) %>%
  summarise(numeros = n_distinct(numero)) %>%
  filter(revue == "sante-publique")

# journal close to sample average
filter(auts, mu > 330, mu < 340)

group_by(d, revue) %>%
  summarise(numeros = n_distinct(numero)) %>%
  filter(revue == "politix")
