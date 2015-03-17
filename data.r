if(!file.exists(data)) {

  dir.create("html/rev", showWarnings = FALSE)
  dir.create("html/num", showWarnings = FALSE)

  r = "http://www.cairn.info/"

  if(!file.exists(html))
     download.file(paste0(r, "discipline.php?POS=11&TITRE=ALL"),
                   html, mode = "wb", quiet = TRUE)

  y = html(html)
  y = html_nodes(y, ".revue a") %>% html_attr("href") %>% unique

  for(i in y) {

    u = paste0(r, i)
    f = paste0("html/rev/", i)

    # first page
    cat("Parsing:", f, "\n")

    if(!file.exists(f))
      download.file(u, f, mode = "wb", quiet = TRUE)

    h = html(f)

    # possible additional pages
    p = html_nodes(h, ".pager .nb a") %>% html_attr("href")

    for(j in p) {

      u = paste0(r, j)
      f = gsub("\\.htm$", paste0("-", which(p == j) + 1, ".htm"), i)
      f = paste0("html/rev/", f)

      cat("Parsing:", f, "\n")

      if(!file.exists(f))
        download.file(u, f, mode = "wb", quiet = TRUE)

    }

  }

  d = data.frame()
  for(i in paste0("html/rev/", y)) {

    h = html(i)

    # issues
    n = h %>%
      html_nodes(".list_numeros a") %>%
      html_attr("href") %>%
      unique

    cat("\n", i, ":", length(n), "issue(s)\n")

    for(j in rev(gsub("\\s", "%20", n))) {

      cat(sprintf("%3.0f", which(n == j)), j)

      u = paste0(r, j)
      f = paste0("html/num/", j)

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
        l = str_trim(h %>% html_nodes(".list_articles .authors") %>% html_text())
        l = length(l[ l!= "" ])

        cat(":", l, "articles(s)", length(a), "author(s)\n")

        # add to dataset
        if(length(a))
          d = rbind(d, data.frame(numero = f,
                                  revue = gsub("html/num/revue-|-\\d{4}-(.*)\\.htm", "", f),
                                  annee = str_extract(f, "[0-9]{4}"),
                                  auteur = a,
                                  articles = l,
                                  stringsAsFactors = FALSE))

      }


    }

  }

  write.csv(d, data, row.names = FALSE)

}

d = read.csv(data, stringsAsFactors = FALSE)

table(d$annee)

total = group_by(d, numero) %>% summarise(sum = unique(articles))

cat(data, ":", n_distinct(d$revue), "journals",
    sum(total$sum), "articles",
    n_distinct(d$numero), "issues",
    n_distinct(d$auteur), "authors\n")
