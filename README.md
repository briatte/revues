R code to process author information from [Cairn.info](http://www.cairn.info/) journals. See [this series of blog posts](http://politbistro.hypotheses.org/tag/edition-scientifique) (in French) for a preliminary version of the analysis.

# HOWTO

Open `make.r` to check the package dependencies and run it to replicate the analysis in full. The code currently runs on sociology journals. To run on e.g. political science journals, edit `make.r` as follows:

```{r}
data = "revues-scpo.csv"
html = "html/revues-scpo-2014.html"
```

You will also need to edit `03-indices.r` to select different example journals.

The `html` folder contains an index for every discipline on which the analysis might be run, as well as a general index to run the analysis on all disciplines combined. Due to a recently introduced limitation in the HTML code of the platform, most of the indexes are from July 2014, except the ones for sociology and for all journals, which are from March 2015.

If you need to download more detailed article data, try the following script:

```{r}
dir.create("html/art", showWarnings = FALSE)
p = list.files("html/num", pattern = "htm$", full.names = TRUE)
p = sample(p, length(p))

# include some sleep time every 100 queries or so to avoid choking
s = length(list.files("html/art", pattern = "html$")) %/% 100

for(i in rev(p)) {

  if(!file.info(i)$size) {

    cat("\n", sprintf("%4.0f", which(p == i)), i, ": empty")
    next

  }

  h = html(i) %>%
    html_nodes(xpath = "//div[contains(@class, 'article')]//a[contains(@href, 'resume')]") %>%
    html_attr("href")

  cat("\n", sprintf("%4.0f", which(p == i)), "Fetching",
      sprintf("%3.0f", length(h)), "article(s) from",
      gsub("html/num/revue-|\\.htm", "", i), "")

  for(j in h) {

    f = paste0("html/art/", gsub("(.*)ID_ARTICLE=(.*)", "\\2", j), ".html")
    if(!file.exists(f))
      try(download.file(paste0("http://www.cairn.info/", j), f,
                        mode = "wb", quiet = TRUE), silent = TRUE)

    if(!file.info(f)$size)
      cat("\n[!] ERROR downloading", j)

    # avoid writing too quickly to disk
    Sys.sleep(1)

  }

  a = list.files("html/art", pattern = "html$")
  if(length(a) %/% 100 > s) {

    cat("\nFetched", length(a), "articles, sleeping a bit...")
    Sys.sleep(sample(10 * 1:11, 1)) # averages at 60 seconds
    s = s + 1

  }

}
```
