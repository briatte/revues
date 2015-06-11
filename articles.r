# run other download routines first
library(rvest)

dir.create("html/art", showWarnings = F)
p = dir('html/num', pattern = "htm$", full.names = T)
p = sample(p, length(p))

# include some sleep time every 100 queries or so to avoid choking
s = length(dir("html/art", pattern = "html$")) %/% 100

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

  a = dir("html/art", pattern = "html$")
  if(length(a) %/% 100 > s) {

    cat("\nFetched", length(a), "articles, sleeping a bit...")
    Sys.sleep(sample(10 * 1:11, 1)) # averages at 60 seconds
    s = s + 1

  }

}
