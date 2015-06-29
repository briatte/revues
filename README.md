Scripts to process author information from [Cairn.info](http://www.cairn.info/) journals.

See [this series of blog posts](http://politbistro.hypotheses.org/tag/edition-scientifique) (in French).

# HOWTO

Run `make.r` to replicate the results shown in the blog posts.

The code currently runs on sociology journals. To run on political science journals, edit lines 50-51 of `make.r` as follows:

```{r}
data = "revues-scpo.csv"
html = "html/revues-scpo-2014.html"
```

You will also need to select different example journals at lines 41-45 and 62-64 of `03-draw-plots.r`.

Two of the scripts contained in the repo are _not_ run by `make.r`:

- `05-get-articles.r` downloads article abstracts and keywords, and does so pretty slowly (on purpose)
- `06-more-networks.r` runs a different network construction routine, where tie strength is equal to the proportion of authors shared by two journals
