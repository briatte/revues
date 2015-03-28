Scripts to process author information from [Cairn.info](http://www.cairn.info/) journals.

See [this series of blog posts](http://politbistro.hypotheses.org/tag/edition-scientifique) (in French).

# HOWTO

Run `make.r` to replicate in full:

* `data.r` processes the raw data
* `plots.r` produces various concentration rankings
* `networks.r` produces network plots and measures
* `pca.r` produces cluster plots and measures

The code currently runs on sociology journals. To run on political science journals, edit lines 46-47 of `make.r` as follows:

```{r}
data = "revues-scpo.csv"
html = "html/revues-scpo-2014.html"
```

You will also need to select different example journals at lines 41-45 and 62-64 of `plots.r`.

See `articles.r` for the code to download article abstracts and keywords, which requires approximately 1.7GB of disk space. This script runs slowly on purpose, and is _not_ run by `make.r`.

# TODO

- [x] test-run on all represented disciplines
- [x] discipline-level cluster analysis

