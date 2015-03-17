Scripts to process author information from [Cairn.info](http://www.cairn.info/) journals.

# HOWTO

Run `make.r` to replicate in full:

* `data.r` processes the raw data
* `plots.r` produces various concentration rankings
* `networks.r` produces network plots and measures
* `pca.r` produces cluster plots and measures

The code currently runs on sociology journals. To run on all journals, edit lines 46-47 of `make.r` as follows:

```{r}
data = "revues.csv"
html = "html/revues-2015.html"
```

# TODO

- test on other disciplines
- discipline-specific cluster analysis
