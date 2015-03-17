Scripts to process author information from [Cairn.info](http://www.cairn.info/) journals.

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

# TODO

- discipline-specific cluster analysis
- test-run on all represented disciplines
