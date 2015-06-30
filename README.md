R code to process author information from [Cairn.info](http://www.cairn.info/) journals. See [this series of blog posts](http://politbistro.hypotheses.org/tag/edition-scientifique) (in French) for the first part of the analysis.

# HOWTO

Run `make.r` to replicate the results shown in the blog posts and produce some additional results using different network tie weights.

The code currently runs on sociology journals. To run on e.g. political science journals, edit lines 50-51 of `make.r` as follows:

```{r}
data = "revues-scpo.csv"
html = "html/revues-scpo-2014.html"
```

You will also need to select different example journals at lines 41-45 and 64-66 of `03-concentration.r`.

The `html` folder contains an index for every discipline on which the analysis might be run, as well as a general index to run the analysis on all disciplines combined. Most of the indexes are from July 2014, except the ones for sociology and for all journals, which are from early 2015.

The script `06-articles.r` downloads article abstracts and keywords, and does so pretty slowly on purpose. This script is _not_ run by `make.r`, as its results are not used in any part of the analysis.
