As requested by Dr. Uwe Ligges I have reduced the size of the package tarball
under 5 MB by omitting the package (visual) unit tests 
and omitting seven package vignettes.

## Test environments

* win-builder (windows, R devel)
* mac-builder (macosx, R release)
* local (linux, R 4.1.2)
* github actions (linux, R release)
* github actions (linux, R devel)

## R CMD check --as-cran results

2 NOTES

```
New submission
```

This is a new submission

```
Possibly misspelled words in DESCRIPTION:
  Geoms (3:26)
  geoms (10:33)
```

"geoms" is not misspelled.  Other CRAN packages such as {ggforce} 
use it in their DESCRIPTION without quotes.
