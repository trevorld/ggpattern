In `vignettes/patterns-noise.Rmd` we now save images with
`png(type="cairo")` instead of `ragg::agg_png()` which should
prevent any UBSAN warnings occurring on CRAN checks

In order to keep the package tarball
under 5 MB we omit the package (visual) unit tests
and we omit seven package vignettes.

## Test environments

* win-builder (windows, R devel)
* local (linux, R 4.4.2)
* github actions (linux, R release)
* github actions (linux, R devel)

## R CMD check --as-cran results

OK

## revdepcheck results

We checked 7 reverse dependencies (0 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
