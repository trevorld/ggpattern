Added missing package anchors to a few Rd cross-references.
These should eliminate the "Rd cross-references" check NOTEs on CRAN.

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
