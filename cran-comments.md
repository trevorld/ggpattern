In under to keep the package tarball
under 5 MB we omit the package (visual) unit tests 
and we omit seven package vignettes.

## Test environments

* win-builder (windows, R devel)
* local (linux, R 4.4.0)
* github actions (linux, R release)
* github actions (linux, R devel)

## R CMD check --as-cran results

OK

## revdepcheck results

We checked 6 reverse dependencies (3 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
