should_cd <- basename(getwd()) != "vignettes"
if (should_cd) setwd("vignettes")
knitr::knit("geom-gallery-array.Rmd.orig", "geom-gallery-array.Rmd")
knitr::knit("geom-gallery-geometry.Rmd.orig", "geom-gallery-geometry.Rmd")
knitr::knit("patterns-image.Rmd.orig", "patterns-image.Rmd")
if (should_cd) setwd("..")
