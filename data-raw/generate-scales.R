suppressPackageStartupMessages({
  library("dplyr")
  library("glue")
})

sink("R/scale-pattern-auto.R")

cat("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("#\n")
cat("# All functions in this file are auto-generated by the script:\n")
cat("#     'data-raw/generate-scales.R'\n")
cat("#\n")
cat("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n\n")

source("data-raw/generate-scales-continuous-discrete.R")
source("data-raw/generate-scales-manual.R")
source("data-raw/generate-scales-identity.R")

sink()
