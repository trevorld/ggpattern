% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-pattern-size.R
\name{scale_pattern_size_continuous}
\alias{scale_pattern_size_continuous}
\alias{scale_pattern_size}
\alias{scale_pattern_size_discrete}
\alias{scale_pattern_size_ordinal}
\title{Scales for area or radius}
\usage{
scale_pattern_size_continuous(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  range = c(1, 6),
  trans = deprecated(),
  guide = "legend",
  ...,
  transform = "identity"
)

scale_pattern_size(
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  limits = NULL,
  range = c(1, 6),
  trans = deprecated(),
  guide = "legend",
  ...,
  transform = "identity"
)
}
\arguments{
\item{name, breaks, labels, limits, trans, guide, ..., transform}{See \code{\link[ggplot2:scale_size]{ggplot2::scale_size()}} for more information}

\item{range}{a numeric vector of length 2 that specifies the minimum and
maximum size of the plotting symbol after transformation.}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Scale} object.
}
\description{
Scales for area or radius
}
\examples{
  if (require("ggplot2")) {
    # 'circle' pattern example
    df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
    gg <- ggplot(df) +
      geom_col_pattern(
        aes(level, outcome, pattern_fill = level,
            linewidth = outcome, pattern_size = outcome),
        pattern_density = 0.4,
        pattern_spacing = 0.3,
        pattern = 'circle',
        fill    = 'white',
        colour  = 'black'
      ) +
      theme_bw(18) +
      theme(legend.position = 'none') +
      scale_pattern_size() +
      labs(
        title    = "ggpattern::geom_col_pattern()",
        subtitle = "pattern = 'circle'"
      )
    plot(gg)
  }

}
