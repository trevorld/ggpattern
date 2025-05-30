% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpattern-package.R
\docType{package}
\name{ggpattern}
\alias{ggpattern-package}
\alias{ggpattern}
\title{ggpattern: 'ggplot2' Pattern Geoms}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Provides 'ggplot2' geoms filled with various patterns. Includes a patterned version of every 'ggplot2' geom that has a region that can be filled with a pattern. Provides a suite of 'ggplot2' aesthetics and scales for controlling pattern appearances. Supports over a dozen builtin patterns (every pattern implemented by 'gridpattern') as well as allowing custom user-defined patterns.
}
\section{Package options}{

The following \code{ggpattern} options may be set globally via \code{\link[base:options]{base::options()}}:
\describe{
\item{ggpattern_array_funcs}{Set custom \dQuote{array} pattern functions.}
\item{ggpattern_geometry_funcs}{Set custom \dQuote{geometry} pattern functions.}
\item{ggpattern_res}{Set new default raster resolution default (pixels per inch)
for the \code{pattern_res} aesthetic.}
\item{ggpattern_use_R4.1_clipping}{If \code{TRUE} use the grid clipping path feature introduced in R v4.1.0
else do a \code{rasterGrob} approximation of the clipped pattern.}
\item{ggpattern_use_R4.1_features}{If \code{TRUE} sets the default for all the other
\verb{ggpattern_use_R4.1_*} options arguments to \code{TRUE}.}
\item{ggpattern_use_R4.1_gradients}{If \code{TRUE} use the grid gradient feature introduced in R v4.1.0
else do a \code{rasterGrob} approximation of the gradient pattern.}
\item{ggpattern_use_R4.1_masks}{If \code{TRUE} use the grid mask feature introduced in R v4.1.0.
else do a \code{rasterGrob} approximation of the masked pattern.}
\item{ggpattern_use_R4.1_patterns}{If \code{TRUE} use the grid pattern feature introduced in R v4.1.0.
Available for use in writing custom patterns.}
}
Note to use the R v4.1.0 features one needs R be (at least) version 4.1 and not all graphic devices
support any/all these features.  See \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html} for more information on the R v4.1.0 grid features.
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/trevorld/ggpattern}
  \item \url{https://trevorldavis.com/R/ggpattern/}
  \item Report bugs at \url{https://github.com/trevorld/ggpattern/issues}
}

}
\author{
\strong{Maintainer}: Trevor L. Davis \email{trevor.l.davis@gmail.com} (\href{https://orcid.org/0000-0001-6341-4639}{ORCID})

Authors:
\itemize{
  \item Mike FC
  \item ggplot2 authors
}

}
\keyword{internal}
