#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pattern scale palettes in the theme
#'
#' Like `{ggplot2}`'s own scales our `scale_pattern_*_discrete()` and
#' `scale_pattern_*_continuous()` scales (e.g. the default scales used when a pattern aesthetic
#' is mapped without adding a scale)
#' look up their palette in the `palette.<aesthetic>.<type>` theme element
#' (e.g. `palette.pattern_type.discrete` or `palette.pattern_angle.continuous`)
#' when it isn't given by the scale's arguments.
#'
#' A scale's palette is the first of:
#'
#' 1. Its `palette` argument.
#' 2. Its `choices` or `range` argument (or other arguments like `solid` that set a palette).
#' 3. The `palette.<aesthetic>.discrete` or `palette.<aesthetic>.continuous` theme element.
#' 4. The scale's default palette (some scales such as [scale_pattern_type_discrete()] have none
#'    and throw an error when the scale is used without one).
#'
#' A theme palette element (like a scale's `palette` argument) must be one of:
#'
#' * A palette function e.g. `scales::pal_rescale(c(0, 45))` for a continuous scale
#'   or `function(n) c(0, 45, 90)[seq_len(n)]` for a discrete scale.
#' * For `r md_code_list(pattern_colour_aesthetics)`:
#'   a character vector of colours or the name of a palette (see [scales::as_discrete_pal()]
#'   and [scales::as_continuous_pal()]).
#' * For the discrete palettes of aesthetics that can take character values
#'   (`r md_code_list(pattern_character_aesthetics)`):
#'   a character vector of values (used in order, see [scales::pal_manual()]).
#'
#' Other vectors of values (e.g. numbers such as `pch` shapes for `pattern_shape`)
#' aren't converted into palettes (`{ggplot2}`'s own `palette.shape.discrete` has the same limitation):
#' wrap them in [scales::pal_manual()] for a discrete palette
#' (e.g. `theme(palette.pattern_shape.discrete = scales::pal_manual(c(1, 2, 3)))`)
#' or use the scale's `choices` or `range` argument instead.
#'
#' `pattern_colour`, `pattern_fill`, and `pattern_fill2` have their own theme elements
#' (e.g. `palette.pattern_fill.discrete`) and don't use `palette.colour.*` or `palette.fill.*`
#' so that mapping `fill` and `pattern_fill` to the same variable doesn't draw
#' patterns the same colour as their background.
#' Their theme elements use the British spelling: `palette.pattern_colour.*` (not `palette.pattern_color.*`).
#'
#' These theme elements only affect scales and hence mapped aesthetics.
#' An unmapped pattern aesthetic still uses the pattern's own default.
#'
#' @examples
#' if (require("ggplot2")) {
#'   df <- data.frame(level = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#'   gg <- ggplot(df) +
#'     geom_col_pattern(
#'       aes(level, outcome, pattern_type = level, pattern_angle = outcome),
#'       pattern = "polygon_tiling", fill = "white", colour = "black"
#'     ) +
#'     theme_bw(18) +
#'     theme(
#'       palette.pattern_type.discrete = c("hexagonal", "rhombille", "pythagorean"),
#'       palette.pattern_angle.continuous = scales::pal_rescale(c(0, 45))
#'     )
#'   plot(gg)
#' }
#' @name pattern_palettes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL

# The pattern aesthetics that have scales and hence `palette.<aes>.<type>` theme elements
# (all but `pattern_option_1` ... `pattern_option_5`, see #177)
pattern_palette_aesthetics <- function() {
  ns <- topenv()
  has_scales <- function(aes) {
    exists(paste0("scale_", aes, "_discrete"), envir = ns, inherits = FALSE) &&
      exists(paste0("scale_", aes, "_continuous"), envir = ns, inherits = FALSE)
  }
  Filter(has_scales, names(pattern_aesthetics))
}

pattern_colour_aesthetics <- c("pattern_colour", "pattern_fill", "pattern_fill2")

# The pattern aesthetics that can take character values (and hence whose discrete palettes can be
# a character vector of values).  `pattern_shape` and `pattern_linetype` can also be numeric
# (e.g. `pch` shapes) but `{scales}` can't convert a numeric vector into a palette
# (not even for `{ggplot2}`'s own `palette.shape.discrete`) so those need `scales::pal_manual()`.
pattern_character_aesthetics <- c("pattern", "pattern_type", "pattern_subtype",
                                  "pattern_linetype", "pattern_shape",
                                  "pattern_filename", "pattern_filter", "pattern_gravity",
                                  "pattern_orientation", "pattern_grid", "pattern_units")

# The classes a palette (theme element or `palette` argument) of `aesthetic` may have.
# A vector of values only works where `scales::as_discrete_pal()` or `scales::as_continuous_pal()`
# can convert it into a palette:
# colours, or a character vector of values (`scales::pal_manual()`) for a discrete palette.
pattern_palette_class <- function(aesthetic, type) {
  if (aesthetic %in% pattern_colour_aesthetics ||
      (type == "discrete" && aesthetic %in% pattern_character_aesthetics)) {
    c("character", "function")
  } else {
    "function"
  }
}

pattern_palette_element_tree <- function() {
  tree <- list()
  for (aes in pattern_palette_aesthetics()) {
    for (type in c("discrete", "continuous")) {
      tree[[paste0("palette.", aes, ".", type)]] <- el_def(pattern_palette_class(aes, type))
    }
  }
  tree
}

# A scale's palette from its `palette` argument else from its `values` (e.g. `choices` or `range`)
# else `NULL` (so the scale uses the theme element else its `fallback.palette`).
# `values_arg` is the name of the scale's argument for `values` (if any) used in error messages.
as_scale_palette <- function(palette = NULL, values = NULL, pal_values = NULL,
                             type = c("discrete", "continuous"), aesthetic, values_arg = NULL,
                             call = caller_env()) {
  type <- match.arg(type)
  if (!is.null(palette)) {
    check_palette(palette, aesthetic, type, values_arg, call = call)
    switch(type,
           discrete = scales::as_discrete_pal(palette),
           continuous = scales::as_continuous_pal(palette))
  } else if (!is.null(values)) {
    pal_values(values)
  } else {
    NULL
  }
}

# Markdown list of code e.g. for roxygen inline code
md_code_list <- function(x) cli::ansi_collapse(paste0("`", x, "`"))

check_palette <- function(palette, aesthetic, type, values_arg = NULL, call = caller_env()) {
  if (inherits(palette, pattern_palette_class(aesthetic, type))) {
    return(invisible(NULL))
  }
  manual <- paste0("scale_", aesthetic, "_manual")
  hints <- c(if (type == "discrete") "{.fn scales::pal_manual} (e.g. {.code palette = scales::pal_manual(values)})",
             if (!is.null(values_arg)) "the {.arg {values_arg}} argument",
             "{.fn {manual}}")
  hints <- cli::ansi_collapse(hints, sep2 = " or ", last = ", or ")
  cli::cli_abort(c("{.arg palette} must be a palette function, not {.obj_type_friendly {palette}}.",
                   i = paste0("For a vector of values use ", hints, ".")),
                 call = call)
}

# Discrete palette that spreads the `choices` across the levels
pal_choices_discrete <- function(choices) {
  force(choices)
  function(n) {
    idx <- cut(seq(n), length(choices), labels = FALSE, include.lowest = TRUE)
    choices[idx]
  }
}

# Continuous palette that picks from the `choices` by splitting [0, 1] into equal-width bins.
# Like `{ggplot2}`'s binned scales (and `pal_choices_discrete()`) the bins are closed on the right
# (a value on an edge goes in the lower bin) and the minimum goes in the first bin.
# A value within rounding error of an edge counts as on it
# (e.g. a rescaled 0.6 times 5 bins is 3.0000000000000004).
# Like the `fuzz` of `hist()` (which defaults to 1e-7 of a bin width) this means a value
# less than `sqrt(.Machine$double.eps)` of a bin width above an edge also goes in the lower bin.
# Values outside [0, 1] (e.g. with `oob = scales::oob_keep`) are `NA` like in `{ggplot2}`'s binned scales.
pal_choices_continuous <- function(choices) {
  force(choices)
  n <- length(choices)
  function(x) {
    x[x < 0 | x > 1] <- NA
    choices[pmax(ceiling(x * n - sqrt(.Machine$double.eps)), 1)]
  }
}

# Discrete palette of evenly spaced values within `range`
pal_seq_range <- function(range) {
  force(range)
  function(n) seq(range[1], range[2], length.out = n)
}

# Fallback palette for scales with no default palette
pal_required <- function(aesthetic, type, arg) {
  fn <- paste0("scale_", aesthetic, "_", type)
  force(arg)
  function(...) {
    cli::cli_abort(c("{.fn {fn}} needs a palette.",
                     i = paste("Set its {.arg {arg}} or {.arg palette} argument",
                               "or the {.code palette.{aesthetic}.{type}} theme element.")),
                   call = NULL)
  }
}
