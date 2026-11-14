# Helpers to write a pattern geom from an existing `{ggplot2}` geom:
#
# * `make_GeomPattern()` (exported) makes a pattern `Geom` from a `Geom`
#   (whose `geom_*_pattern()` function is then made by `ggplot2::make_constructor()`),
# * the `Layer` method of `gridpattern::with_pattern()` (which we re-export) makes a pattern layer from a layer
#   e.g. `with_pattern(geom_rect(), pattern_fill = "blue")`,
#   and its `dotstackGrob` method fills the dots of `ggplot2::geom_dotplot()` when they are drawn,
# * `with_pattern_aes()` (exported) fills the areas of a grob within a `draw_panel()` / `draw_group()` method,
# * `with_pattern_key()` (exported) fills the areas of a key glyph within a `draw_key()` method.

# Columns that can't be passed on as pattern parameters:
# `gridpattern::with_pattern()` rejects these names (they're its (or `patternGrob()`'s) own arguments)
# and `PANEL` is a factor (which patterns aren't expected to handle).
# (A layer's `x` and `y` were never passed on to patterns.)
with_pattern_reserved <- c("x", "y", "id", "name", "gp", "vp", "boundary", "default.units", "draw",
                           "pool", "layers", "delay", "leaves", "legend", "prefix", "PANEL")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill the areas of a grob with the patterns of their rows of aesthetics
#'
#' `with_pattern_aes()` fills each area of a grob
#' (e.g. the grob drawn by a `{ggplot2}` geom's draw method)
#' with the pattern of its row of a layer's data
#' and `with_pattern_key()` fills each area of a legend key glyph with the key's pattern.
#' They are wrappers around [gridpattern::with_pattern()] for writing the
#' `draw_panel()` / `draw_group()` and `draw_key()` methods of a pattern geom
#' (see `vignette("developing-pattern-geoms", package = "ggpattern")`).
#'
#' The pattern aesthetics (e.g. `pattern` and `pattern_fill`) are the pattern parameters
#' and the other columns (e.g. `fill` and `colour`) are passed on as well
#' for the (custom) patterns that use them,
#' except the `x` and `y` positions, `PANEL`,
#' and the names of the arguments of [gridpattern::with_pattern()] and [gridpattern::patternGrob()]
#' (`id`, `name`, `gp`, `vp`, `boundary`, `default.units`, `draw`, `pool`, `layers`, `delay`,
#' `leaves`, `legend`, and `prefix`).
#' The `pattern` column is the pattern's name (`"stripe"` if there isn't one).
#'
#' @param grob A grid grob (usually the grob drawn by the parent `{ggplot2}` geom).
#' @param data A data frame of aesthetics with either
#'   one row per area of `grob` (in the order of [gridpattern::area_boundaries()])
#'   (or per unique value of `pool`) or a single row used for every area.
#'   For `with_pattern_key()` the key's (single row) data.
#' @param pool,legend Passed to [gridpattern::with_pattern()].
#' @param delay If `TRUE` the areas of `grob` are only filled when it is drawn:
#'   for a grob whose areas only exist then
#'   (e.g. one that makes its children in a `makeContent()` method).
#'   The `dotstackGrob` of [ggplot2::geom_dotplot()] (whose dots are made when it is drawn)
#'   is always filled when it is drawn.
#'   Then a wrong number of rows is only an error when the grob is drawn.
#' @return A grob.
#' @seealso [make_GeomPattern()] to make a pattern geom from a geom without writing its draw methods.
#'   [default_pattern_aes()] for the default pattern aesthetics.
#' @examples
#' grob <- grid::rectGrob(x = c(0.25, 0.75), width = 0.4, height = 0.8,
#'                        gp = grid::gpar(fill = c("grey80", "grey60")))
#' data <- data.frame(pattern = c("stripe", "circle"), pattern_fill = c("blue", "red"))
#' grid::grid.newpage()
#' grid::grid.draw(with_pattern_aes(grob, data))
#'
#' # e.g. the `draw_key()` method of a pattern geom
#' draw_key <- function(data, params, size) {
#'     with_pattern_key(ggplot2::draw_key_polygon(data, params, size), data, size)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern_aes <- function(grob, data, pool = FALSE, legend = FALSE, delay = FALSE) {
    if (!is.data.frame(data)) data <- vctrs::new_data_frame(as.list(data))
    params <- data[!names(data) %in% with_pattern_reserved]
    # (`gridpattern::with_pattern()` checks there is one row per area (or pool) or a single row)
    # (the `pattern` column is the pattern name and "stripe" is used if there is none)
    gridpattern::with_pattern(grob, !!!params, pool = pool, legend = legend, delay = delay, prefix = "")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname with_pattern_aes
#' @param key A grid grob: a `{ggplot2}` key glyph e.g. as drawn by [ggplot2::draw_key_polygon()].
#' @param size,aspect_ratio As in [draw_key_polygon_pattern()]:
#'   the key's width and height (in mm) and the plot's aspect ratio.
#'   Call `with_pattern_key()` within a `draw_key()` method
#'   (when `{ggplot2}` draws the legend) so the default `aspect_ratio` is right.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern_key <- function(key, data, size, aspect_ratio = get_aspect_ratio()) {
    legend <- key_legend_factor(size, aspect_ratio)
    # (nothing to draw in a zero-size key)
    if (is.na(legend)) return(key)
    if (!is.data.frame(data)) data <- vctrs::new_data_frame(as.list(data))
    with_pattern_aes(key, data[1L, , drop = FALSE], legend = legend)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Make a pattern geom from a geom
#'
#' `make_GeomPattern()` makes a pattern geom from a geom:
#' it draws the grob of `geom` and fills each of its areas with a pattern,
#' has the pattern aesthetics as well as those of `geom`,
#' and draws the key glyph of `geom` filled with the key's pattern.
#' The grob it draws is named after the pattern geom (e.g. `"geom_smooth_pattern"` for `GeomSmoothPattern`).
#' Use [ggplot2::make_constructor()] to make its `geom_*_pattern()` function
#' or [with_pattern()] to make a pattern layer from a layer.
#'
#' A geom doesn't know the default `stat` and `position` of its `geom_*()` function
#' so pass them to [ggplot2::make_constructor()] if they aren't `"identity"`
#' e.g. `stat = "smooth"` for `ggplot2::geom_smooth()`
#' (otherwise the layer may fail with an error about missing aesthetics the stat would have computed).
#' [with_pattern()] keeps those of the layer it is given.
#'
#' The pattern geom stores a copy of `geom` as it is when `make_GeomPattern()` is called.
#' So if you call it at the top level of a package's code (which is run when the package is built)
#' with a geom from another package the pattern geom won't pick up later updates of that geom
#' until your package is reinstalled:
#' call it in your package's `.onLoad()` instead or write the pattern geom's draw methods yourself
#' with [with_pattern_aes()] (see `vignette("developing-pattern-geoms", package = "ggpattern")`).
#'
#' @param geom A [ggplot2::Geom] object e.g. `ggplot2::GeomSmooth`.
#' @param name The class name of the pattern geom.
#' @param area_by How the areas of the grob the geom draws are matched to the rows of the data frame
#'   its `draw_group()` method is given
#'   (or else its `draw_panel()` method if it doesn't have its own `draw_group()` method)
#'   i.e. which rows give the areas their patterns and in what order. One of:
#'
#'   * `"trace"`: works for most geoms without knowing how they draw their areas (but is the slowest).
#'     The geom is given a different `fill` for each kind of row
#'     and each area is then filled with the pattern of the row whose `fill` it was drawn with
#'     (and given back its actual `fill`).
#'     Areas that weren't drawn with a row's `fill` are left alone.
#'     Needs the geom to draw a row's area with its (character) `fill` (possibly with an alpha).
#'   * `"row"`: the geom draws an area per row (in row order) e.g. `ggplot2::GeomRect`.
#'   * `"group"`: the geom draws an area per group (sorted by group) with the pattern of its first row
#'     (it is an error if the pattern aesthetics vary within a group)
#'     e.g. `ggplot2::GeomPolygon` or a geom whose `draw_group()` method draws a single area
#'     e.g. `ggplot2::GeomRibbon`.
#'   * A function returning the rows of the areas
#'     (in the order of [gridpattern::area_boundaries()]) or a single row for every area.
#'     It is given the data frame (as its first argument)
#'     and the draw method's other parameters (e.g. `lineend`) by name
#'     if it has an argument of that name (or `...`).
#'
#'   By default what we know the draw method of `geom` needs (if it is a `{ggplot2}` one) or else `"trace"`.
#'   A grob whose areas are only made when it is drawn (e.g. in a `makeContent()` method)
#'   is filled when it is drawn.
#'
#'   Filling areas other than those of a [grid::polygonGrob()], [grid::pathGrob()], or [grid::rectGrob()]
#'   (e.g. the round rectangles of a `{treemapify}` treemap) requires R (>= 4.2.0)
#'   (see [gridpattern::with_pattern()]).
#' @return A [ggplot2::Geom] object.
#' @seealso [with_pattern()] to make a pattern layer from a layer.
#' @examples
#' library("ggplot2")
#' GeomSmoothPattern <- make_GeomPattern(GeomSmooth)
#' geom_smooth_pattern <- make_constructor(GeomSmoothPattern, stat = "smooth")
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     geom_smooth_pattern(method = "lm", formula = y ~ x, pattern = "crosshatch", pattern_colour = "grey40")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_GeomPattern <- function(geom, name = paste0(class(geom)[1L], "Pattern"), area_by = NULL) {
    if (!inherits(geom, "Geom")) {
        cli::cli_abort("{.arg geom} must be a {.cls Geom} object, not {.obj_type_friendly {geom}}.")
    }
    area_by <- area_by %||% known_area_by(geom)
    if (!is.function(area_by)) {
        area_by <- arg_match0(area_by, c("trace", names(area_by_functions)), "area_by")
        label <- area_by
        if (area_by != "trace") area_by <- area_by_functions[[area_by]]
    } else {
        label <- NULL
    }
    # wrap the method that draws the areas: `draw_group()` if `geom` has its own else `draw_panel()`
    has_draw_group <- !identical(ggproto_fn(geom$draw_group), ggproto_fn(Geom$draw_group))
    method <- if (has_draw_group) "draw_group" else "draw_panel"
    # with the formals of the parent's method (which `Geom$parameters()` reads) all passed on to it
    fmls <- formals(ggproto_fn(geom[[method]]))
    if (!"self" %in% names(fmls)) fmls <- c(alist(self = ), fmls)
    # (the data, panel parameters, and coord renamed to `{ggplot2}`'s names
    #  which `Geom$parameters()` and `make_constructor()` leave out
    #  e.g. for a geom written for `{ggplot2}` (< 3.0.0) which called them `panel_scales`)
    fmls <- standardize_draw_formals(fmls)
    args <- setdiff(names(fmls), "self")
    # (the data, panel parameters, and coord by position since their names may differ
    #  unless after `...` e.g. `GeomViolin$draw_group(self, data, ..., quantile_gp, flipped_aes)`)
    passed <- lapply(args, as.name)
    dots <- match("...", args, nomatch = length(args) + 1L)
    by_position <- args == "..." | (seq_along(args) <= 3L & seq_along(args) < dots)
    names(passed) <- ifelse(by_position, "", args)
    # (the draw method's parameters passed by name to an `area_by` function)
    params_call <- as.call(c(list(as.name("list")), set_names(passed[!by_position], args[!by_position])))
    parent_call <- as.call(c(call("$", quote(ggproto_parent(geom, self)), as.name(method)), passed))
    data_arg <- as.name(args[1L])
    # (the parent's method as a function of its data since tracing draws it with other fills)
    draw <- call("function", as.pairlist(set_names(alist(x = ), args[1L])), parent_call)
    body <- bquote({
        draw <- .(draw)
        grob <- draw_with_pattern(draw, .(data_arg), area_by, .(params_call),
                                  geom = class(self)[1L], label = label)
        # named after the pattern geom like `{ggplot2}` names the grobs of its geoms e.g. "geom_rect_pattern"
        # (unless it isn't a single grob e.g. the `gList` `ggplot2::GeomSf` may draw)
        if (is.grob(grob)) ggname(snake_class(self), grob) else grob
    })
    members <- list(
        default_aes = defaults(geom$default_aes, pattern_aesthetics),
        draw_key = function(data, params, size) {
            with_pattern_key(geom$draw_key(data, params, size), data, size)
        }
    )
    members[[method]] <- new_function(fmls, body, env = environment())
    inject(ggproto(name, geom, !!!members))
}

# The formals of a draw method with its data, panel parameters, and coord
# (its first three arguments after `self` before any `...`, passed by position) given `{ggplot2}`'s names
standardize_draw_formals <- function(fmls) {
    nms <- names(fmls)
    args <- which(nms != "self")
    dots <- match("...", nms[args], nomatch = length(args) + 1L)
    n <- min(3L, dots - 1L)
    if (n < 1L) return(fmls)
    standard <- c("data", "panel_params", "coord")[seq_len(n)]
    old <- nms[args[seq_len(n)]]
    # (unless one of the names is already the name of another argument)
    if (any(standard %in% setdiff(nms, old))) return(fmls)
    nms[args[seq_len(n)]] <- standard
    names(fmls) <- nms
    fmls
}

# the function of a ggproto method
ggproto_fn <- function(method) environment(method)$f %||% method

# The rows of the areas a geom draws for `data` (see the `area_by` of `make_GeomPattern()`)
area_by_functions <- list(
    row = function(data) data,
    group = function(data) {
        params <- data[names(data) %in% c("group", names(pattern_aesthetics))]
        if (nrow(unique0(params)) > length(unique0(data$group))) {
            cli::cli_abort("Pattern aesthetics can not vary within an area.")
        }
        first_rows_by_group(data)
    }
)

# The `{ggplot2}` geoms whose draw methods we know the `area_by` of
known_area_by_geoms <- c(
    GeomRect = "row", GeomTile = "row", GeomBar = "row", GeomCol = "row", GeomBin2d = "row",
    GeomCrossbar = "row", GeomBoxplot = "row", GeomHex = "row", GeomLabel = "row", GeomDotplot = "row",
    GeomPolygon = "group", GeomContourFilled = "group", GeomDensity2dFilled = "group",
    # (their `draw_group()` method draws a single area)
    GeomRibbon = "group", GeomArea = "group", GeomDensity = "group", GeomViolin = "group"
)

# The `area_by` of `geom` if it draws with the methods of a `{ggplot2}` geom we know it for else "trace"
known_area_by <- function(geom) {
    for (name in intersect(class(geom), names(known_area_by_geoms))) {
        known <- getExportedValue("ggplot2", name)
        if (identical(ggproto_fn(geom$draw_panel), ggproto_fn(known$draw_panel)) &&
            identical(ggproto_fn(geom$draw_group), ggproto_fn(known$draw_group))) {
            return(known_area_by_geoms[[name]])
        }
    }
    "trace"
}

# The grob `draw(data)` draws with its areas filled with the patterns of their rows
# (`params` are the draw method's parameters for an `area_by` function,
#  `geom` and `label` (the `area_by` preset or `NULL` for a function) are for error messages)
draw_with_pattern <- function(draw, data, area_by, params = list(), geom = "Geom", label = NULL) {
    if (!is.function(area_by)) return(draw_traced(draw, data))
    grob <- draw(data)
    rows <- call_area_by(area_by, data, params)
    # (a grob that only makes its areas when it is drawn is filled then)
    delay <- length(gridpattern::area_boundaries(grob)) == 0L
    with_area_mismatch_context(with_pattern_aes(grob, rows, delay = delay), geom, label, delay)
}

# `area_by(data)` also given the `params` it has an argument for (or all of them if it has `...`)
call_area_by <- function(area_by, data, params) {
    fmls <- names(formals(args(area_by)))[-1L]
    if (!"..." %in% fmls) params <- params[names(params) %in% fmls]
    inject(area_by(data, !!!params))
}

# `expr` (a grob) with `{gridpattern}`'s wrong number of rows per area error
# re-raised to say which geom and `area_by` (when `delay` the error is only raised when drawn)
with_area_mismatch_context <- function(expr, geom, label, delay = FALSE) {
    grob <- try_fetch(expr, gridpattern_area_mismatch = function(cnd) {
        abort_area_mismatch(cnd, geom, label)
    })
    if (!delay) return(grob)
    gTree(children = gList(grob), geom = geom, label = label, cl = "ggpattern_area_mismatch_context")
}

#' @export
makeContent.ggpattern_area_mismatch_context <- function(x) {
    grob <- try_fetch(grid::forceGrob(x$children[[1L]]), gridpattern_area_mismatch = function(cnd) {
        abort_area_mismatch(cnd, x$geom, x$label)
    })
    setChildren(x, gList(grob))
}

abort_area_mismatch <- function(cnd, geom, label) {
    # (with `pool = TRUE` {gridpattern} counts grobs not areas)
    units <- paste0(cnd$unit, if (cnd$n_expected != 1L) "s")
    area_by <- if (is.null(label)) "the {.arg area_by} function" else "{.code area_by = \"{label}\"}"
    cli::cli_abort(c(
        paste0("{.cls {geom}} drew {cnd$n_expected} {units} but ", area_by,
               " gave {cnd$n_given} row{?s}."),
        i = "Does it draw its areas in a different way? Try {.code area_by = \"trace\"}."
    ), parent = cnd, call = NULL, class = "ggpattern_area_mismatch")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tracing which row each area was drawn for by its fill
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The aesthetics a row's pattern (and so its tracer fill) may depend on:
# rows with the same ones get the same tracer fill
# so e.g. a ribbon (whose rows must all have the same fill) is still drawn as one
trace_aesthetics <- function() {
    c("group", "subgroup", "fill", "colour", "alpha", "linewidth", "linetype", "size", "shape", "stroke",
      names(pattern_aesthetics))
}

tracer_fill <- function(id) sprintf("#%06X", id)

# The id of each tracer `fill` (`NA` if it isn't one)
# and the alpha the geom gave it (`NA` if none)
tracer_id <- function(fill) {
    is_tracer <- is.character(fill) & grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", fill)
    id <- rep_len(NA_integer_, length(fill))
    id[is_tracer] <- strtoi(substr(fill[is_tracer], 2L, 7L), 16L)
    alpha <- rep_len(NA_real_, length(fill))
    has_alpha <- is_tracer & nchar(fill) == 9L
    alpha[has_alpha] <- strtoi(substr(fill[has_alpha], 8L, 9L), 16L) / 255
    list(id = id, alpha = alpha)
}

# `grob` with `f()` applied to each of its (character) fills
map_fill <- function(grob, f) {
    if (inherits(grob, "gList")) {
        grob[] <- lapply(grob, map_fill, f)
        return(grob)
    }
    if (is.character(grob$gp$fill)) grob$gp$fill <- f(grob$gp$fill)
    if (inherits(grob, "gTree") && length(grob$children) > 0L) {
        grob$children[] <- lapply(grob$children, map_fill, f)
    }
    grob
}

# The grob `draw(data)` draws with each area filled with the pattern of the row whose fill it was drawn with
draw_traced <- function(draw, data) {
    fill <- data$fill
    # (can't trace other fills (e.g. a list of gradients): assume an area per row)
    if (nrow(data) == 0L || is.null(fill) || !(is.character(fill) || all(is.na(fill)))) {
        return(with_pattern_aes(draw(data), data))
    }
    id <- vec_group_id(data[intersect(trace_aesthetics(), names(data))])
    first <- match(seq_len(attr(id, "n")), id)
    traced <- data
    traced$fill <- tracer_fill(id)
    grob <- draw(traced)
    out <- untrace(grob, data, first)
    if (!is.null(out)) return(out)
    # no area was drawn with a row's fill: maybe they will be when the grob is drawn
    # (if still not it is drawn with its actual fills instead)
    gTree(traced = grob, plain = draw(data), data = data, first = first, cl = "ggpattern_traced")
}

# The traced `grob` given back its actual fills
# with each area filled with the pattern of the row whose tracer fill it has
# (`NULL` if no area has a tracer fill).
# `first` is the row of each tracer fill.
untrace <- function(grob, data, first) {
    areas <- gridpattern::area_boundaries(grob)
    actual <- function(fill) {
        tracer <- tracer_id(fill)
        row <- first[tracer$id]
        is_traced <- !is.na(row)
        fill[is_traced] <- ifelse(is.na(tracer$alpha[is_traced]), data$fill[row[is_traced]],
                                  fill_alpha(data$fill[row[is_traced]], tracer$alpha[is_traced]))
        fill
    }
    rows <- vapply(areas, function(area) {
        fill <- area$gp$fill
        if (is.character(fill) && length(fill) == 1L) first[tracer_id(fill)$id] else NA_integer_
    }, integer(1L))
    if (all(is.na(rows))) return(NULL)
    # (whether each is still an area with its actual fill)
    is_area <- vapply(areas, function(area) {
        length(gridpattern::area_boundaries(map_fill(area, actual))) > 0L
    }, logical(1L))
    rows <- rows[is_area]
    grob <- map_fill(grob, actual)
    if (all(is.na(rows))) return(grob)
    # (areas that weren't drawn with a row's fill are left alone)
    params <- data[ifelse(is.na(rows), 1L, rows), , drop = FALSE]
    params$pattern <- as.character(params$pattern %||% "stripe")
    params$pattern[is.na(rows)] <- NA_character_
    with_pattern_aes(grob, params)
}

#' @export
makeContent.ggpattern_traced <- function(x) {
    grob <- untrace(grid::forceGrob(x$traced), x$data, x$first) %||% x$plain
    setChildren(x, gList(grob))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Make a pattern layer from a layer
#'
#' The `Layer` method of [with_pattern()] makes a pattern layer from a `{ggplot2}` layer:
#' the layer's geom is replaced by our pattern geom for it if we have one
#' (e.g. `GeomRectPattern` for `ggplot2::GeomRect`)
#' or else by the pattern geom [make_GeomPattern()] makes from it.
#'
#' Some `{ggplot2}` functions return a list instead of a layer
#' e.g. `ggplot2::geom_sf()` returns a list of a layer and a `ggplot2::coord_sf()`.
#' The `list` method of [with_pattern()] makes a pattern layer from each layer in such a list
#' and the `gg` method leaves any other `{ggplot2}` object (which has no areas to fill)
#' as it is (whatever the other arguments)
#' e.g. a coord, scale, facet, theme, `ggplot2::labs()`, or `ggplot2::guides()`
#' (a `NULL` in the list is left as it is too).
#' So e.g. `with_pattern(geom_sf(), pattern = "circle")` works too.
#' It is an error to give it a plot: give it the plot's layers instead
#' e.g. `ggplot(df, aes(x, y)) + with_pattern(geom_col())`.
#'
#' To combine it with a `{ggfx}` filter make the pattern layer first
#' e.g. `ggfx::with_blur(with_pattern(geom_col()))`:
#' the layer of `with_pattern(ggfx::with_blur(geom_col()))` is drawn without its patterns
#' (since `{ggfx}` draws the layer with the draw methods of the geom it was given).
#'
#' @param x A `{ggplot2}` layer e.g. `ggplot2::geom_smooth()`
#'   (or another `{ggplot2}` object, which is returned as it is).
#' @param ... Pattern aesthetics set to a constant e.g. `pattern = "circle"` or `pattern_fill = "blue"`.
#' @param mapping Pattern aesthetic mappings (created by [ggplot2::aes()]) added to those of the layer.
#' @param area_by Passed to [make_GeomPattern()] (if given the pattern geom is always made by it).
#' @return A `{ggplot2}` layer (or the other `{ggplot2}` object).
#' @seealso [make_GeomPattern()] to make a pattern geom from a geom.
#' @examples
#' library("ggplot2")
#' ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     with_pattern(geom_smooth(method = "lm", formula = y ~ x), pattern = "crosshatch", pattern_colour = "grey40")
#' @exportS3Method gridpattern::with_pattern
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern.Layer <- function(x, ..., mapping = NULL, area_by = NULL) {
    aes_params <- list2(...)
    unknown <- setdiff(names2(aes_params), names(pattern_aesthetics))
    if (length(unknown) > 0L) {
        cli::cli_abort(c(
            "{.arg ...} must be named pattern aesthetics.",
            x = "Not {cli::qty(length(unknown))} {?a/} pattern aesthetic{?s}: {.val {unknown}}."
        ))
    }
    geom <- if (is.null(area_by)) our_pattern_geom(x$geom)
    geom <- geom %||% make_GeomPattern(x$geom, area_by = area_by)
    out <- ggproto(NULL, x, geom = geom)
    out$aes_params <- c(x$aes_params[setdiff(names(x$aes_params), names(aes_params))], aes_params)
    if (!is.null(mapping)) {
        new <- x$mapping %||% aes()
        new[names(mapping)] <- mapping
        out$mapping <- new
    }
    out
}

# Any other `{ggplot2}` object (e.g. a coord, scale, facet, theme, labels, or guides) has no areas to fill:
# returned as it is (whatever the other arguments)
# so that `gridpattern::with_pattern()`'s `list` method can be given a list of them and layers
# (e.g. what `ggplot2::geom_sf()` returns).
#' @rdname with_pattern.Layer
#' @exportS3Method gridpattern::with_pattern
with_pattern.gg <- function(x, ...) x

# (but a plot is most likely a mistake rather than something to leave as it is)
#' @rdname with_pattern.Layer
#' @exportS3Method gridpattern::with_pattern
with_pattern.ggplot <- function(x, ...) {
    cli::cli_abort(c(
        "{.fn with_pattern} can't make a pattern plot from a plot.",
        i = "Give it the plot's layers instead e.g. {.code ggplot(df, aes(x, y)) + with_pattern(geom_col())}."
    ))
}

# Our pattern geom for the `{ggplot2}` geom `geom` (`NULL` if we don't have one)
our_pattern_geom <- function(geom) {
    name <- class(geom)[1L]
    ours <- get0(paste0(name, "Pattern"), envir = asNamespace("ggpattern"), inherits = FALSE)
    if (!inherits(ours, "Geom") || !name %in% getNamespaceExports("ggplot2")) return(NULL)
    if (identical(geom, getExportedValue("ggplot2", name))) ours else NULL
}

# The dots of `ggplot2::geom_dotplot()`'s `dotstackGrob` are only made when it is drawn
# so its areas (a dot each) must be filled then.
#' @exportS3Method gridpattern::with_pattern
with_pattern.dotstackGrob <- function(x, ...) {
    NextMethod(delay = TRUE)
}
