# Helpers to write a pattern geom from an existing `{ggplot2}` geom.
# They aren't exported (yet):
#
# * `make_GeomPattern()` makes a pattern `Geom` from a `Geom`
#   (whose `geom_*_pattern()` function is then made by `ggplot2::make_constructor()`),
# * the `Layer` method of `gridpattern::with_pattern()` makes a pattern layer from a layer
#   e.g. `with_pattern(geom_rect(), pattern_fill = "blue")`
#   (it is registered but we don't re-export `with_pattern()` (yet)),
#   and its `dotstackGrob` method fills the dots of `ggplot2::geom_dotplot()` when they are drawn,
# * `with_pattern_aes()` fills the areas of a grob within a `draw_panel()` / `draw_group()` method,
# * `with_pattern_key()` fills the areas of a key glyph within a `draw_key()` method.

# Columns that can't be passed on as pattern parameters:
# `gridpattern::with_pattern()` rejects these names (they're its (or `patternGrob()`'s) own arguments)
# and `PANEL` is a factor (which patterns aren't expected to handle).
# (A layer's `x` and `y` were never passed on to patterns.)
with_pattern_reserved <- c("x", "y", "id", "name", "gp", "vp", "boundary", "default.units", "draw",
                           "pool", "layers", "delay", "leaves", "legend", "prefix", "PANEL")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill each area of a grob with the pattern of its row of `data`
#'
#' A wrapper around [gridpattern::with_pattern()] taking a layer's data:
#' its pattern aesthetics (e.g. `pattern` and `pattern_fill`) are the pattern parameters
#' and its other columns (e.g. `fill` and `colour`) are passed on as well
#' for the (custom) patterns that use them
#' (except the `x` and `y` positions, `PANEL`, and the names [gridpattern::with_pattern()] reserves).
#'
#' @param grob A grid grob (usually the grob drawn by the parent `{ggplot2}` geom).
#' @param data A data frame of aesthetics with either
#'             one row per area of `grob` (in the order of [gridpattern::area_boundaries()])
#'             (or per unique value of `pool`) or a single row used for every area.
#' @param pool,legend Passed to [gridpattern::with_pattern()].
#' @param delay If `TRUE` the areas of `grob` are only filled when it is drawn:
#'              for a grob whose areas only exist then
#'              (e.g. one that makes its children in a `makeContent()` method).
#'              The `dotstackGrob` of [ggplot2::geom_dotplot()] (whose dots are made when it is drawn)
#'              is always filled when it is drawn.
#'              Then a wrong number of rows is only an error when the grob is drawn.
#' @return A grob.
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern_aes <- function(grob, data, pool = FALSE, legend = FALSE, delay = FALSE) {
    if (!is.data.frame(data)) data <- vctrs::new_data_frame(as.list(data))
    params <- data[!names(data) %in% with_pattern_reserved]
    # (`gridpattern::with_pattern()` checks there is one row per area (or pool) or a single row)
    # (the `pattern` column is the pattern name and "stripe" is used if there is none)
    gridpattern::with_pattern(grob, !!!params, pool = pool, legend = legend, delay = delay, prefix = "")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill each area of a key glyph with the pattern of its key
#'
#' @param key A grid grob: a `{ggplot2}` key glyph e.g. as drawn by [ggplot2::draw_key_polygon()].
#' @param data,size,aspect_ratio As in [draw_key_polygon_pattern()].
#' @return A grob.
#' @noRd
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
#' The pattern geom draws the grob of `geom` and fills each of its areas with a pattern,
#' has our pattern aesthetics as well as those of `geom`,
#' and draws the key glyph of `geom` filled with the key's pattern.
#' Use [ggplot2::make_constructor()] to make its `geom_*_pattern()` function
#' (after assigning it to a variable named `name`).
#'
#' @param geom A [ggplot2::Geom] object e.g. `ggplot2::GeomRect`.
#' @param name The class name of the pattern geom.
#' @param rows How the rows of the data frame the geom's `draw_group()` method is given
#'             (or else its `draw_panel()` method if it doesn't have its own `draw_group()` method)
#'             are matched to the areas of the grob it draws. One of:
#'
#'   * `"trace"`: works for most geoms without knowing how they draw their areas (but is the slowest).
#'     The geom is given a different `fill` for each kind of row
#'     and each area is then filled with the pattern of the row whose `fill` it was drawn with
#'     (and given back its actual `fill`).
#'     Areas that weren't drawn with a row's `fill` are left alone.
#'     Needs the geom to draw a row's area with its (character) `fill` (possibly with an alpha).
#'     If there are no such areas when the panel is drawn this is tried again when the grob is drawn
#'     (so `delay` isn't needed).
#'   * `"all"`: the geom draws an area per row (in row order) e.g. `ggplot2::GeomRect`.
#'   * `"group"`: the geom draws an area per group (sorted by group) e.g. `ggplot2::GeomPolygon`.
#'   * `"constant"`: the geom's areas all have the pattern of the first row
#'     (it is an error if the pattern aesthetics vary) e.g. `ggplot2::GeomRibbon`.
#'   * A function of the data frame returning the rows of the areas
#'     (in the order of [gridpattern::area_boundaries()]) or a single row for every area.
#'
#'   By default what we know the draw method of `geom` needs (if it is a `{ggplot2}` one) or else `"trace"`.
#' @param delay Use `TRUE` for a geom whose grob only has its areas when it is drawn (see [with_pattern_aes()]).
#'              Not needed if `rows` is `"trace"`
#'              nor for `ggplot2::GeomDotplot` (whose `dotstackGrob` is always filled when drawn).
#' @return A [ggplot2::Geom] object.
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_GeomPattern <- function(geom, name = paste0(class(geom)[1L], "Pattern"), rows = NULL, delay = FALSE) {
    if (!inherits(geom, "Geom")) {
        cli::cli_abort("{.arg geom} must be a {.cls Geom} object, not {.obj_type_friendly {geom}}.")
    }
    stopifnot(is.logical(delay), length(delay) == 1L, !is.na(delay))
    rows <- rows %||% known_rows(geom)
    if (!is.function(rows)) {
        rows <- arg_match0(rows, c("trace", names(rows_functions)), "rows")
        if (rows != "trace") rows <- rows_functions[[rows]]
    }
    # wrap the method that draws the areas: `draw_group()` if `geom` has its own else `draw_panel()`
    has_draw_group <- !identical(ggproto_fn(geom$draw_group), ggproto_fn(Geom$draw_group))
    method <- if (has_draw_group) "draw_group" else "draw_panel"
    # with the formals of the parent's method (which `Geom$parameters()` reads) all passed on to it
    fmls <- formals(ggproto_fn(geom[[method]]))
    if (!"self" %in% names(fmls)) fmls <- c(alist(self = ), fmls)
    args <- setdiff(names(fmls), "self")
    # (the data, panel parameters, and coord by position since their names may differ)
    passed <- lapply(args, as.name)
    names(passed) <- ifelse(args == "..." | seq_along(args) <= 3L, "", args)
    parent_call <- as.call(c(call("$", quote(ggproto_parent(geom, self)), as.name(method)), passed))
    data_arg <- as.name(args[1L])
    # (the parent's method as a function of its data since tracing draws it with other fills)
    draw <- call("function", as.pairlist(set_names(alist(x = ), args[1L])), parent_call)
    body <- bquote({
        draw <- .(draw)
        draw_with_pattern(draw, .(data_arg), rows, delay)
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

# the function of a ggproto method
ggproto_fn <- function(method) environment(method)$f %||% method

# The rows of the areas a geom draws for `data` (see the `rows` of `make_GeomPattern()`)
rows_functions <- list(
    all = function(data) data,
    group = function(data) first_rows_by_group(data),
    constant = function(data) {
        if (nrow(unique0(data[names(data) %in% names(pattern_aesthetics)])) > 1L) {
            cli::cli_abort("Pattern aesthetics can not vary within an area.")
        }
        data[1L, , drop = FALSE]
    }
)

# The `{ggplot2}` geoms whose draw methods we know the `rows` of
known_rows_geoms <- c(
    GeomRect = "all", GeomTile = "all", GeomBar = "all", GeomCol = "all", GeomBin2d = "all",
    GeomCrossbar = "all", GeomBoxplot = "all", GeomHex = "all", GeomLabel = "all", GeomDotplot = "all",
    GeomPolygon = "group", GeomContourFilled = "group", GeomDensity2dFilled = "group",
    GeomRibbon = "constant", GeomArea = "constant", GeomDensity = "constant", GeomViolin = "constant"
)

# The `rows` of `geom` if it draws with the methods of a `{ggplot2}` geom we know them for else "trace"
known_rows <- function(geom) {
    for (name in intersect(class(geom), names(known_rows_geoms))) {
        known <- getExportedValue("ggplot2", name)
        if (identical(ggproto_fn(geom$draw_panel), ggproto_fn(known$draw_panel)) &&
            identical(ggproto_fn(geom$draw_group), ggproto_fn(known$draw_group))) {
            return(known_rows_geoms[[name]])
        }
    }
    "trace"
}

# The grob `draw(data)` draws with its areas filled with the patterns of their rows
draw_with_pattern <- function(draw, data, rows, delay = FALSE) {
    if (is.function(rows)) {
        with_pattern_aes(draw(data), rows(data), delay = delay)
    } else {
        draw_traced(draw, data)
    }
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
#' The `Layer` method of [gridpattern::with_pattern()]
#' e.g. `with_pattern(geom_rect(), pattern_fill = "blue")`:
#' the layer's geom is replaced by our pattern geom for it if we have one
#' (e.g. `GeomRectPattern` for `ggplot2::GeomRect`)
#' or else by the pattern geom [make_GeomPattern()] makes from it.
#'
#' @param x A `{ggplot2}` layer e.g. `ggplot2::geom_rect()`.
#' @param ... Pattern aesthetics set to a constant e.g. `pattern = "circle"` or `pattern_fill = "blue"`.
#' @param mapping Pattern aesthetic mappings (created by [ggplot2::aes()]) added to those of the layer.
#' @param rows,delay Passed to [make_GeomPattern()] (if given the pattern geom is always made by it).
#' @return A `{ggplot2}` layer.
#' @noRd
#' @exportS3Method gridpattern::with_pattern
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern.Layer <- function(x, ..., mapping = NULL, rows = NULL, delay = FALSE) {
    aes_params <- list2(...)
    unknown <- setdiff(names2(aes_params), names(pattern_aesthetics))
    if (length(unknown) > 0L) {
        cli::cli_abort(c(
            "{.arg ...} must be named pattern aesthetics.",
            x = "Not {cli::qty(length(unknown))} {?a/} pattern aesthetic{?s}: {.val {unknown}}."
        ))
    }
    geom <- if (is.null(rows) && isFALSE(delay)) our_pattern_geom(x$geom)
    geom <- geom %||% make_GeomPattern(x$geom, rows = rows, delay = delay)
    out <- ggproto(NULL, x, geom = geom)
    out$aes_params <- c(x$aes_params[setdiff(names(x$aes_params), names(aes_params))], aes_params)
    if (!is.null(mapping)) {
        new <- x$mapping %||% aes()
        new[names(mapping)] <- mapping
        out$mapping <- new
    }
    out
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
