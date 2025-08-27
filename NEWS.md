# ggpattern 1.2.1

## New features

* Adds `guide` argument to `scale_pattern_angle_discrete()`, `scale_pattern_density_discrete()`, `scale_pattern_spacing_discrete()`, `scale_pattern_xoffset_discrete()`, `scale_pattern_yoffset_discrete()`, `scale_pattern_aspect_ratio_discrete()`, `scale_pattern_key_scale_factor_discrete()`, `scale_pattern_scale_discrete()`, `scale_pattern_phase_discrete()`, `scale_pattern_frequency_discrete()`, `scale_pattern_res_discrete()`, `scale_pattern_rot_discrete()` (#139).

## Bug fixes and minor improvements

* `GeomSfPattern` now extends `GeomSf` (#141).
* Fixed broken 'wave' pattern example in the *Patterns 'stripe', 'wave', 'crosshatch', 'weave' - Parameters and Examples* vignette (#136).
* Updated placeholder website links in the *Patterns 'image' and 'placeholder' - Parameters and Examples* vignette.

# ggpattern 1.1.4

## Bug fixes and minor improvements

* Updated a couple of examples to use the `linewidth` aesthetic
  instead of the `size` aesthetic which was deprecated in `{ggplot2}` 3.4.0.

# ggpattern 1.1.3

## Bug fixes and minor improvements

* Added alt text to vignette and README images.
* Added missing package anchors to a few Rd cross-references (#128).

# ggpattern 1.1.1

## Breaking changes

* The argument `binwidth` has been removed from `geom_bar_pattern()`.
  This argument was deprecated with ggpattern 0.1.0 (2020-04-01).
* The `draw_key_polygon_pattern()` called by `geom_sf_pattern()` is now passed in its
  actual aspect ratio (instead of 1).
  This may cause your legends to `geom_sf_pattern()` to look different.

## Deprecated features

* In line with upstream changes in `{ggplot2}` the `trans` argument has been deprecated in various scales functions.
  Use the new `transform` argument instead (#113).
* `outline.type = "legacy"` is now formally deprecated in
  `geom_area_pattern()` and `geom_ribbon_pattern()`.
  Use `outline.type = "full"` instead.
  `outline.type = "legacy"` was deprecated with ggpattern 0.1.0 (2020-04-01)
  but this is the first time `outline.type = "full"` has been supported as an alternative.

## New features

* Geoms now more consistently expose the `lineend` and `linejoin` parameters (#94).
  This matches a similar upstream change in ggplot2 3.4.0.
* `geom_area_pattern()` and `geom_ribbon_pattern()` now accept `outline.type = "lower"` and `outline.type = "full"`.
  `geom_density_pattern()` now exposes the `outline.type` parameter.
* `geom_bar_pattern()` and `geom_col_pattern()` now accept parameter `just`.
  The `just` parameter was added to `ggplot2::geom_bar()` and `ggplot2::geom_col()` with ggplot2 3.4.0.
* `geom_boxplot_pattern()` now accept the `outliers` and `staplewidth` parameters.
  The `outliers` and `staplewidth` parameters were added to `geom_boxplot()` in ggplot2 3.5.0.
* `geom_sf_pattern()` now accepts the `arrow` parameter.
  The `arrow` parameter was added to `geom_sf()` in ggplot2 3.5.0.
* `geom_violin_pattern()` now accepts the `bounds` parameter.
  The `bounds` parameter was added to `geom_violin()` in ggplot2 3.5.0.
* Each pattern aesthetic may now be a list of vectors with each list element
  providing that aesthetic for a different pattern (#100).
  Most builtin `{gridpattern}` "geometry" patterns support multiple fill colors etc. which previously we could only access in `{ggpattern}` via custom patterns.
* The `fill`, `pattern_fill`, and `pattern_fill2` aesthetics may now be (a list of) gradient/pattern fills
  (in addition to color strings) (#112).
  Note using gradient/pattern fills will require R (>= 4.2), a graphics device with support for the gradient/pattern fill feature,
  and will work only for a subset of patterns (i.e. most of the "geometry" patterns).
  Use of just color fills should continue to work on a wider variety of R versions, graphics devices, and patterns.
* {ggpattern} now supports the `pattern_units` aesthetic (#81).
  Supported by most "geometry" patterns.
  It sets the `grid::unit()` used by the `pattern_spacing`, `pattern_xoffset`, `pattern_yoffset`,
  and (for the "wave" pattern) the `pattern_frequency` aesthetics.
  Default is "snpc" while "cm" and "inches" are likely alternatives.

* `geom_bin_2d_pattern()` is now an alias for `geom_bin2d_pattern()`.
  This matches `{ggplot2}` which has both `geom_bin_2d()` and `geom_bin2d()`.

## Bug fixes and minor improvements

* `draw_key_boxplot_pattern()`, `draw_key_crossbar_pattern()`, and `draw_key_polygon_pattern()` `aspect_ratio` argument
  now defaults to `get_aspect_ratio()`.
* Several (continuous) scales now have an `...` argument which will be passed to `ggplot2::continuous_scale()`.
* The default `na.value` for `scale_pattern_continuous()`, `scale_pattern_discrete()`, and `scale_pattern_manual()` is now "none" (#107).

# ggpattern 1.0.1

## Deprecated features

* In line with upstream changes in `{ggplot2}` a `linewidth` aesthetic has been introduced and supersedes the `size` aesthetic for scaling the width of patterned area border lines (#82). 
 `size` will remain functioning but deprecated for these geoms and it is recommended to update all code to reflect the new aesthetic.  
  Continue to use the `size` aesthetic to adjust the size of any *points* in `geom_sf_pattern()`.

# ggpattern 1.0.0

## Bug fixes and minor improvements

* Fixes bug in "image" pattern when `pattern_type = "tile"` (#37).
  Transparent parts of tiled images are now consistently transparent.
  We now support the `pattern_gravity` aesthetic when `pattern_type = "tile"`.

# ggpattern 0.4.2

## Breaking changes

* To turn off "clipping" certain built-in patterns with the new R 4.1 graphic features 
  and instead use a raster image approximation one should now use
  `options(ggpattern_use_R4.1_masks = FALSE)` instead of `options(ggpattern_use_R4.1_clipping = FALSE)`.
  Alternatively, using `options(ggpattern_use_R4.1_features = FALSE)` to turn off all 
  R 4.1 features will continue to work.

* {ggpattern} removes the following data/functions
  which were deprecated in a previous version of {ggpattern}:

  * `convert_polygon_df_to_polygon_grob()`
  * `convert_polygon_df_to_polygon_sf()`
  * `convert_polygon_sf_to_polygon_df()`

* Defunct functions are no longer exported.

## New features

* {ggpattern} now supports `geom_histogram_pattern()` (#71)

## Bug fixes and minor improvements

* `geom_sf_pattern()` and `geom_polygon_pattern()` now support polygons with holes
  by using "alpha masking" instead of a "clipping path".
  If R 4.1 alpha mask feature is not supported by the active graphics device
  (or the masking feature is declined) we use a raster image approximation of the pattern.
  Resolution of the raster approximation can be adjusted by the `pattern_res` aesthetic
  whose default in turn can be adjusted by the `"ggpattern_res"` global option.

  This change fixes patterns filling holed polygons in certain graphic devices (#68).
  This change fixes patterns sometimes escaping plot window (#60).
* `draw_key_polygon_pattern()`, `GeomMapPattern`, `GeomPolygonPattern`, and 
  `GeomSfPattern` now draws a "border" grob on top of the "pattern" grob (#72).
* Continuous "pattern_colour", "pattern_fill", and "pattern_fill2" color scales'
  default "colourbar" guide support should now work.
* `scale_pattern_size_continuous()` now uses "pattern_size" aesthetic 
  instead of "size" aesthetic in underlying `ggplot2::continuous_scale()` call.
* We now export `scale_pattern_alpha()` which is an alias of
  `scale_pattern_alpha_continuous()`.

# ggpattern 0.3.1

## Breaking changes

{ggpattern} removes the following data/functions
which were deprecated in a previous version of {ggpattern}:

* `calculate_bbox_polygon_df()`
* `convert_img_to_array()`
* `convert_polygon_df_to_alpha_channel()`
* `create_gradient_img()`
* `fetch_placeholder_img()`
* `fill_area_with_img()`
* `magick_filter_names`, instead use `magick::filter_types()`
* `magick_gravity_names`, instead use `magick::gravity_types()`
* `magick_pattern_intensity_names`, instead use `gridpattern::names_magick_intensity`
* `magick_pattern_names`, instead use `gridpattern::names_magick`
* `magick_pattern_stripe_names`, instead use `gridpattern::names_magick_stripe`
* `placeholder_names`, instead use `gridpattern::names_placeholder`
* `rotate_polygon_df()`

## Deprecated features

{ggpattern} now deprecates the following data/functions,
they may be removed in a future version of {ggpattern}:

* `convert_polygon_df_to_polygon_grob()`
* `convert_polygon_df_to_polygon_sf()`
* `convert_polygon_sf_to_polygon_df()`

## New features

* {ggpattern} now supports the following aesthetics (#45):

  * `pattern_grid`: Supported by most "geometry" patterns.
    Default is "square" while "hex" and "hex\_circle" allow hexagonal layouts.
  * `pattern_res`: Supported by "array" patterns.
    Allow user to explicitly set resolution of image-based patterns.
  * `pattern_rot`: Used by "pch", "regular\_polygon", "rose", and "text" patterns 
    to rotate symbol within pattern.
    'rot' is parameter used in `grid:textGrob()` to indicate rotation and
    `pattern_angle` already used to rotate entire pattern.

## Bug fixes and minor improvements

* `geom_polygon_pattern()` and `geom_sf_pattern()` now support polygons with holes (#26, #44).
  If R 4.1 clipping features are not supported by the graphic device
  (or the clipping feature is declined) we must use a raster image approximation of the pattern.
  Resolution of the raster approximation can be adjusted by the `pattern_res` aesthetic
  whose default in turn can be adjusted by the `"ggpattern_res"` global option.

# ggpattern 0.2.0

## Breaking changes

{ggpattern} now uses {gridpattern} to generate pattern grobs.
This means there are minor tweaks to visual behavior:

* 'pattern_spacing', 'pattern_xoffset', and 'pattern_yoffset'
  are now interpreted as 'spnc' units instead of 'npc' units.
  and the center of the pattern "grid" is now the center of the viewport.
  To exactly restore the original appearance of 
  {ggpattern} "geometry" patterns one may need to tweak 
  the 'pattern_density', 'pattern_spacing', 'pattern_xoffset', 
  and/or 'pattern_yoffset' aesthetics.
* The resolution of "array" patterns should be close
  but not identical to before.  
  In particular placeholder images will likely be different.
* Custom {ggpattern} patterns can no longer use the same name as a
  {ggpattern} / {gridpattern} builtin pattern and an error will
  be thrown if you try to do so.

The following functions copied from {ggplot2}
are no longer exported by {ggpattern}:

* `alpha()`, instead use `ggplot2::alpha()` or `scales::alpha()`
* `flip_data()`, instead use `ggplot2::flip_data()`
* `flipped_names()`, instead use `ggplot2::flipped_names()`
* `gg_dep()`, instead use `ggplot2::gg_dep()`
* `has_flipped_aes()`, instead use `ggplot2::has_flipped_aes()`
* `remove_missing()`, instead use `ggplot2::remove_missing()`
* `should_stop()`, instead use `ggplot2::should_stop()`
* `waiver()`, instead use `ggplot2::waiver()`

## Deprecated features

{ggpattern} now deprecates the following data/functions,
they may be removed in a future version of {ggpattern}:

* `calculate_bbox_polygon_df()`
* `convert_img_to_array()`
* `convert_polygon_df_to_alpha_channel()`
* `create_gradient_img()`
* `fetch_placeholder_img()`
* `fill_area_with_img()`
* `magick_filter_names`, instead use `magick::filter_types()`
* `magick_gravity_names`, instead use `magick::gravity_types()`
* `magick_pattern_intensity_names`, instead use `gridpattern::names_magick_intensity`
* `magick_pattern_names`, instead use `gridpattern::names_magick`
* `magick_pattern_stripe_names`, instead use `gridpattern::names_magick_stripe`
* `placeholder_names`, instead use `gridpattern::names_placeholder`
* `rotate_polygon_df()`

## New features

* {ggpattern} now supports all patterns provided by {gridpattern}.
  In particular {gridpattern} provides the following "new" patterns:

  * "ambient" (the suggested package `{ambient}` must be installed)
  * "pch"
  * "polygon_tiling"
  * "regular_polygon"
  * "rose"
  * "text"
  * "wave"
  * "weave"

* 'pattern_fill2' can now be used to set the fill for the vertical lines
  in the 'crosshatch' pattern.  
  By default, if unset,  it uses 'pattern_fill' (#35).
* 'circle' pattern can now be adjusted by 'pattern_type' and 'pattern_subtype'.
  See `help("grid.pattern_circle", package = "gridpattern")` for more info.
* 'gradient' pattern can now use grid gradient feature introduced in R v4.1.0
  if `options(ggpattern_use_R4.1_gradients = TRUE)`.
* The following package options can now be set by `options()`:

  * `ggpattern_use_R4.1_clipping` If `TRUE` use the grid clipping path feature introduced in R v4.1.0
                    else do a `rasterGrob` approximation of the clipped pattern.
  * `ggpattern_use_R4.1_features` If `TRUE` sets the default for all the other
                    `ggpattern_use_R4.1_*` options arguments to `TRUE`.
  * `ggpattern_use_R4.1_gradients` If `TRUE` use the grid gradient feature introduced in R v4.1.0
                    else do a `rasterGrob` approximation of the gradient pattern.
  * `ggpattern_use_R4.1_masks` If `TRUE` use the grid mask feature introduced in R v4.1.0.
                    Available for use in custom patterns.
  * `ggpattern_use_R4.1_patterns` If `TRUE` use the grid pattern feature introduced in R v4.1.0.
                    Available for use in custom patterns.

## Bug fixes and minor improvements

* 'pattern_alpha' default is now `NA` (preserve any alpha transparency in
  `pattern_fill`, `pattern_fill2`, and `pattern_colour` colors) rather than `1`
  (set them fully opaque).
* 'circle' pattern can now be adjusted by 'pattern_xoffset' and 'pattern_yoffset'.

# ggpattern 0.1.3  2020-06-28

* Support POLYGON in `geom_sf_pattern()` (Issue #5)

# ggpattern 0.1.2  2020-06-28

* Fix issue with attempts to fill an area with a pattern when it has zero area. (Issue #9)

# ggpattern 0.1.1  2020-04-23

* Use `magick::image_data()` instead of `magick::as_EBImage()` for internal
  conversion of images to arrays in `convert_img_to_array()`

# ggpattern 0.1.0  2020-04-01

* Initial release
