# ggpattern 0.4.1

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
