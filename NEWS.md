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

## New features

* {ggpattern} now supports all patterns provided by {gridpattern}.
  In particular {gridpattern} provides the following "new" patterns:

  * "ambient" (the suggested package `{ambient}` must be installed)
  * "pch"
  * "polygon_tiling"
  * "regular_polygon"
  * "weave"

* 'pattern_fill2' can now be used to set the fill for the vertical lines
  in the 'crosshatch' pattern.  
  By default, if unset,  it uses 'pattern_fill' (#35).

## Bug fixes and minor improvements

* 'pattern_alpha' default is now `NA` (preserve any alpha transparency in
  `pattern_fill`, `pattern_fill2`, and `pattern_colour` colors) rather than `1`
  (set them fully opaque).
* 'circle' pattern can now be adjusted by 'pattern_xoffset' and 'pattern_yoffset'

# ggpattern 0.1.3  2020-06-28

* Support POLYGON in `geom_sf_pattern()` (Issue #5)

# ggpattern 0.1.2  2020-06-28

* Fix issue with attempts to fill an area with a pattern when it has zero area. (Issue #9)

# ggpattern 0.1.1  2020-04-23

* Use `magick::image_data()` instead of `magick::as_EBImage()` for internal
  conversion of images to arrays in `convert_img_to_array()`

# ggpattern 0.1.0  2020-04-01

* Initial release
