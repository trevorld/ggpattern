# ggpattern 0.2.0

## Breaking changes

{ggpattern} now uses {gridpattern} to generate pattern grobs.
This means there are minor tweaks to visual behavior.  To exactly restore
the original appearance of {ggpattern} graphs one may need to tweak 
the 'pattern_density', 'pattern_spacing', 'pattern_xoffset', 
and/or 'pattern_yoffset' aesthetics:

* 'pattern_spacing', 'pattern_xoffset', and 'pattern_yoffset'
  are now interpreted as 'spnc' units instead of 'npc' units.
* the center of the pattern "grid" is now the center of the viewport

## New features

* 'pattern_fill2' can now be used to set the fill for the vertical lines
  in the 'crosshatch' pattern.  
  By default, if unset,  it uses 'pattern_fill' (#35).

## Bug fixes and minor improvements

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
