
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpattern <img src="man/figures/logo.png" align="right" width="300" />

<!-- badges: start -->

![](http://img.shields.io/badge/cool-useless-green.svg) [![R build
status](https://github.com/coolbutuseless/ggpattern/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/ggpattern/actions)
<!-- badges: end -->

<span style="font-size: xx-large; font-weight: normal;">`ggpattern`
provides custom `ggplot2` geoms which support filled areas with
geometric and image-based patterns.</span>

Reading the articles/vignettes on [the package
website](https://coolbutuseless.github.io/package/ggpattern/) is
probably the best way to get started.

## Feature Summary

  - Custom versions of (almost) all the **geoms** from ggplot2 which
    have a region which can be filled.
  - A suite of **aesthetics** for controlling the pattern appearance
    (e.g. `pattern_alpha`)
  - The ability to include **user-defined patterns**

## Installation

You can install the development version from
[GitHub](https://github.com/coolbutuseless/ggpattern) with:

``` r
# install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")
```

## Quickstart

1.  Take an existing plot which contains a geom with a fillable area e.g
    `geom_col()`.
2.  Use the `{ggpattern}` version of the geom
    e.g. `ggpattern::geom_col_pattern()` instead of
    `ggplot2::geom_col()`
3.  Set the aesthetic `pattern` to your choice of pattern e.g `pattern =
    'stripe'`, and set other options using `pattern_*` aesthetics

<!-- end list -->

``` r
df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))

ggplot(df) +
  geom_col_pattern(
    aes(level, outcome, pattern_fill = level), 
    pattern = 'stripe',
    fill    = 'white',
    colour  = 'black'
  ) +
  theme_bw(18) +
  theme(legend.position = 'none') + 
  labs(
    title    = "ggpattern::geom_pattern_col()",
    subtitle = "pattern = 'stripe'"
  ) +
  coord_fixed(ratio = 1/2)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

# Gallery

<div>

<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html#geom-bar-pattern-"><img width="45%" src="man/figures/readme/gallery-bar-array.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-geometry.html#b-w-example"><img width="45%" src="man/figures/readme/gallery-bar-bw.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-geometry.html#colour-example-1"><img width="45%" src="man/figures/readme/gallery-bar-colour.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html#geom-bar-pattern-coord-flip-and-fit-image-to-height-and-graivty-towards-the-east-"><img width="45%" src="man/figures/readme/gallery-bar2-array.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html#geom-density-pattern-"><img width="45%" src="man/figures/readme/gallery-density-array.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-geometry.html#geom-density-pattern-"><img width="45%" src="man/figures/readme/gallery-density-grid.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html#geom-map-pattern-"><img width="45%" src="man/figures/readme/gallery-map-array.jpg" />
</a>
<a href="https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html#geom-rect-pattern-"><img width="45%" src="man/figures/readme/gallery-rect-array.jpg" />
</a>

<div>


# Feature Details

## Available Geoms

`ggpattern` includes versions of (nearly) all geoms from `ggplot2` which
could plausiblly support being filled with a pattern.

See the vignette galleries for examples of all the available geoms
filled with [geometry-based
patterns](https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-geometry.html)
and [image-based/array-based
patterns](https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html).

<details>

<summary> <span style="color: blue;"> Click to show/hide list of
supported geoms </span> </summary>

| ggplot2        | ggpattern               |
| -------------- | ----------------------- |
| geom\_area     | geom\_area\_pattern     |
| geom\_bar      | geom\_bar\_pattern      |
| geom\_bin2d    | geom\_bin2d\_pattern    |
| geom\_boxplot  | geom\_boxplot\_pattern  |
| geom\_col      | geom\_col\_pattern      |
| geom\_crossbar | geom\_crossbar\_pattern |
| geom\_density  | geom\_density\_pattern  |
| geom\_map      | geom\_map\_pattern      |
| geom\_polygon  | geom\_polygon\_pattern  |
| geom\_rect     | geom\_rect\_pattern     |
| geom\_ribbon   | geom\_ribbon\_pattern   |
| geom\_sf       | geom\_sf\_pattern       |
| geom\_tile     | geom\_tile\_pattern     |
| geom\_violin   | geom\_violin\_pattern   |

</details>

## New aesthetics

To control pattern appearance, a raft of new aesthetics have been added.
e.g.  `pattern_alpha`, `pattern_filename`, `pattern_density`.

There are also scale functions to control each of these new aesthetics
e.g.  `scale_pattern_alpha_discrete`.

Not all aesthetics apply to all patterns. See the individual pattern
vignettes for which aesthetics it uses, or see the first vignette on
developing user-defined patterns for a [table of aesthetic use by
pattern](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-1.html#aes-by-pattern),
or see the individual vignettes for each pattern.

<details>

<summary> <span style="color: blue;"> Click to show/hide list of new
aesthetics </span> </summary>

| aesthetic                  | description                                | default    | possible values                                                          |
| -------------------------- | ------------------------------------------ | ---------- | ------------------------------------------------------------------------ |
| `pattern`                  | Name of the pattern to draw                | ‘stripe’   | stripe, crosshatch, circle, image, placeholder, magick, gradient, plasma |
| `pattern_type`             | Generic control option                     | NA         | pattern-dependent                                                        |
| `pattern_subtype`          | Generic control option                     | NA         | pattern-dependent                                                        |
| `pattern_alpha`            | Alpha                                      | 1          | value in range \[0, 1\] (npc units)                                      |
| `pattern_linetype`         | Stroke linetype                            | 1          | linetype                                                                 |
| `pattern_size`             | Stroke linewidth                           | 1          | linewidth                                                                |
| `pattern_shape`            | Plotting shape                             | 1          | shapes                                                                   |
| `pattern_colour`           | Stroke colour                              | ‘grey20’   | colour                                                                   |
| `pattern_fill`             | Fill colour                                | ‘grey80’   | colour                                                                   |
| `pattern_fill2`            | Second fill colour                         | ‘\#4169E1’ | colour                                                                   |
| `pattern_angle`            | Rotation angle                             | 30         | angle in degrees                                                         |
| `pattern_density`          | Approx. fraction of area the pattern fills | 0.2        | value in range \[0, 1\] (fraction)                                       |
| `pattern_spacing`          | Spacing between repetitions of pattern     | 0.05       | value in range \[0, 1\] (npc units)                                      |
| `pattern_xoffset`          | Shift pattern along x axis                 | 0          | value in range \[0, 1\] (npc units)                                      |
| `pattern_yoffset`          | Shift pattern along y axis                 | 0          | value in range \[0, 1\] (npc units)                                      |
| `pattern_aspect_ratio`     | Aspect ratio adjustment                    | NA         | usual range \[0.01, 10\]                                                 |
| `pattern_key_scale_factor` | Scale factor for pattern in legend         | 1          |                                                                          |
| `pattern_filename`         | Image filename/URL                         | ’’         | Filename/URL                                                             |
| `pattern_gravity`          | Image placement                            | ‘center’   | `ggpattern::magick_gravity_names`                                        |
| `pattern_filter`           | Image scaling filter                       | ‘lanczos’  | `ggpattern::magick_filter_names`                                         |
| `pattern_scale`            | Scale                                      | 1          | Multiplier                                                               |
| `pattern_orientation`      | Orientation                                | ‘vertical’ | ‘vertical’, ‘horizontal’, ‘radial’                                       |
| `pattern_phase`            | Phase                                      | 0          |                                                                          |
| `pattern_frequency`        | Frequency                                  | 0.1        |                                                                          |
| `pattern_option_1 - 5`     | Generic options for expansion              | 0          |                                                                          |

</summary>

</details>

## User-Defined Patterns

Users can write their own pattern functions and ask `ggpattern` to use
them, without having to include the pattern in the package.

See the vignettes on developing patterns (
[1](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-1.html)
[2](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html),
[3](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html)
) for how to do this, and see the vignettes on experimental patterns to
see this in action ( [Point
filling](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-points.html),
[Hex
pattern](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-hex.html),
[Ambient
Noise](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-ambient.html)
).

# Vignettes

#### General examples

  - [geom gallery (geometry-based
    patterns)](https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-geometry.html)
    Examples of every geom filled with the geometry-based patterns
    (i.e. ‘stripe’, ‘crosshatch’, ‘circle’)
  - [geom gallery (array-based
    patterns)](https://coolbutuseless.github.io/package/ggpattern/articles/geom-gallery-array.html)
    Examples of every geom filled with the array-based patterns
    (i.e. ‘image’, ‘magick’, ‘gradient’, ‘plasma’, ‘placeholder’)

#### Exploration of pattern parameters and appearance

  - Geometry-based patterns
      - [Common aesthetics for geometry-based
        patterns](https://coolbutuseless.github.io/package/ggpattern/articles/geometry-based-pattern-parameters.html)
      - [stripes](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html)
      - [crosshatch](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html)
      - [circles](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html)
  - Array-based patterns
      - [image](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-image.html)
      - [placeholder](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-placeholder.html)
      - [gradient](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html)
      - [plasma](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-plasma.html)
      - [magick](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-magick.html)

#### Developing your own pattern

  - [Devloping Patterns 1 - Pattern
    Overview](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-1.html)
  - [Devloping Patterns 2 - Geometry-based
    pattern](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html)
  - [Devloping Patterns 3 - Array-based
    pattern](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html)

#### Experimental patterns

These are patterns that aren’t quite ready for prime-time. Feel free to
steal the code and extend to suit your needs.

  - [Point
    filling](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-points.html)
  - [Hex
    pattern](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-hex.html)
  - [Ambient
    Noise](https://coolbutuseless.github.io/package/ggpattern/articles/experimental-pattern-ambient.html)

#### Other examples

  - [gganimate](https://coolbutuseless.github.io/package/ggpattern/articles/gganimate.html)

# Limitations

  - Nearly always need to use `coord_fixed()` to ensure the aspect ratio
    is calculated correctly. Use `pattern_aspect_ratio` to override the
    internal calculation, of for occasions where you can’t use
    `coord_fixed()` because a different `coord_*()` is used.
  - Legend rendering for patterns is still not great.
      - Use `pattern_key_scale_factor` to adjust legend appearance.
  - The Rstudio output device can be quite slow for plots with lots of
    patterns. It is often faster to save directly to PNG or PDF and view
    that.
  - Self intersecting geometry can be an issue.  
  - Non-linear coordinate systems have not been tested.
  - Polygons with holes are not supported

# ToDo

  - Possibly add geoms from third-party sources e.g.
      - `geom_circle()` and `geom_voronoi()` from
        [ggforce](https://github.com/thomasp85/ggforce)
  - A vignette on how array-based patterns are implemented.
  - A vignette on why aspect ratio is hard to get right.
