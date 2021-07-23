# deeptime 0.1.0
First minor release (planned for CRAN submission)

* Fixed ability to use text transformations (from `scales`) in `coord_geo()` (#30)
* Added automatic R CMD check Github Actions
* Fixed S3 method consistency and other R CMD check warnings and notes
* Fixed `coord_geo()` when axis breaks were reversed or NULL (#23 and #29)
* Added packages from examples and README to `Suggests`

# deeptime 0.0.6
Several bug fixes and minor features.

* Added ability to specify a layout for `ggarrange2()` (#21)
* Added ability to center labels for time intervals that are broken by axis limits (#18)
* Fixed `gggeo_scale()` for `ggplot2`>=3.3.0 (#22)
* Replaced README examples with examples using real data (#28)
* Fixed using first letters for the `pos` argument
* Fixed label order (#26)
* Switched from Travis to Github Actions (#27)
* Fixed handling of axis tick labels for coord_trans_xy (#31)

# deeptime 0.0.5
Added `coord_geo()`, which is a coordinate system for `ggplot2` that allows for adding highly customized timescales to `ggplot` objects. Both `gggeo_scale()` and `gggeo_scale_old` are preserved but will receive notably less maintenance moving forward.

* Updated examples and documentation to use `coord_geo()`
* Added `coord_trans_xy()`, which is a coordinate system for `ggplot2` that is similar to `coord_trans()` but allows for 2-dimensional transformations
* Added `disparity_through_time()` which allows for plotting 2-dimensional data across a 3rd dimension (using `lattice`)

# deeptime 0.0.4
`ggarrange2()` now accepts `geo_scale` objects and other grobified ggplots

* Fixed missing axes and axis titles

# deeptime 0.0.3
Minor release for bug fixes

* Added options to customize borders and line width
* Fixed abbreviations for stages named "Series X" (#9)
* Fixed scale when x-axis crosses 0 (#10, #12)
* Preserve margins of original plots (#13)
* Fixed scale for plots with multiple panels

# deeptime 0.0.2
Complete redesign of `gggeo_scale()` that adds the scale outside of the plotting space using `gtable`. The old version of `gggeo_scale()` is preserved as `gggeo_scale_old()`.

* Can now pull timescales from the Macrostrat API (#8)

# deeptime 0.0.1
First full release. Adds scale onto the bottom of a ggplot.

* Depends on R>3.4 (#1)
* Fixed adding scales to faceted plots (#2)
* Added timescale data from PBDB API (#3)
* Fixed the use of ggtree when a geom has its own data (#5)
* Fixed the stacking of multiple scales (#6)
* Fixed documentation spelling (#7)