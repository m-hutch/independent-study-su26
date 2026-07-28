# Install in-progress version of TexMix in R
```
library(devtools)
install_github("m-hutch/independent-study-su26/02_TexMix/TexMix")
```


# Summary of Changes/Updates

*As of 6/12/2026*

- updated some examples of existing functions to pass `devtools::check()` (needed to explicitly load `sp` to plot)
- added `prepIJDf()` function and test cases
- added Italy data
- reformatted `ItalyMigration.R` script into vignette sytle and added it to package vignettes

*As of 6/28/2026*

- updated `mapColorRamp()` function to accept differnt palette and break styles
- added `palette_hist()` to quickly visualize distribution of color breaks

*As of 7/28/2026*
- renamed `prepIJDf()` to `flow2vec()`
- renamed `palette_hist()` to `viewPaletteSpread`
- implemented `ageStructure()` to make age structure diagrams (i.e. Population Pyramids)
- added new TX state-wide data: `TX_countyShp`, `TX_hwyShp`, and `TX_neighShp` (neighboring states/Mexico only have geometry no variables)
- added updated Dallas and surrounding counties data: `DFW_tractShp`, `DFW_bgShp` `DFW_waterShp` (neighboring counties included in the same dataset with same variables as Dallas county)
- renamed Italy data to match naming scheme (and updated the loaded variable names)
- added `requireNamespace("sp")` to the `.onLoad()` function
- added `plot.SpatialPolygonsDataFrame` wrapper to force use of `sp::plot` around spatial data objects (e.g. `plot(TX_countyshp)` will now call `sp::plot(TX_countyshp)` and avoids that weird S4 signature error we were getting sometimes

