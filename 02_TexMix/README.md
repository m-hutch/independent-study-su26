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
