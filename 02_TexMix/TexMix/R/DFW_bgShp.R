#' Dallas-Fort Worth Metropolitan Area Block Group Demographics
#'
#' A spatial data frame containing demographic, socioeconomic, and housing
#' characteristics for block groups in the Dallas-Fort Worth metropolitan area,
#' derived from the American Community Survey (ACS).
#' @name DFW_bgShp
#' @format A SpatialPolygonsDataFrame with block group observations and
#' 36 variables:
#'
#' \describe{
#'   \item{GEOID}{Character. Geographic identifier (block group FIPS code,
#'     12 digits: state + county + tract + block group).}
#'   \item{NAME}{Character. Block group name/description.}
#'
#'   \strong{Area and Population Density}
#'   \item{LANDAREA}{Numeric. Land area in square miles.}
#'   \item{WATERAREA}{Numeric. Water area in square miles.}
#'   \item{NIGHTPOP}{Integer. Resident population (nighttime population).}
#'   \item{POPDEN}{Numeric. Population density (persons per square mile).}
#'
#'   \strong{Age and Sex}
#'   \item{MALE}{Integer. Total male population.}
#'   \item{FEMALE}{Integer. Total female population.}
#'   \item{MEDAGE}{Numeric. Median age in years.}
#'
#'   \strong{Race and Ethnicity}
#'   \item{PCTWHITE}{Numeric. Percentage of population identifying as White
#'     (non-Hispanic).}
#'   \item{PCTBLACK}{Numeric. Percentage of population identifying as Black or
#'     African American (non-Hispanic).}
#'   \item{PCTASIAN}{Numeric. Percentage of population identifying as Asian
#'     (non-Hispanic).}
#'   \item{PCTHISPAN}{Numeric. Percentage of population identifying as Hispanic
#'     or Latino (any race).}
#'   \item{PCTMINOR}{Numeric. Percentage of population in racial/ethnic minority
#'     groups.}
#'
#'   \strong{Education}
#'   \item{PCTNOHIGH}{Numeric. Percentage of population 25+ years with less than
#'     high school education.}
#'   \item{PCTUNIVDEG}{Numeric. Percentage of population 25+ years with a
#'     bachelor's degree or higher.}
#'
#'   \strong{Employment and Transportation}
#'   \item{PCTPUB2WRK}{Numeric. Percentage of workers using public transportation
#'     to commute.}
#'   \item{PCTUNEMP}{Numeric. Percentage unemployed (unemployment rate).}
#'
#'   \strong{Income and Poverty}
#'   \item{HHMEDINC}{Numeric. Median household income in dollars.}
#'   \item{MEDFAMINC}{Numeric. Median family income in dollars.}
#'   \item{PERCAPINC}{Numeric. Per capita income in dollars.}
#'   \item{PCTPOPPOV}{Numeric. Percentage of population living below poverty line.}
#'   \item{PCTFAMPOV}{Numeric. Percentage of families living below poverty line.}
#'
#'   \strong{Housing}
#'   \item{MEDVALHOME}{Numeric. Median home value in dollars.}
#'   \item{PCTHUVAC}{Numeric. Percentage of housing units that are vacant.}
#'   \item{PCTNOVEH}{Numeric. Percentage of households with no vehicle available.}
#'
#'   \strong{Health Insurance}
#'   \item{PCTNOHINS}{Numeric. Percentage of population under 65 without health
#'     insurance coverage.}
#'
#'   \strong{Birth Decade}
#'   \item{PCTB2010}{Numeric. Percentage of population born in 2010s.}
#'   \item{PCTB2000}{Numeric. Percentage of population born in 2000s.}
#'   \item{PCTB1990}{Numeric. Percentage of population born in 1990s.}
#'   \item{PCTB1980}{Numeric. Percentage of population born in 1980s.}
#'   \item{PCTB1970}{Numeric. Percentage of population born in 1970s.}
#'   \item{PCTB1960}{Numeric. Percentage of population born in 1960s.}
#'   \item{PCTB1950}{Numeric. Percentage of population born in 1950s.}
#'   \item{PCTB1940}{Numeric. Percentage of population born in 1940s.}
#'   \item{PCTBPRE}{Numeric. Percentage of population born before 1940.}
#' }
#'
#' @details
#' Data are derived from the American Community Survey (ACS), a continuous
#' survey conducted by the U.S. Census Bureau. Most variables represent 5-year
#' estimates aggregated at the block group level, providing the finest-grain
#' geographic detail available from the ACS suitable for hyperlocal analysis
#' within the Dallas-Fort Worth metropolitan statistical area (DFW MSA).
#'
#' Block groups are subdivisions of census tracts, typically containing 600–3,000
#' residents (ideal target: 1,500 residents). This geographic scale enables
#' neighborhood-level analysis and is commonly used for:
#' \itemize{
#'   \item Environmental justice and equity assessment
#'   \item Health disparities mapping
#'   \item Targeted service and program delivery planning
#'   \item Hyperlocal housing market analysis
#'   \item School attendance area analysis
#' }
#'
#' The GEOID field uniquely identifies each block group and enables hierarchical
#' aggregation to census tracts (by dropping the last digit) and counties
#' (by keeping only the first 5 digits).
#'
#' Percentage variables (prefix "PCT") are stored as numeric values between 0
#' and 100. Note that block group-level estimates from the ACS have larger
#' margins of error than larger geographies (tracts or counties) due to smaller
#' sample sizes. Users should consult the margins of error when conducting
#' statistical inference or hypothesis testing on small populations.
#'
#' @section Hierarchical Geography:
#' Block groups nest within census tracts and counties. The GEOID structure is:
#' \itemize{
#'   \item Digits 1–2: State FIPS code
#'   \item Digits 3–5: County FIPS code
#'   \item Digits 6–11: Census tract (6 digits)
#'   \item Digit 12: Block group (1 digit)
#' }
#'
#' Example: GEOID "480130001001" = Texas (48), Dallas County (013),
#' Tract 0001.00, Block Group 1.
#'
#' @source U.S. Census Bureau American Community Survey (ACS)
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' # Summary statistics for median household income
#' summary(DFW_bgShp@data$HHMEDINC)
#'
#' # Identify block groups with high racial diversity (minority population > 50%)
#' diverse <- DFW_bgShp@data[DFW_bgShp@data$PCTMINOR > 50, ]
#'
#' # Find economically disadvantaged block groups
#' disadvantaged <- DFW_bgShp@data[
#'   DFW_bgShp@data$PCTPOPPOV > 25 &
#'   DFW_bgShp@data$PCTUNIVDEG < 15,
#' ]
#'
#' # Extract parent census tract from GEOID
#' DFW_bgShp@data$TRACT_GEOID <- substr(DFW_bgShp@data$GEOID, 1, 11)
#'
#' # Aggregate to census tract level
#' tract_summary <- aggregate(
#'   cbind(NIGHTPOP, HHMEDINC) ~ TRACT_GEOID,
#'   data = DFW_bgShp@data,
#'   FUN = mean
#' )
#' }
#'
#' @keywords datasets spatial demographics DFW blockgroup
"DFW_bgShp"
