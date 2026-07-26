#' @title Polygon layer of Texas Counties
#' @description Counties in Texas, USA in the longitude and
#'   latitude format (see \code{proj4string=CRS("+proj=longlat +ellps=WGS84")}).
#'
#'   \strong{Note}: Several fields have missing observations and several variables
#'   are reported as absolute numbers.
#' @docType data
#' @name TX_countyShp
#' @source Based on 2024 ACS data.
#' @format Spatial polygon data-frame with 254 counties. The variables are
#' as follows:
#' #' \describe{
#'   \item{GEOID}{Character. Geographic identifier (FIPS code) for each county.}
#'   \item{NAME}{Character. County name.}
#'
#'   \strong{Area and Population Density}
#'   \item{LANDAREA}{Numeric. Land area in square miles.}
#'   \item{WATERAREA}{Numeric. Water area in square miles.}
#'   \item{NIGHTPOP}{Integer. Resident population (nighttime population).}
#'   \item{DAYPOP}{Integer. Daytime population (workers commuting in).}
#'   \item{POPDEN}{Numeric. Population density (persons per square mile).}
#'   \item{PCTDAYPOP}{Numeric. Percentage of daytime population relative to
#'     nighttime population. Values > 100 indicate net in-commuting.}
#'
#'   \strong{Employment}
#'   \item{WORKERS_WORKING}{Integer. Total workers 16 years and over.}
#'   \item{WORKERS_LIVE}{Integer. Total workers living in county (employed or
#'     seeking work).}
#'   \item{PCTPUB2WRK}{Numeric. Percentage of workers using public transportation
#'     to commute.}
#'   \item{TIME2WORK}{Numeric. Median commute time in minutes.}
#'   \item{PCTUNEMP}{Numeric. Percentage unemployed (unemployment rate).}
#'
#'   \strong{Age and Sex}
#'   \item{MALE}{Integer. Total male population.}
#'   \item{FEMALE}{Integer. Total female population.}
#'   \item{MEDAGE}{Numeric. Median age in years.}
#'   \item{MALE_0_TO_5, MALE_5_TO_10, MALE_10_TO_15, MALE_15_TO_18,
#'     MALE_18_TO_20, MALE_20, MALE_21, MALE_22_TO_25, MALE_25_TO_30,
#'     MALE_30_TO_35, MALE_35_TO_40, MALE_40_TO_45, MALE_45_TO_50,
#'     MALE_50_TO_55, MALE_55_TO_60, MALE_60_TO_62, MALE_62_TO_65,
#'     MALE_65_TO_67, MALE_67_TO_70, MALE_70_TO_75, MALE_75_TO_80,
#'     MALE_80_TO_85, MALE_85_PLUS}{Integer. Male population in specified
#'     age group.}
#'   \item{FEMALE_0_TO_5, FEMALE_5_TO_10, FEMALE_10_TO_15, FEMALE_15_TO_18,
#'     FEMALE_18_TO_20, FEMALE_20, FEMALE_21, FEMALE_22_TO_25, FEMALE_25_TO_30,
#'     FEMALE_30_TO_35, FEMALE_35_TO_40, FEMALE_40_TO_45, FEMALE_45_TO_50,
#'     FEMALE_50_TO_55, FEMALE_55_TO_60, FEMALE_60_TO_62, FEMALE_62_TO_65,
#'     FEMALE_65_TO_67, FEMALE_67_TO_70, FEMALE_70_TO_75, FEMALE_75_TO_80,
#'     FEMALE_80_TO_85, FEMALE_85_PLUS}{Integer. Female population in specified
#'     age group.}
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
#'   \item{PCTBADENG}{Numeric. Percentage of population 5+ years speaking English
#'     less than very well.}
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
#'   \strong{Migration and Birth Decade}
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
#' estimates aggregated at the county level.
#'
#' Percentage variables (prefix "PCT") are stored as numeric values between 0
#' and 100. Age-sex variables represent counts and should sum to the total
#' population when aggregated.
#'
#' The daytime population variable (\code{DAYPOP}) is useful for understanding
#' employment centers and commuting patterns. \code{PCTDAYPOP} > 100 indicates
#' counties with net in-commuting (more workers than residents), while values
#' < 100 indicate net out-commuting.
#'
#' @examples
#' data(TX_countyShp)
#' sp::summary(TX_countyShp)
NULL
