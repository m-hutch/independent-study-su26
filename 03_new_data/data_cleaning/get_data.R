# ══════════════════════════════════════════════════════════════════════════
# ACS Demographic Wrapper
# Pulls the standard variable set (population, race, education, income,
# poverty, employment, housing, vehicles, insurance, vintage, transportation)
# at any geography level / year / survey, for any set of counties.
#
# Default county set = the 16 counties of the North Central Texas Council
# of Governments (NCTCOG).
#
# Design note: not every detailed table is published at every geography
# level (block groups in particular can lack tables like transportation,
# vehicles, or vintage in some vintages). Rather than erroring out, this
# wrapper probes table availability up front, DROPS any unavailable tables,
# and emits a warning naming exactly what was dropped and why. Derived
# percentage columns that depend on a dropped table are skipped the same
# way, with their own warning, instead of failing the whole pipeline.
# ══════════════════════════════════════════════════════════════════════════

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

# ── 1. Default county set: NCTCOG (16 counties) ─────────────────────────────

nctcog_counties <- c(
  "Collin", "Dallas", "Denton", "Ellis", "Erath", "Hood", "Hunt",
  "Johnson", "Kaufman", "Navarro", "Palo Pinto", "Parker",
  "Rockwall", "Somervell", "Tarrant", "Wise"
)

# ── 1b. County name -> FIPS code resolver ────────────────────────────────────
# get_acs()/tigris county matching is substring-based, which is ambiguous for
# names like "Collin" (also matches "Collingsworth"). To avoid that entirely,
# every county name gets resolved to its exact 3-digit FIPS code up front via
# tidycensus::fips_codes, and FIPS codes (not names) are used for every API
# call from here on. Returns a NAMED character vector: names = the friendly
# county names you passed in, values = FIPS codes.

resolve_county_fips <- function(state, counties) {

  state_abbr <- toupper(state)
  fc <- tidycensus::fips_codes |> filter(state == state_abbr)

  if(counties=="ALL"){
    matched <- fc
  }else{
  target     <- paste0(str_trim(counties), " County")
  matched <- fc |> filter(county %in% target)
  }



  matched_names <- str_remove(matched$county, " County$")
  missing       <- setdiff(str_trim(counties), matched_names)

  if (length(missing) > 0) {
    warning(sprintf(
      "Could not resolve FIPS codes for these counties in state '%s' (dropped -- check spelling): %s",
      state_abbr, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  setNames(matched$county_code, matched_names)
}

# ── 1c. Optional: population-pyramid & institutionalized-population vars ────
# NOT pulled by default -- pass include_pyramid = TRUE to get_acs_shp() to
# include these. Adds ~48 columns: single-year/cohort male & female age
# bands from Table B01001 (for population pyramid charts), plus
# institutionalized-population counts by sex from Table B26217 (adult
# correctional facilities, military quarters/ships). These are RAW counts,
# not percentages -- meant to feed directly into a pyramid chart.

pyramid_vars <- c(
  # Population pyramid age/sex cohorts (Table B01001)
  MALE_0_TO_5     = "B01001_003",
  MALE_5_TO_10    = "B01001_004",
  MALE_10_TO_15   = "B01001_005",
  MALE_15_TO_18   = "B01001_006",
  MALE_18_TO_20   = "B01001_007",
  MALE_20         = "B01001_008",
  MALE_21         = "B01001_009",
  MALE_22_TO_25   = "B01001_010",
  MALE_25_TO_30   = "B01001_011",
  MALE_30_TO_35   = "B01001_012",
  MALE_35_TO_40   = "B01001_013",
  MALE_40_TO_45   = "B01001_014",
  MALE_45_TO_50   = "B01001_015",
  MALE_50_TO_55   = "B01001_016",
  MALE_55_TO_60   = "B01001_017",
  MALE_60_TO_62   = "B01001_018",
  MALE_62_TO_65   = "B01001_019",
  MALE_65_TO_67   = "B01001_020",
  MALE_67_TO_70   = "B01001_021",
  MALE_70_TO_75   = "B01001_022",
  MALE_75_TO_80   = "B01001_023",
  MALE_80_TO_85   = "B01001_024",
  MALE_85_PLUS    = "B01001_025",
  FEMALE_0_TO_5   = "B01001_027",
  FEMALE_5_TO_10  = "B01001_028",
  FEMALE_10_TO_15 = "B01001_029",
  FEMALE_15_TO_18 = "B01001_030",
  FEMALE_18_TO_20 = "B01001_031",
  FEMALE_20       = "B01001_032",
  FEMALE_21       = "B01001_033",
  FEMALE_22_TO_25 = "B01001_034",
  FEMALE_25_TO_30 = "B01001_035",
  FEMALE_30_TO_35 = "B01001_036",
  FEMALE_35_TO_40 = "B01001_037",
  FEMALE_40_TO_45 = "B01001_038",
  FEMALE_45_TO_50 = "B01001_039",
  FEMALE_50_TO_55 = "B01001_040",
  FEMALE_55_TO_60 = "B01001_041",
  FEMALE_60_TO_62 = "B01001_042",
  FEMALE_62_TO_65 = "B01001_043",
  FEMALE_65_TO_67 = "B01001_044",
  FEMALE_67_TO_70 = "B01001_045",
  FEMALE_70_TO_75 = "B01001_046",
  FEMALE_75_TO_80 = "B01001_047",
  FEMALE_80_TO_85 = "B01001_048",
  FEMALE_85_PLUS  = "B01001_049",

  # Institutionalized population by sex (Table B26217)
  PRISON_MALE     = "B26217_011",
  PRISON_FEMALE   = "B26217_012",
  MILITARY_MALE   = "B26217_026",
  MILITARY_FEMALE = "B26217_027"
)

# ── 2. Variable list builder ─────────────────────────────────────────────────
# Rebuilds the named core variable vector + dynamically-detected multi-cell
# groups (English proficiency, health insurance) for a given year/survey,
# since ACS table structures can shift slightly across vintages. Optionally
# folds in the population-pyramid / institutionalized-population variables.

build_variable_list <- function(year, survey = "acs5", include_pyramid = FALSE) {

  vars_lu <- load_variables(year, survey)

  eng_ltevw <- vars_lu |>
    filter(str_detect(name, "^B16001_"), str_detect(label, "less than")) |>
    pull(name)  # speaks English less than "very well"

  no_ins <- vars_lu |>
    filter(str_detect(name, "^B27001_"), str_detect(label, "No health insurance coverage")) |>
    pull(name)  # no health insurance coverage

  core_vars <- c(
    # Population
    NIGHTPOP      = "B01003_001",
    MALE          = "B01001_002",
    FEMALE        = "B01001_026",
    MEDAGE        = "B01002_001",

    # Race / ethnicity
    RACE_TOT      = "B02001_001",
    WHITE         = "B02001_002",
    BLACK         = "B02001_003",
    ASIAN         = "B02001_005",
    HISPAN        = "B03003_003",

    # Educational attainment (pop 25+)
    EDUC_TOT      = "B15003_001",
    EDUC_02 = "B15003_002", EDUC_03 = "B15003_003", EDUC_04 = "B15003_004",
    EDUC_05 = "B15003_005", EDUC_06 = "B15003_006", EDUC_07 = "B15003_007",
    EDUC_08 = "B15003_008", EDUC_09 = "B15003_009", EDUC_10 = "B15003_010",
    EDUC_11 = "B15003_011", EDUC_12 = "B15003_012", EDUC_13 = "B15003_013",
    EDUC_14 = "B15003_014", EDUC_15 = "B15003_015", EDUC_16 = "B15003_016",
    UNIV_22 = "B15003_022", UNIV_23 = "B15003_023",
    UNIV_24 = "B15003_024", UNIV_25 = "B15003_025",

    # English language denominator (pop 5+)
    LANG_TOT      = "B16001_001",

    # Means of transportation to work
    TRANS_TOT     = "B08301_001",
    PUBTRANS      = "B08301_010",
    TTIME_WRKRS   = "B08303_001",
    TTIME_AGG     = "B08136_001",

    # Income
    HHMEDINC      = "B19013_001",
    MEDFAMINC     = "B19113_001",
    PERCAPINC     = "B19301_001",

    # Poverty
    POPOV_TOT     = "B17001_001",
    POPOV_BEL     = "B17001_002",
    FAMPOV_TOT    = "B17010_001",
    FAMPOV_BEL    = "B17010_002",

    # Employment
    LABORFORCE    = "B23025_002",
    UNEMPLOYED    = "B23025_005",

    # Housing occupancy
    HU_TOT        = "B25002_001",
    HU_VACANT     = "B25002_003",
    MEDVALHOME    = "B25077_001",

    # Vehicles available (households)
    VEH_TOT       = "B08201_001",
    VEH_NONE      = "B08201_002",

    # Health insurance denominator
    INS_TOT       = "B27001_001",

    # Housing vintage
    BLDG_TOT      = "B25034_001",
    B_2020PLUS    = "B25034_002",
    B_2010_19     = "B25034_003",
    B_2000_09     = "B25034_004",
    B_1990_99     = "B25034_005",
    B_1980_89     = "B25034_006",
    B_1970_79     = "B25034_007",
    B_1960_69     = "B25034_008",
    B_1950_59     = "B25034_009",
    B_1940_49     = "B25034_010",
    B_PRE1940     = "B25034_011"
  )

  if (isTRUE(include_pyramid)) {
    core_vars <- c(core_vars, pyramid_vars)
  }

  all_vars <- c(
    core_vars,
    setNames(eng_ltevw, eng_ltevw),
    setNames(no_ins,    no_ins)
  )

  list(
    core_vars        = core_vars,
    eng_ltevw         = eng_ltevw,
    no_ins            = no_ins,
    all_vars          = all_vars,
    pyramid_included  = isTRUE(include_pyramid)
  )
}

# ── 3. Table availability probe ──────────────────────────────────────────────
# Tests each distinct ACS table (grouped by table prefix, e.g. "B08301")
# against a single county with geometry off, which is cheap. Any table that
# errors at this geography/year is dropped from the pull, with a warning
# naming the table and every named variable that depended on it.

probe_table_availability <- function(all_vars, geography, state, county_fips, year, survey) {

  test_county <- unname(county_fips[1])

  lookup <- tibble(
    var_name = names(all_vars),
    code     = unname(all_vars)
  ) |>
    mutate(table = str_extract(code, "^[A-Za-z]+[0-9]+"))

  tables <- unique(lookup$table)
  bad_tables <- character(0)

  for (tbl in tables) {
    probe_code <- lookup$code[lookup$table == tbl][1]
    ok <- tryCatch({
      suppressMessages(suppressWarnings(
        get_acs(
          geography = geography, state = state, county = test_county,
          variables = probe_code, year = year, survey = survey, geometry = FALSE
        )
      ))
      TRUE
    }, error = function(e) FALSE)

    if (!ok) bad_tables <- c(bad_tables, tbl)
  }

  if (length(bad_tables) > 0) {
    dropped <- lookup |> filter(table %in% bad_tables)
    warning(sprintf(
      "Dropped %d table(s) not available at geography = '%s', year = %d, survey = '%s': %s\n  Affected variables: %s",
      length(bad_tables), geography, year, survey,
      paste(bad_tables, collapse = ", "),
      paste(dropped$var_name, collapse = ", ")
    ), call. = FALSE)

    all_vars <- all_vars[!lookup$table %in% bad_tables]
  }

  all_vars
}

# ── 4. TIGER land/water area helper ─────────────────────────────────────────

get_tiger_area <- function(geography, state, county_fips, year) {

  geo_fun <- switch(geography,
                    "county"      = function() tigris::counties(state = state, year = year, cb = FALSE),
                    "tract"       = function() tigris::tracts(state = state, county = unname(county_fips), year = year, cb = FALSE),
                    "block group" = function() tigris::block_groups(state = state, county = unname(county_fips), year = year, cb = FALSE),
                    stop("Unsupported geography: ", geography, ". Use 'county', 'tract', or 'block group'.")
  )

  geo_fun() |>
    st_drop_geometry() |>
    select(GEOID, ALAND, AWATER)
}

# ── 4b. "Workers working in area" fetcher (for DAYPOP estimation) ───────────
# Used to estimate daytime population when no daytime_pop table is supplied:
#   DAYPOP = resident population + workers working here - workers living here
#
# The source differs by geography, because ACS's own workplace-geography
# table (B08604 = total worker population AT the workplace) is only
# published for county and above -- it does NOT exist at tract or block
# group resolution (confirmed via Census documentation). So:
#   - geography == "county": pull B08604 directly via get_acs(), one call
#     per county, same pattern as everything else in this script.
#   - geography == "tract" / "block group": fall back to the Census
#     Bureau's LEHD LODES Workplace Area Characteristics (WAC) data, which
#     IS published down to the block level and can be aggregated to tract
#     or block group. This requires the 'lehdr' package (not a tidyverse/
#     tidycensus dependency -- install with install.packages("lehdr")).
#     If it's not installed, or the LODES download fails for any reason
#     (no data for that year, network issue, etc.), this warns and returns
#     NULL -- callers should treat NULL as "skip DAYPOP estimation" rather
#     than erroring, consistent with the rest of this script.

fetch_workers_working_county <- function(state, county_fips, year, survey) {
  results <- purrr::imap(county_fips, function(code, nm) {
    tryCatch({
      get_acs(
        geography = "county", state = state, county = unname(code),
        variables = c(WORKERS_WORKING = "B08604_001"),
        year = year, survey = survey, geometry = FALSE, output = "wide"
      ) |>
        select(GEOID, WORKERS_WORKING = WORKERS_WORKINGE)
    }, error = function(e) {
      warning(sprintf(
        "Could not fetch workplace worker count (B08604) for county '%s': %s",
        nm, conditionMessage(e)
      ), call. = FALSE)
      NULL
    })
  })
  results <- purrr::compact(results)
  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

fetch_workers_working_lodes <- function(geography, state, county_fips, year, lodes_year = NULL) {

  if (!requireNamespace("lehdr", quietly = TRUE)) {
    warning(
      "Package 'lehdr' is not installed, so 'workers working in area' can't be estimated ",
      "at tract/block-group level (ACS itself doesn't publish this below county level -- ",
      "LEHD LODES is the standard source). Install with install.packages('lehdr'), or supply ",
      "your own daytime_pop table to get_acs_shp(). Skipping DAYPOP estimation.",
      call. = FALSE
    )
    return(NULL)
  }

  agg_geo  <- switch(geography, "tract" = "tract", "block group" = "bg",
                     stop("LODES fallback only supports 'tract' or 'block group'."))
  geo_col  <- switch(geography, "tract" = "w_tract", "block group" = "w_bg")
  use_year <- if (is.null(lodes_year)) year else lodes_year

  wac <- tryCatch({
    lehdr::grab_lodes(
      state      = tolower(state),
      year       = use_year,
      version    = "LODES8",
      lodes_type = "wac",
      job_type   = "JT00",   # all jobs
      segment    = "S000",   # total, all ages/earnings/industries
      agg_geo    = agg_geo
    )
  }, error = function(e) {
    warning(sprintf(
      "Could not download LODES WAC data for %d (state '%s'): %s -- try a different lodes_year, or supply your own daytime_pop table. Skipping DAYPOP estimation.",
      use_year, state, conditionMessage(e)
    ), call. = FALSE)
    NULL
  })

  if (is.null(wac)) return(NULL)

  if (!geo_col %in% names(wac) || !"C000" %in% names(wac)) {
    warning("LODES WAC data came back in an unexpected format -- skipping DAYPOP estimation.", call. = FALSE)
    return(NULL)
  }

  state_fips_code <- tidycensus::fips_codes |>
    filter(state == toupper(state)) |>
    pull(state_code) |>
    unique()

  county_geoids <- paste0(state_fips_code, unname(county_fips))

  wac |>
    rename(GEOID = !!geo_col, WORKERS_WORKING = C000) |>
    mutate(GEOID = as.character(GEOID)) |>
    filter(str_sub(GEOID, 1, 5) %in% county_geoids) |>
    select(GEOID, WORKERS_WORKING)
}

fetch_workers_working <- function(geography, state, county_fips, year, survey, lodes_year = NULL) {
  if (geography == "county") {
    fetch_workers_working_county(state, county_fips, year, survey)
  } else {
    fetch_workers_working_lodes(geography, state, county_fips, year, lodes_year)
  }
}

# ── 5. Core ACS pull ─────────────────────────────────────────────────────────
# NOTE: get_acs() has a known issue where passing a vector of county NAMES
# together with geography = "tract" or "block group" can throw
# "formal argument 'state' matched by multiple actual arguments" -- an
# internal argument-duplication bug when it tries to loop over counties
# itself. To sidestep this reliably (and to make per-county failures
# warn-and-skip rather than kill the whole pull), we loop over counties
# ourselves, one get_acs() call per county, and stitch the results together.

pull_acs_data <- function(geography,
                          year             = 2024,
                          survey           = "acs5",
                          county_fips      = NULL,   # named vector: name = friendly county name, value = FIPS code
                          state            = "TX",
                          geometry         = TRUE,
                          include_pyramid  = FALSE) {

  vc <- build_variable_list(year, survey, include_pyramid = include_pyramid)

  available_vars <- probe_table_availability(
    vc$all_vars, geography, state, county_fips, year, survey
  )

  message(sprintf(
    "Pulling %d of %d requested variables for geography = '%s', year = %d, survey = '%s', %d counties (one call per county)...",
    length(available_vars), length(vc$all_vars), geography, year, survey, length(county_fips)
  ))

  county_results <- purrr::imap(county_fips, function(code, nm) {
    tryCatch({
      get_acs(
        geography = geography,
        state     = state,
        county    = unname(code),
        variables = available_vars,
        year      = year,
        survey    = survey,
        geometry  = geometry,
        output    = "wide"
      )
    }, error = function(e) {
      warning(sprintf("Skipping county '%s' (FIPS %s): %s", nm, code, conditionMessage(e)), call. = FALSE)
      NULL
    })
  })

  county_results <- purrr::compact(county_results)  # drop failed counties

  if (length(county_results) == 0) {
    stop("No data could be pulled for any requested county -- check geography/year/survey.")
  }
  if (length(county_results) < length(county_fips)) {
    warning(sprintf(
      "Only %d of %d requested counties returned data; see prior warnings for which were skipped.",
      length(county_results), length(county_fips)
    ), call. = FALSE)
  }

  raw <- do.call(rbind, county_results)

  # Keep the ORIGINAL var_meta (not the trimmed one) so downstream derived-
  # variable calculations can still attempt every metric; safe_calc() below
  # will skip (with a warning) any metric whose source columns are missing.
  attr(raw, "var_meta") <- vc
  raw
}


# ── 6. Safe derived-variable calculator ──────────────────────────────────────
# Attempts to compute one derived column; if any referenced source column
# is missing (because its table was dropped upstream), catches the error,
# warns with the column name, and leaves that column out of the output
# entirely rather than filling it with NA silently.
#
# IMPORTANT: this routes the expression through a real dplyr::mutate() call
# (via tidy evaluation) rather than plain base::eval(). Some of our formulas
# use across()/all_of(), which are data-masking functions that only work
# inside an actual dplyr verb -- calling them via base eval() throws
# "Must only be used inside data-masking verbs", which safe_calc would
# otherwise (wrongly) report as a missing-column problem.

safe_calc <- function(df, name, expr) {
  expr_quo <- rlang::enquo(expr)
  result <- tryCatch(
    dplyr::mutate(df, .safe_calc_tmp = !!expr_quo)$.safe_calc_tmp,
    error = function(e) {
      warning(sprintf("Skipping '%s': %s", name, conditionMessage(e)), call. = FALSE)
      NULL
    }
  )
  if (!is.null(result)) df[[name]] <- result
  df
}

# ── 7. Derived variable computation ──────────────────────────────────────────
# Same formulas as the original county script, generalized and made
# column-safe. POPDEN is always computed from NIGHTPOP alone. DAYPOP and
# PCTDAYPOP come from one of two sources, in priority order:
#   1. A daytime_pop table you supply directly (GEOID + daytime_pop_est) --
#      used as-is if provided.
#   2. Otherwise, if `workers_working` data was fetched (see
#      fetch_workers_working() / section 4b), DAYPOP is ESTIMATED as:
#         DAYPOP = resident population + workers working here - workers living here
#      i.e. NIGHTPOP + WORKERS_WORKING - WORKERS_LIVE. WORKERS_LIVE (total
#      workers 16+ residing in the geography, from B08301_001) is also
#      added to the output in its own right, alongside WORKERS_WORKING.
# If neither is available, DAYPOP-related columns are simply skipped.

compute_derived_vars <- function(df, var_meta, daytime_pop = NULL, workers_working = NULL) {

  eng_ltevw <- var_meta$eng_ltevw
  no_ins    <- var_meta$no_ins

  df <- safe_calc(df, "LANDAREA",  ALAND  / 2589988)
  df <- safe_calc(df, "WATERAREA", AWATER / 2589988)

  df <- safe_calc(df, "POPDEN", NIGHTPOPE / LANDAREA)

  df <- safe_calc(df, "PCTWHITE",  WHITEE  / RACE_TOTE)
  df <- safe_calc(df, "PCTBLACK",  BLACKE  / RACE_TOTE)
  df <- safe_calc(df, "PCTASIAN",  ASIANE  / RACE_TOTE)
  df <- safe_calc(df, "PCTHISPAN", HISPANE / NIGHTPOPE)
  df <- safe_calc(df, "PCTMINOR",  1 - (WHITEE / RACE_TOTE))

  df <- safe_calc(df, "PCTNOHIGH",
                  (EDUC_02E + EDUC_03E + EDUC_04E + EDUC_05E + EDUC_06E +
                     EDUC_07E + EDUC_08E + EDUC_09E + EDUC_10E + EDUC_11E +
                     EDUC_12E + EDUC_13E + EDUC_14E + EDUC_15E + EDUC_16E) / EDUC_TOTE)
  df <- safe_calc(df, "PCTUNIVDEG",
                  (UNIV_22E + UNIV_23E + UNIV_24E + UNIV_25E) / EDUC_TOTE)

  df <- safe_calc(df, "PCTBADENG",
                  rowSums(across(all_of(paste0(eng_ltevw, "E")))) / LANG_TOTE)

  df <- safe_calc(df, "PCTPUB2WRK", PUBTRANSE  / TRANS_TOTE)
  df <- safe_calc(df, "TIME2WORK",  TTIME_AGGE / TTIME_WRKRSE)

  df <- safe_calc(df, "PCTPOPPOV", POPOV_BELE  / POPOV_TOTE)
  df <- safe_calc(df, "PCTFAMPOV", FAMPOV_BELE / FAMPOV_TOTE)

  df <- safe_calc(df, "PCTUNEMP", UNEMPLOYEDE / LABORFORCEE)

  df <- safe_calc(df, "PCTHUVAC", HU_VACANTE / HU_TOTE)
  df <- safe_calc(df, "PCTNOVEH", VEH_NONEE  / VEH_TOTE)

  df <- safe_calc(df, "PCTNOHINS",
                  rowSums(across(all_of(paste0(no_ins, "E")))) / INS_TOTE)

  df <- safe_calc(df, "PCTB2010", (B_2020PLUSE + B_2010_19E) / BLDG_TOTE)
  df <- safe_calc(df, "PCTB2000",  B_2000_09E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1990",  B_1990_99E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1980",  B_1980_89E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1970",  B_1970_79E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1960",  B_1960_69E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1950",  B_1950_59E / BLDG_TOTE)
  df <- safe_calc(df, "PCTB1940",  B_1940_49E / BLDG_TOTE)
  df <- safe_calc(df, "PCTBPRE",   B_PRE1940E / BLDG_TOTE)

  # WORKERS_LIVE = total workers 16+ residing here (B08301_001), already
  # pulled as TRANS_TOTE for PCTPUB2WRK -- surfaced here as its own column.
  df <- safe_calc(df, "WORKERS_LIVE", TRANS_TOTE)

  if (!is.null(daytime_pop)) {
    # Priority 1: caller-supplied daytime population estimate, used as-is.
    df <- df |>
      left_join(
        daytime_pop |> select(GEOID, DAYPOP = daytime_pop_est),
        by = "GEOID"
      )
    df <- safe_calc(df, "PCTDAYPOP", DAYPOP / (DAYPOP + NIGHTPOPE))

  } else if (!is.null(workers_working)) {
    # Priority 2: estimate DAYPOP from resident population + workers
    # working here - workers living here.
    df <- df |> left_join(workers_working, by = "GEOID")
    df <- safe_calc(df, "DAYPOP", NIGHTPOPE + WORKERS_WORKING - TRANS_TOTE)
    df <- safe_calc(df, "PCTDAYPOP", DAYPOP / (DAYPOP + NIGHTPOPE))
  }

  df
}



# ── 8. Final column selection ────────────────────────────────────────────────
# Uses any_of() instead of a hard select() so that any column skipped
# upstream (dropped table or failed calc) simply doesn't appear in the
# output, rather than throwing an "object not found" error here.

select_final_cols <- function(df, daypop_attempted = FALSE, pyramid_requested = FALSE) {

  # opt-in: only appear if a daytime_pop table was supplied, OR DAYPOP was
  # successfully estimated via workers-working-here data
  daypop_cols <- c("DAYPOP", "PCTDAYPOP", "WORKERS_WORKING")

  # opt-in: only appear if include_pyramid = TRUE was passed to get_acs_shp()
  pyramid_cols <- names(pyramid_vars)

  wanted <- c(
    "GEOID", "NAME", "LANDAREA", "WATERAREA", "POPDEN",
    daypop_cols,
    "MALE", "FEMALE", "MEDAGE",
    pyramid_cols,
    "PCTWHITE", "PCTBLACK", "PCTASIAN", "PCTHISPAN", "PCTMINOR",
    "PCTNOHIGH", "PCTUNIVDEG", "PCTBADENG",
    "PCTPUB2WRK", "TIME2WORK", "WORKERS_LIVE",
    "HHMEDINC", "MEDFAMINC", "PERCAPINC",
    "PCTPOPPOV", "PCTFAMPOV",
    "PCTUNEMP",
    "PCTHUVAC",
    "MEDVALHOME",
    "PCTNOVEH",
    "PCTNOHINS",
    "PCTB2010", "PCTB2000", "PCTB1990", "PCTB1980", "PCTB1970",
    "PCTB1960", "PCTB1950", "PCTB1940", "PCTBPRE"
  )

  # rename the *E estimate columns down to their friendly names first,
  # where present, so the any_of() selection below can find them
  rename_map <- c(
    NIGHTPOP = "NIGHTPOPE", MALE = "MALEE", FEMALE = "FEMALEE", MEDAGE = "MEDAGEE",
    HHMEDINC = "HHMEDINCE", MEDFAMINC = "MEDFAMINCE", PERCAPINC = "PERCAPINCE",
    MEDVALHOME = "MEDVALHOMEE",
    setNames(paste0(pyramid_cols, "E"), pyramid_cols)
  )
  present_renames <- rename_map[rename_map %in% names(df)]
  if (length(present_renames) > 0) {
    df <- df |> rename(!!!present_renames)
  }

  # DAYPOP/PCTDAYPOP/WORKERS_WORKING are opt-in -- they only exist if a
  # daytime_pop table was supplied, or if the workers-working-here estimate
  # succeeded (ACS B08604 at county level, LODES WAC at tract/block group).
  # Population-pyramid columns are opt-in via include_pyramid = TRUE. Only
  # warn about either group if it was actually requested and still didn't
  # come through. WORKERS_LIVE and POPDEN are always expected
  # (straightforward ACS-derived values), so they're core columns.
  core_wanted <- setdiff(wanted, c(daypop_cols, pyramid_cols))
  missing <- setdiff(c("NIGHTPOP", core_wanted), names(df))
  if (length(missing) > 0) {
    warning(sprintf(
      "Final output is missing these columns (upstream table/calc unavailable): %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  if (daypop_attempted) {
    missing_daypop <- setdiff(c("DAYPOP", "PCTDAYPOP"), names(df))
    if (length(missing_daypop) > 0) {
      warning(sprintf(
        "DAYPOP estimation was attempted, but these columns still couldn't be computed (see earlier warnings for the specific cause): %s",
        paste(missing_daypop, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (pyramid_requested) {
    missing_pyramid <- setdiff(pyramid_cols, names(df))
    if (length(missing_pyramid) > 0) {
      warning(sprintf(
        "include_pyramid = TRUE was requested, but these columns weren't available at this geography/year: %s",
        paste(missing_pyramid, collapse = ", ")
      ), call. = FALSE)
    }
  }

  df |> select(any_of(c("GEOID", "NAME", "LANDAREA", "WATERAREA", "NIGHTPOP")), any_of(wanted), any_of("geometry"))
}


# ── 9. Top-level wrapper: one call per geography/year/survey ────────────────
#
#   geography      "county" | "tract" | "block group"
#   year           ACS end year, e.g. 2024
#   survey         "acs5" (default) or "acs1"
#   counties       character vector of county names, default = nctcog_counties
#   state          state abbreviation, default "TX"
#   geometry       TRUE/FALSE, include sf geometry
#   daytime_pop    optional data frame with GEOID + daytime_pop_est -- if
#                  supplied, used directly for DAYPOP/PCTDAYPOP and takes
#                  priority over estimate_daypop below
#   estimate_daypop  TRUE/FALSE (default TRUE) -- if daytime_pop isn't
#                  supplied, estimate DAYPOP as
#                  NIGHTPOP + workers working here - workers living here.
#                  Workers working here comes from ACS B08604 at county
#                  level, or LEHD LODES (via the 'lehdr' package) at tract/
#                  block group level, since ACS doesn't publish
#                  workplace-geography tables below county. WORKERS_LIVE
#                  and (when available) WORKERS_WORKING are added to the
#                  output regardless. Set FALSE to skip this entirely.
#   lodes_year     optional -- LODES data year to use for the tract/block
#                  group fallback, if it should differ from `year`
#   include_pyramid  TRUE/FALSE (default FALSE) -- adds ~48 raw-count columns:
#                  single-year/cohort male & female age bands (Table B01001)
#                  for population pyramid charts, plus institutionalized
#                  population by sex (adult correctional facilities,
#                  military quarters/ships -- Table B26217)
#   save_path      if not NULL, saves the resulting object (.rda) to this path
#
# Returns an sf/tibble object with the same column layout as the original
# countyShp script, ready to use at whichever geography you asked for.
# Any table unavailable at this geography/year, or any derived metric that
# depends on one, is dropped with a warning() rather than erroring out.

get_acs_shp <- function(geography,
                        year            = 2024,
                        survey          = "acs5",
                        counties        = nctcog_counties,
                        state           = "TX",
                        geometry        = TRUE,
                        daytime_pop     = NULL,
                        estimate_daypop = TRUE,
                        lodes_year      = NULL,
                        include_pyramid = FALSE,
                        save_path       = NULL) {

  # Resolve friendly county names -> exact FIPS codes ONCE, up front. Every
  # downstream call (probe, pull, TIGER lookup) uses FIPS codes from here on,
  # which avoids ambiguous substring matches like "Collin" vs "Collingsworth".
  county_fips <- resolve_county_fips(state, counties)

  if (length(county_fips) == 0) {
    stop("None of the requested counties could be resolved to FIPS codes -- check spelling and state.")
  }

  raw <- pull_acs_data(
    geography = geography, year = year, survey = survey,
    county_fips = county_fips, state = state, geometry = geometry,
    include_pyramid = include_pyramid
  )

  var_meta <- attr(raw, "var_meta")

  tiger <- get_tiger_area(geography, state, county_fips, year)

  merged <- raw |> left_join(tiger, by = "GEOID")

  # If no daytime_pop table was supplied, try to estimate DAYPOP via the
  # workers-working-here formula (county -> ACS B08604, tract/bg -> LODES).
  workers_working <- NULL
  daypop_attempted <- !is.null(daytime_pop)
  if (is.null(daytime_pop) && isTRUE(estimate_daypop)) {
    daypop_attempted <- TRUE
    workers_working <- fetch_workers_working(
      geography = geography, state = state, county_fips = county_fips,
      year = year, survey = survey, lodes_year = lodes_year
    )
  }

  derived <- compute_derived_vars(
    merged, var_meta, daytime_pop = daytime_pop, workers_working = workers_working
  )

  final_shp <- select_final_cols(
    derived, daypop_attempted = daypop_attempted, pyramid_requested = include_pyramid
  )

  if (!is.null(save_path)) {
    save(final_shp, file = save_path)
    message("Saved to ", save_path)
  }

  final_shp
}

# ══════════════════════════════════════════════════════════════════════════
# USAGE EXAMPLES
# ══════════════════════════════════════════════════════════════════════════

# --- Tract-level, all 16 NCTCOG counties, 2024 ACS5 ---
# By default (estimate_daypop = TRUE), DAYPOP is estimated automatically as
# NIGHTPOP + workers working here - workers living here, using LODES data
# (requires install.packages("lehdr") once). WORKERS_LIVE and
# WORKERS_WORKING are included in the output alongside DAYPOP/PCTDAYPOP.
# nctcog_tractShp <- get_acs_shp(
#   geography = "tract",
#   year      = 2024,
#   survey    = "acs5",
#   counties  = nctcog_counties,
#   save_path = "NCTCOG_tractShp.rda"
# )

# --- Block-group level, all 16 NCTCOG counties, 2024 ACS5 ---
# If some tables aren't published at block-group resolution, you'll see
# warnings naming exactly which tables/variables/derived columns were
# dropped -- the run still completes with whatever IS available.
# nctcog_bgShp <- get_acs_shp(
#   geography = "block group",
#   year      = 2024,
#   survey    = "acs5",
#   counties  = nctcog_counties,
#   save_path = "NCTCOG_bgShp.rda"
# )

# --- County-level, single county, different year ---
# County-level DAYPOP estimation uses ACS B08604 directly (no lehdr needed).
dallas_2019 <- get_acs_shp(
  geography = "county",
  year      = 2024,
  counties  = "Dallas",
  include_pyramid = TRUE,
  save_path = "Dallas_county_2019.rda"
)

# --- Tract-level with your OWN daytime population table instead ---
# Supplying daytime_pop always takes priority over the automatic estimate.
# nctcog_tractShp_daypop <- get_acs_shp(
#   geography   = "tract",
#   year        = 2024,
#   counties    = nctcog_counties,
#   daytime_pop = hybrid_daytime_pop,
#   save_path   = "NCTCOG_tractShp_daypop.rda"
# )

# --- Skip DAYPOP estimation entirely (e.g. no internet access for LODES,
#     or you just don't need it) ---
# nctcog_tractShp_nodaypop <- get_acs_shp(
#   geography       = "tract",
#   year            = 2024,
#   counties        = nctcog_counties,
#   estimate_daypop = FALSE,
#   save_path       = "NCTCOG_tractShp_nodaypop.rda"
# )

# --- Use a specific LODES year for the tract/block-group workers-working
#     estimate, if it should differ from the ACS year (LODES releases lag
#     behind the current year) ---
# nctcog_tractShp_2024 <- get_acs_shp(
#   geography  = "tract",
#   year       = 2024,
#   counties   = nctcog_counties,
#   lodes_year = 2022,
#   save_path  = "NCTCOG_tractShp.rda"
# )

# --- Include population pyramid + institutionalized population variables ---
# Adds MALE_0_TO_5 ... FEMALE_85_PLUS (raw counts, Table B01001) plus
# PRISON_MALE, PRISON_FEMALE, MILITARY_MALE, MILITARY_FEMALE (Table B26217).
tx_countyShp_pyramid <- get_acs_shp(
  geography       = "county",
  year            = 2024,
  counties        = "ALL",
  include_pyramid = TRUE,
  save_path       = "TX_countyShp_pyramid.rda"
)


# --- Capture warnings explicitly if you want to log/inspect what was
#     dropped, rather than just having them print to the console ---
# w <- NULL
# result <- withCallingHandlers(
#   get_acs_shp(geography = "block group", year = 2024, counties = nctcog_counties),
#   warning = function(wc) { w <<- c(w, conditionMessage(wc)); invokeRestart("muffleWarning") }
# )
# print(w)
