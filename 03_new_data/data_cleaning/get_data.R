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

# ── 2. Variable list builder ─────────────────────────────────────────────────
# Rebuilds the named core variable vector + dynamically-detected multi-cell
# groups (English proficiency, health insurance) for a given year/survey,
# since ACS table structures can shift slightly across vintages.

build_variable_list <- function(year, survey = "acs5") {

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

  all_vars <- c(
    core_vars,
    setNames(eng_ltevw, eng_ltevw),
    setNames(no_ins,    no_ins)
  )

  list(
    core_vars = core_vars,
    eng_ltevw = eng_ltevw,
    no_ins    = no_ins,
    all_vars  = all_vars
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
                          year        = 2024,
                          survey      = "acs5",
                          county_fips = NULL,   # named vector: name = friendly county name, value = FIPS code
                          state       = "TX",
                          geometry    = TRUE) {

  vc <- build_variable_list(year, survey)

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

select_final_cols <- function(df, daytime_pop_requested = FALSE) {

  daypop_cols <- c("DAYPOP", "PCTDAYPOP")   # opt-in, needs daytime_pop supplied

  wanted <- c(
    "GEOID", "NAME", "LANDAREA", "WATERAREA", "POPDEN",
    daypop_cols,
    "MALE", "FEMALE", "MEDAGE",
    "PCTWHITE", "PCTBLACK", "PCTASIAN", "PCTHISPAN", "PCTMINOR",
    "PCTNOHIGH", "PCTUNIVDEG", "PCTBADENG",
    "PCTPUB2WRK", "TIME2WORK",
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
    MEDVALHOME = "MEDVALHOMEE"
  )
  present_renames <- rename_map[rename_map %in% names(df)]
  if (length(present_renames) > 0) {
    df <- df |> rename(!!!present_renames)
  }

  # DAYPOP/PCTDAYPOP are opt-in (only computed if you pass a daytime_pop
  # table into get_acs_shp()) -- only warn about those if the caller
  # actually asked for them and they still didn't come through. POPDEN is
  # always expected (computed from NIGHTPOP alone), so it's treated as a
  # core column below.
  core_wanted <- setdiff(wanted, daypop_cols)
  missing <- setdiff(c("NIGHTPOP", core_wanted), names(df))
  if (length(missing) > 0) {
    warning(sprintf(
      "Final output is missing these columns (upstream table/calc unavailable): %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  if (daytime_pop_requested) {
    missing_daypop <- setdiff(daypop_cols, names(df))
    if (length(missing_daypop) > 0) {
      warning(sprintf(
        "A daytime_pop table was supplied, but these columns still couldn't be computed: %s",
        paste(missing_daypop, collapse = ", ")
      ), call. = FALSE)
    }
  }

  df |> select(any_of(c("GEOID", "NAME", "LANDAREA", "WATERAREA", "NIGHTPOP")), any_of(wanted), any_of("geometry"))
}

# ── 9. Top-level wrapper: one call per geography/year/survey ────────────────
#
#   geography   "county" | "tract" | "block group"
#   year        ACS end year, e.g. 2024
#   survey      "acs5" (default) or "acs1"
#   counties    character vector of county names, default = nctcog_counties
#   state       state abbreviation, default "TX"
#   geometry    TRUE/FALSE, include sf geometry
#   daytime_pop optional data frame with GEOID + daytime_pop_est
#   save_path   if not NULL, saves the resulting object (.rda) to this path
#
# Returns an sf/tibble object with the same column layout as the original
# countyShp script, ready to use at whichever geography you asked for.
# Any table unavailable at this geography/year, or any derived metric that
# depends on one, is dropped with a warning() rather than erroring out.

get_acs_shp <- function(geography,
                        year        = 2024,
                        survey      = "acs5",
                        counties    = "ALL",
                        state       = "TX", # TX FIPS code is 48
                        geometry    = TRUE,
                        daytime_pop = NULL,
                        save_path   = NULL) {

  # Resolve friendly county names -> exact FIPS codes ONCE, up front. Every
  # downstream call (probe, pull, TIGER lookup) uses FIPS codes from here on,
  # which avoids ambiguous substring matches like "Collin" vs "Collingsworth".
  county_fips <- resolve_county_fips(state, counties)

  if (length(county_fips) == 0) {
    stop("None of the requested counties could be resolved to FIPS codes -- check spelling and state.")
  }

  raw <- pull_acs_data(
    geography = geography, year = year, survey = survey,
    county_fips = county_fips, state = state, geometry = geometry
  )

  var_meta <- attr(raw, "var_meta")

  tiger <- get_tiger_area(geography, state, county_fips, year)

  merged <- raw |> left_join(tiger, by = "GEOID")

  derived <- compute_derived_vars(merged, var_meta, daytime_pop = daytime_pop)

  final_shp <- select_final_cols(derived, daytime_pop_requested = !is.null(daytime_pop))

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
# dallas_2019 <- get_acs_shp(
#   geography = "county",
#   year      = 2019,
#   counties  = "Dallas",
#   save_path = "Dallas_county_2019.rda"
# )

# --- County-level, all texas counties ---
texas_2024 <- get_acs_shp(
  geography = "county",
  year      = 2024,
  save_path = "TX_counties.rda"
)

# --- Tract-level with the daytime population join ---
# nctcog_tractShp_daypop <- get_acs_shp(
#   geography   = "tract",
#   year        = 2024,
#   counties    = nctcog_counties,
#   daytime_pop = hybrid_daytime_pop,
#   save_path   = "NCTCOG_tractShp_daypop.rda"
# )

# --- Capture warnings explicitly if you want to log/inspect what was
#     dropped, rather than just having them print to the console ---
# w <- NULL
# result <- withCallingHandlers(
#   get_acs_shp(geography = "block group", year = 2024, counties = nctcog_counties),
#   warning = function(wc) { w <<- c(w, conditionMessage(wc)); invokeRestart("muffleWarning") }
# )
# print(w)
