library(dplyr)

maptitude.tract <- read.csv('../maptitude_texas_census_tracts.csv')
maptitude.county <- read.csv('../maptitude_texas_counties.csv') %>% arrange(data_county)

county.summary <- maptitude.tract %>%
  group_by(data_county) %>%
  summarise(daypop = sum(daytime_age_18., na.rm = T)) %>%
  arrange(data_county)

all.equal(county.summary$daypop, maptitude.county$daytime_age_18.)

library(tidycensus)
library(tidyverse)

# add api key to Renviron if not installed already
# census_api_key("API_KEY_HERE", install = TRUE)

# Pull Texas residence data (Total Pop and Resident Workers)
tx_residence <- get_acs(
  geography = "county",
  state = "TX",
  variables = c(
    total_pop = "B01003_001",
    res_workers = "B08008_001"
  ),
  year = 2024,
  survey = "census",
  output = "wide"
)

# Pull Texas workplace data (Workers located in the Area)
tx_workplace <- get_acs(
  geography = "county",
  state = "TX",
  variables = c(
    workers_in_area = "B08604_001"
  ),
  year = 2024,
  survey = "acs5",
  output = "wide"
)

# Join datasets and calculate Daytime Population
tx_daytime_pop <- tx_residence %>%
  inner_join(tx_workplace, by = "GEOID", suffix = c("", "_work")) %>%
  mutate(
    # Formula: Total Residents + Workers in Area - Resident Workers
    daytime_pop_est = total_popE + workers_in_areaE - res_workersE,

    # Calculate the net population shift during the day
    net_commute_shift = workers_in_areaE - res_workersE
  ) %>%
  select(GEOID, NAME, total_popE, daytime_pop_est, net_commute_shift)

# View counties with the largest daytime population shifts
tx_daytime_pop %>%
  arrange(desc(net_commute_shift)) %>%
  head(10)

tx_daytime_pop <- tx_daytime_pop %>% arrange(GEOID)

#  percent dif
(tx_daytime_pop$daytime_pop_est - maptitude.county$daytime_age_18.)/tx_daytime_pop$daytime_pop_est


library(lehdr)

# ---- Pull Baseline Resident Population from ACS (2021) ----
tx_acs_residents <- get_acs(
  geography = "county",
  state = "TX",
  variables = c(total_res_pop = "B01003_001",
                total_18plus = "B09021_001"),
  year = 2023,
  survey = "acs5",
  output = "wide"
) %>%
  select(GEOID, NAME, total_res_pop = total_res_popE, total_18plus = total_18plusE)


# ---- Pull Total Jobs in Area from LEHD WAC ----
tx_lehd_wac <- grab_lodes(
  state = "tx", year = 2023, lodes_type = "wac",
  job_type = "JT00", segment = "S000", agg_geo = "county"
) %>%
  select(GEOID = w_county, total_jobs_in_area = C000)


# ---- Pull Employed Residents from LEHD RAC ----
tx_lehd_rac <- grab_lodes(
  state = "tx", year = 2023, lodes_type = "rac",
  job_type = "JT00", segment = "S000", agg_geo = "county"
) %>%
  select(GEOID = h_county, total_resident_workers = C000)


# ---- Combine Data and Calculate Hybrid Daytime Population ----
hybrid_daytime_pop <- tx_acs_residents %>%
  inner_join(tx_lehd_wac, by = "GEOID") %>%
  inner_join(tx_lehd_rac, by = "GEOID") %>%
  mutate(
    # Hybrid Formula application
    daytime_pop_est = total_18plus + total_jobs_in_area - total_resident_workers,

    # Net administrative commuter shift
    net_lehd_shift = total_jobs_in_area - total_resident_workers
  )


# ---- View the Final Combined Output ----
print(head(hybrid_daytime_pop))


#  percent dif
diff <- abs((hybrid_daytime_pop$daytime_pop_est - maptitude.county$daytime_age_18.)/hybrid_daytime_pop$daytime_pop_est)*100

plot(diff)

