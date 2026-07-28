library(testthat)
data("TX_countyShp")
data("TX_hwyShp")


test_that(".ageStructure errors for missing data", {
  expect_error(ageStructure(sdf = list(1,2,3)), "sdf or sdf@data must be a data frame")
})

test_that(".ageStructure errors for missing age columns", {
  expect_error(ageStructure(TX_hwyShp), "No columns matching the age regex pattern found.")
})

test_that(".ageStructure errors for mismatched bin.width", {
  expect_error(ageStructure(TX_countyShp, bin.width = 1))
})

test_that(".ageStructure errors for non-numeric bin.width", {
  expect_error(ageStructure(TX_countyShp, bin.width = "a"), "bin.width must be numeric")
})

test_that(".ageStructure errors for missing age regex", {
  expect_error(ageStructure(TX_countyShp, age_regex = ""), "Could not parse an age range from one or more matched column names.")
})

test_that(".ageStructure errors for non-matching age regex", {
  expect_error(ageStructure(TX_countyShp, age_regex = "\\w+"), "Could not parse an age range from one or more matched column names.")
})

test_that(".ageStructure returns expected result", {
  expect_length(ageStructure(TX_countyShp)$ages, 18)
  expect_length(ageStructure(TX_countyShp)$male, 18)
  expect_length(ageStructure(TX_countyShp)$female, 18)
})

test_that(".ageStructure handles 'PLUS' age group correctly", {
  TX_countyShp@data$MALE_85_PLUS <- NA
  TX_countyShp@data$FEMALE_85_PLUS <- NA
  expect_length(ageStructure(TX_countyShp)$ages, 18)
  expect_length(ageStructure(TX_countyShp)$male, 18)
  expect_length(ageStructure(TX_countyShp)$female, 18)
})

test_that(".ageStructure handles single-age group correctly", {
  TX_countyShp@data$MALE_100 <- NA #should add 15 years or 3 additional age groups
  TX_countyShp@data$FEMALE_100 <- NA
  expect_length(ageStructure(TX_countyShp)$ages, 21)
  expect_length(ageStructure(TX_countyShp)$male, 21)
  expect_length(ageStructure(TX_countyShp)$female, 21)
})
