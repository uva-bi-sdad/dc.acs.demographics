# Get census poverty variables
# RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS (C17002)
# HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP (B09019)
# Getting all data for Fairfax (51059) (ACS5)
## Note: Block group data not available for 2012-2019
# Subsetting to NCR
# B09019_001 = Total population
# B09019_003 = In families
# B09019_024 = In nonfamily households
# C17002_002:_004 = 0-1.25 poverty

library(tidycensus)
library(tidyverse)
states <- c("VA")
geographies <- c("block group")
years <- c(2019)
poverty <- NULL
poverty_pop <- NULL
pov_vars <- c("C17002_002", "C17002_003", "C17002_004")
for(state in states){
  for(year in years){
      for(geography in geographies){
        poverty <- get_acs(geography = geography, table = "C17002", state = state, year = year, geometry = FALSE,
                         survey = "acs5",cache_table = TRUE) %>% filter(variable %in% pov_vars) %>%
          mutate(measure = case_when(
            variable == "C17002_002" ~ "under 0.5",
            variable == "C17002_003" ~ "0.5 to 0.99",
            variable == "C17002_004" ~ "1 to 1.24"),
            year = year, region_type = as.character(geography),
                 measure_type = "integer") %>% select(-variable, -moe) %>%
          rename(geoid = GEOID, region_name = NAME, value = estimate)
        tot_poverty <- poverty %>%
          group_by(geoid) %>% mutate(value = sum(value), measure = "total_below_125", year = year,
                                     region_type = as.character(geography), measure_type = "integer") %>%
          distinct() %>% bind_rows(poverty)
        poverty_pop <- rbind(poverty_pop, tot_poverty)
      }
    }
  }
house_vars <- c("B11016_001", "B11016_003", "B11016_004", "B11016_005", "B11016_006", "B11016_007", "B11016_008","B11016_009")
household <- NULL
household_size <- NULL
for(state in states){
  for(year in years){
      for(geography in geographies){
        household <- get_acs(geography = geography, table = "B11016", state = state, year = year, geometry = TRUE,
                        survey = "acs5", cache_table = TRUE) %>% filter(variable %in% house_vars) %>%
          mutate(measure = case_when(
            variable == "B11016_001" ~ "total",
            variable == "B11016_003" ~ "2 person",
            variable == "B11016_004" ~ "3 person",
            variable == "B11016_005" ~ "4 person",
            variable == "B11016_006" ~ "5 person",
            variable == "B11016_007" ~ "6 person",
            variable == "B11016_008" ~ "7+ person",
            variable == "B11016_009" ~ "non-family"),
            year = year, region_type = as.character(geography),
                 measure_type = "integer") %>% select(-variable, -moe) %>%
          rename(geoid = GEOID, region_name = NAME, value = estimate)
        # total - non-family
        household_size <- rbind(household_size, household)
      }
  }
}

tot_poverty <- tot_poverty %>% filter(measure == "total_below_125") %>% select(geoid, total_below_125 = value)
data <- household %>% group_by(geoid) %>% mutate(total = max(value), prop = value/total) %>% left_join(tot_poverty, by = "geoid") %>% mutate(poverty_prop_hh_size = prop*total_below_125) %>% filter(str_detect(geoid, "^51059"))




ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

#ncr_prices <- prices %>% dplyr::filter(str_detect(geoid, ncr_counties))
#ncr_rents <- rents %>% dplyr::filter(str_detect(geoid, ncr_counties))

#access <- rbind(prices, rents)
#ncr_access <- rbind(prices, rents)

#con <- get_db_conn()
#dc_dbWriteTable(con, "dc_transportation_housing", "vadcmd_cttrbg_acs5_2009_2019_housing_access", access)
#dc_dbWriteTable(con, "dc_transportation_housing", "ncr_cttrbg_acs5_2009_2019_housing_access", ncr_access)
#DBI::dbDisconnect(con)
