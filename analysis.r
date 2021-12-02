library(tidyverse)
library(jsonlite)
library(here)

# owid data is organised as a list of data frames, keyed by country (iso3)
owid <- get_owid()

owid %>%
  bind_rows(.id = "iso3") %>%
  unpack(data) %>%
  mutate(date = as.Date(date)) ->
owid

# fyi: some indicators are singletons (bind_rows recycles them!):
# - continent
# - location
# - population
# - population_density
# - median_age
# - aged_65_older
# - aged_70_older
# - gdp_per_capita
# - extreme_poverty
# - cardiovasc_death_rate
# - diabetes_prevalence
# - female_smokers
# - male_smokers
# - hospital_beds_per_thousand
# - life_expectancy
# - human_development_index
View(owid)

# okay, let's do a quick example: vaccine % over time
owid %>%
  select(iso3, location, continent, population, date,
    people_fully_vaccinated) %>%
  filter(
    !is.na(people_fully_vaccinated),
    !str_starts(iso3, "OWID_")) %>%
  mutate(vac_prop = people_fully_vaccinated / population) ->
owid_pct_vaxxed

owid_pct_vaxxed %>%
{
  ggplot(.) +
    aes(x = date, y = vac_prop, colour = "iso3") +
    geom_line() +
    geom_point(size = 3) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = NULL, y = "% Fully vaccinated"
    )
}