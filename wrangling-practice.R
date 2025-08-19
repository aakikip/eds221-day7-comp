#Clearing environment
rm(list = ls())

# Attach R packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) #help us work with dates

#Data wrangling refresher
# 1. only include penguins at Briscoe and Dream Islands
# 2. Remove the year ans sex variables
# 3. Add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4. Rename the island variable to location

View("palmerpenguins")

penguins |>
  filter(island == c("Briscoe", "Dream")) |>
  select(-year, -sex) |>
  mutate("body_mass_kg" = body_mass_g / 1000) |>
  rename(location = island)

# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, standard deviation, and sample size (n()) of flipper lengths for male and female

penguins |>
  filter(species == "Adelie") |>
           filter(!is.na(flipper_length_mm),
                  !is.na(sex)) |> #remove rows that are not NA
           group_by(sex) |>
           summarise(mean = mean(flipper_length_mm),
                     standard_dev = sd(flipper_length_mm),
                     sample_size = n())












