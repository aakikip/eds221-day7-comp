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


#Wrangling continued - joins of different flavors

#Practice with joins

animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

#Practice with full join
#keeps all rows and adds all columns
full_join(animals, sites)

#left join
left_join(animals, sites)

#right join
right_join(animals, sites)

#inner join
inner_join(animals, sites)

#Filtering joins
semi_join(animals, sites)

animals |>
  filter(location %in% sites$location)

#anti join
anti_join(animals, sites)

animals |>
  filter(!location %in% sites$location)

anti_join(sites, animals)


#Practice with lubridate

my_date <- "03_13-1998"
lubridate::mdy(my_date) #fixed date to ISO 8601

#new format for date
my_date <- "08-Jun-1974"
lubridate::dmy(my_date)

#another example of different format
my_date <- "19160518"
lubridate::ymd(my_date)

#what happens if we give lubridate a date that doesn't make sense?
lubridate::mdy("1942-08-30") #incorrect input

lubridate::dmy("09/12/84")

#working with dates-times
time <- "2020-08-12 11:18"
ymd_hm(time)
class(ymd_hm(time))
time <- ymd_hm(time)

#convert to PDT
with_tz(time, "America/Los_Angeles")

#extract info from dates
week(time)
year(time)
day(time)

time <- ymd_hm(time, tz = "America/Los_Angeles")

Sys.time()

start_time <- Sys.time()

end_time <- Sys.time()

end_time - start_time











