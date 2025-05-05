# Selection of Representative Fall Profile Dates
#
# To compare lake profiles across years, we selected a single representative
# fall date for each lake that was most consistently sampled over time. Profiles
# were commonly collected on different dates each year, so to maximize
# interannual comparability, we identified the day of year (DOY) that minimized
# the total difference between actual sampling dates and a standardized
# reference date.
#
# For each lake, we iteratively tested all DOYs between 300 and 365 (late
# October to December 31). For each candidate DOY, we calculated the absolute
# difference in days between this reference date and the actual sampling dates
# for each year. When multiple profiles were collected in a single year, we used
# the earlier date, the later date, or their average—whichever was closest to
# the reference DOY. We summed these minimum distances across all years, and
# selected the DOY that minimized this total distance as the representative
# sampling date for that lake. This approach allowed us to identify a
# lake-specific fall date that was most consistently sampled across years,
# reducing variability due to shifting sampling schedules.
#
# Once the optimal DOY was determined, we extracted the corresponding closest
# actual profile (or average of two) for each year to create a consistent annual
# time series of fall conditions.

# Highlight profiles used for averaging 
yday.list = list()
counter <- 1  # to index the list
for (lakename in c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) {
  for (useyday in 300:365) {
  fp = df.full.ice |> 
    filter(location_name == lakename) |> 
    mutate(wateryear = if_else(month(date_time) >= 10, year + 1, year)) |> 
    group_by(location_name, wateryear, date_time) %>%
    summarise() |> 
    slice(1:2) |> 
    group_by(location_name, wateryear) |> 
    summarize(
      date1 = min(date_time),
      date2 = max(date_time),
      .groups = "drop"
    ) |> 
    rowwise() %>%
    mutate(
      # dec1 = as.Date(paste0(wateryear-1, "-12-01")),
      dec1 = as.Date(paste0(wateryear - 1, "-01-01")) + useyday - 1,
      avg_date = as.Date(mean(c(as.numeric(date1), as.numeric(date2))), origin = "1970-01-01"),
      dist1 = abs(as.numeric(difftime(date1, dec1, units = "days"))),
      dist2 = abs(as.numeric(difftime(date2, dec1, units = "days"))),
      dist_avg = abs(as.numeric(difftime(avg_date, dec1, units = "days"))),
      closest = case_when(
        dist1 < dist2 & dist1 < dist_avg ~ "first",
        dist2 < dist1 & dist2 < dist_avg ~ "second",
        TRUE ~ "average"
      )
    ) |>
    rowwise() %>%
    mutate(
      chosen_date = case_when(
        closest == "first"   ~ date1,
        closest == "second"  ~ date2,
        closest == "average" ~ as.Date(mean(c(as.numeric(date1), as.numeric(date2))), origin = "1970-01-01")
      )
    ) %>%
    ungroup() %>%
    mutate(min_dist = pmin(dist1, dist2, dist_avg)) 
  yday.list[[counter]] = data.frame(location_name = lakename, yday = useyday, mindist = sum(fp$min_dist))
  counter <- counter + 1  # to index the list
  }
}

# Bind list and extract perferrd date 
bestdates = bind_rows(yday.list) |> 
  mutate(fakedate = as.Date("1995-01-01") + yday - 1) |> 
  arrange(mindist) |> 
  group_by(location_name) |> 
  slice(1) |> 
  mutate(usedate = format(fakedate, "-%m-%d"))
  
# use date to extract date list
chosendates = list()
for (lakename in c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) {
    bestdates.lake = bestdates |> filter(location_name == lakename)
    fp = df.full.ice |> 
      filter(location_name == lakename) |> 
      mutate(wateryear = if_else(month(date_time) >= 10, year + 1, year)) |> 
      group_by(location_name, wateryear, date_time) %>%
      summarise() |> 
      slice(1:2) |> 
      group_by(location_name, wateryear) |> 
      summarize(
        date1 = min(date_time),
        date2 = max(date_time),
        .groups = "drop"
      ) |> 
      rowwise() %>%
      mutate(
        dec1 = as.Date(paste0(wateryear-1, bestdates.lake$usedate)),
        avg_date = as.Date(mean(c(as.numeric(date1), as.numeric(date2))), origin = "1970-01-01"),
        dist1 = abs(as.numeric(difftime(date1, dec1, units = "days"))),
        dist2 = abs(as.numeric(difftime(date2, dec1, units = "days"))),
        dist_avg = abs(as.numeric(difftime(avg_date, dec1, units = "days"))),
        closest = case_when(
          dist1 < dist2 & dist1 < dist_avg ~ "first",
          dist2 < dist1 & dist2 < dist_avg ~ "second",
          TRUE ~ "average"
        )
      ) |>
      rowwise() %>%
      mutate(
        chosen_date = case_when(
          closest == "first"   ~ date1,
          closest == "second"  ~ date2,
          closest == "average" ~ as.Date(mean(c(as.numeric(date1), as.numeric(date2))), origin = "1970-01-01")
        )
      ) %>%
      ungroup() %>%
      # select(location_name, wateryear, chosen_date) |>
      mutate(fakeyear = `year<-`(chosen_date, 2024)) |> 
      mutate(fakeyear2 = if_else(month(fakeyear) >= 10, `year<-`(fakeyear, 2023), fakeyear))
    
    chosendates[[lakename]] = fp
}

chosendates = bind_rows(chosendates)
