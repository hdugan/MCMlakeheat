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
