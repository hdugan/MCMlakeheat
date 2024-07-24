# Get cast day that minimizes RMSE of difference in days between casts 

getBestCastDay <- function(df, lakename, useday, usemonth) {
  nov.cast = df |> 
    filter(location_name == lakename) |> 
    select(location_name, date_time) |> 
    mutate(wyear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time))) |> 
    mutate(cast.day = ymd(paste(wyear-1, usemonth,useday, sep = '-'))) |> 
    mutate(cast.diff = abs(cast.day - date_time)) |> 
    group_by(location_name, wyear) |> 
    arrange(location_name, wyear, cast.diff) |> 
    summarise_all(first) |> 
    filter(!wyear %in% c(1994:1995))
  
  rmse = nov.cast |> ungroup() |> 
    group_by(location_name) |> 
    mutate(cast.diff2 = as.numeric(cast.diff)^2) |> 
    summarise(rmse = sum(cast.diff2)/n()) |> 
    pull(rmse)
  return(rmse)
  
}

pullBestCastDay <- function(df, lakename, useday, usemonth) {
  nov.cast = df |> 
    filter(location_name == lakename) |> 
    mutate(wyear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time))) |> 
    mutate(cast.day = ymd(paste(wyear-1, usemonth,useday, sep = '-'))) |> 
    mutate(cast.diff = abs(cast.day - date_time)) |> 
    group_by(location_name, wyear) |> 
    arrange(location_name, wyear, cast.diff) |> 
    summarise_all(first) |> 
    filter(!wyear %in% c(1994:1995)) |> 
    select(-cast.day)

  return(nov.cast)
  
}
