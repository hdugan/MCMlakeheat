library(dplyr)
library(EDIutils)
library(lubridate)

############################## DISCHARGE ############################## 
streams = data.frame(name = c('Canada','Crescent','Harnish', 'Aiken',
                              'Green','VonGuerard','LostSeal','Huey','Delta'),
                     package = c('knb-lter-mcm.9007.13',
                                 'knb-lter-mcm.9010.13','knb-lter-mcm.9015.9','knb-lter-mcm.9002.12',
                                 'knb-lter-mcm.9013.16','knb-lter-mcm.9027.20','knb-lter-mcm.9018.9',
                                 'knb-lter-mcm.9029.8','knb-lter-mcm.9011.14'))
# 'Commonwealth', 'knb-lter-mcm.9009.10'

getTotQ <- function(packagename, streamname) {
  # Download Discharge data
  res <- read_data_entity_names(packageId = packagename)
  # Read in discharge, change to water year 
  q.vg <- read_data_entity(packageId = packagename, entityId = res$entityId[1])
  q.vg <- readr::read_csv(file = q.vg) |> 
    mutate(date_time = mdy_hm(date_time)) |> 
    mutate(month = month(date_time)) |> 
    mutate(Year = if_else(month > 7, year(date_time) + 1, year(date_time))) |> 
    rename_at(vars(contains('rate')), ~ 'discharge.rate') |> 
    group_by(Year) |> 
    mutate(timestep = as.numeric(date_time - lag(date_time))) |> 
    mutate(q_L = discharge.rate * timestep * 60) |> 
    mutate(q_m3 = q_L/1000) |> 
    filter(!is.na(water_temp) & water_temp >= -10) |> 
    mutate(spHeat_J_kgK = SW_SpcHeat(Temp = water_temp, S = 0, P = 1)) |>  
    mutate(spHeat_J_m3K = spHeat_J_kgK * 1000) |> 
    mutate(heat_J = spHeat_J_m3K * q_m3 * water_temp) |> 
    summarise(avgQ_Ls = mean(discharge.rate, na.rm = T),
              totQ_L = sum(q_L, na.rm = T), 
              totHeat_J = sum(heat_J, na.rm = T), 
              spHeat_J_kgK = mean(spHeat_J_kgK, na.rm = TRUE)) |>
    mutate(stream = streamname, yearStart = Year -1, yearEnd = Year) |> 
    select(stream, yearStart, yearEnd, avgQ_Ls, totQ_L, totHeat_J, spHeat_J_kgK)
  return(q.vg)
  
}

stream.Q.list = list() 
for (i in 1:nrow(streams)) {
  stream.Q.list[[i]] = getTotQ(packagename = streams$package[i], streamname = streams$name[i])
}

stream.Q.df = bind_rows(stream.Q.list)

p1 = ggplot(stream.Q.df) +
  geom_col(aes(x = yearEnd, y = totHeat_J/1e6, fill = stream)) +
  ylab('Heat (MJ)') + 
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

p2 = ggplot(stream.Q.df) +
  geom_col(aes(x = yearEnd, y = totHeat_J/1e6/(totQ_L/1000), fill = stream)) +
  ylab('Heat (MJ/m3)') + 
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())


heat.lake = heat.day |> filter(location_name == 'Lake Fryxell') |> 
  mutate(month = month(date_time)) |> 
  mutate(yearEnd = if_else(month > 7, year(date_time) + 1, year(date_time))) |> 
  group_by(yearEnd) |> 
  summarise(heat_lake = mean(heat_J, na.rm = TRUE))

b = stream.Q.df |> group_by(yearEnd) |> summarise(totHeat_J = sum(totHeat_J, na.rm = T)) |> 
  left_join(heat.lake) |> 
  mutate(perHeat = 100*totHeat_J/heat_lake)

p3 = ggplot(b) +
  geom_line(aes(x = yearEnd, y = perHeat)) +
  ylab('% Stream Heat vs. Lake') + 
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

p1/p2/p3 + plot_layout(heights = c(2,2,1), guides = 'collect')
ggsave(paste0('figures/SI_streamHeat.png'), width = 6.5, height = 6, dpi = 500)


# write_csv(stream.Q.df, 'mcm_totQ.csv')
# 
# ############################## AIR TEMPERATURE AND SOLAR RADIATION ############################## 
# 
# # Hourly met files
# metfiles = list.files('~/Documents/Rpackages/MCMmetprocessing/data_met_out/Historic_Hourly/', full.names = T)
# 
# 
# 
# # Degree data function with a cutoff date
# deg.days <-function(df, cutoff) {
#   df |> 
#     group_by(Year) |> 
#     filter(airt3m >= cutoff) |> 
#     mutate(airt3m = airt3m - cutoff) |> 
#     group_by(Year) |> 
#     summarise(dd = sum(airt3m*(timestep/24), na.rm = T)) |> 
#     mutate(dd = round(dd,2)) |> 
#     rename(!!paste0('degdays.',cutoff) := dd) 
# }
# 
# read_csv(metfiles[i]) |> 
#   mutate(Year = if_else(month(date_time) > 7, year(date_time) + 1, year(date_time))) |> 
#   filter(month(date_time) %in% c(11,12,1,2)) |> 
#   group_by(Year, site)
# 
# 
# met.list = list()
# for (i in 1:length(metfiles)) {
#   
#   met.raw = read_csv(metfiles[i]) |> 
#     mutate(Year = if_else(month(date_time) > 7, year(date_time) + 1, year(date_time))) |> 
#     filter(month(date_time) %in% c(11,12,1,2)) |> 
#     group_by(Year, site) |> 
#     mutate(timestep = as.numeric(date_time - lag(date_time))) |> 
#     dplyr::bind_rows(dplyr::tibble(par = numeric(), swradin = numeric()))# if column doesn't exist add empty column
#   
#   # # Test a bunch of cutoff values
#   degdays = data.frame(Year = 1994:2024) |>
#     left_join(deg.days(met.raw, 4.0)) |> 
#     left_join(deg.days(met.raw, 3.0)) |> 
#     left_join(deg.days(met.raw, 2.0)) |> 
#     left_join(deg.days(met.raw, 1.0)) |> 
#     left_join(deg.days(met.raw, 0)) |> 
#     left_join(deg.days(met.raw, -1)) |> 
#     left_join(deg.days(met.raw, -2))
#   
#   met.list[[i]] = met.raw |> 
#     summarise(airt3m.mean = mean(airt3m, na.rm = T), 
#               par.mean = mean(par, na.rm = T), 
#               sw.mean = mean(swradin, na.rm = T),
#               sum.na.airt3m = sum(is.na(airt3m)),
#               sum.na.swradin = sum(is.na(swradin)),
#               n = n()) |> 
#     ungroup() |> 
#     left_join(degdays) |> 
#     mutate(yearStart = Year -1, yearEnd = Year) |> 
#     select(site, yearStart, yearEnd, airt3m.mean:`degdays.-2`) %>%
#     mutate(across(starts_with("degdays"), ~ifelse(!is.na(airt3m.mean) & is.na(.), 0, .)))
# }
# 
# met.df = bind_rows(met.list)
# 
# ggplot(met.df) +
#   geom_point(aes(x = airt3m.mean, y = par.mean, col = site))
# 
# write_csv(met.df, 'mcm_metMeans.csv')

