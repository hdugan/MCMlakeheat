
# Priscu, J. 2022. Dissolved organic carbon (DOC) concentrations in discrete water column samples collected from 
# lakes in the McMurdo Dry Valleys, Antarctica (1993-2022, ongoing) ver 11. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/a5d82d5d2167679c8ecff0d8ad06c0ee (Accessed 2024-07-22).

# Package ID: knb-lter-mcm.60.11 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/60/11/3385110b19664543ed4c097aff6041b9" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

doc <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  select(location_name, date_time, depth_m, doc_mgl) |>
  filter(location_name %in% c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |> 
  mutate(depth.asl = masl.approx - depth_m)



ggplot(doc) +
  geom_point(aes(y = depth.asl, x = doc_mgl, fill = location_name), shape = 21, stroke = 0.2, size = 1) +
  # geom_path(aes(y = depth.asl, x = chl_ug_chla_l, group = date_time, color = location_name), width = 0.1) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  xlab('DOC (mg L^-1 )') +
  ylab('Depth (m asl)') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text = element_text(size = 7), 
    legend.key.size = unit(0.2,'cm'),
    legend.margin = margin(0, 0, 0, 0),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown())

# Timeseries 
ggplot(doc |> filter(depth_m == 15)) +
  geom_point(aes(x = date_time, y = doc_mgl)) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1)

