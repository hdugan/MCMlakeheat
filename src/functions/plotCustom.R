usecolors = c("#BB9F2F", "#94B9AF", "#942911", "#593837")

plotCustom = list(scale_colour_grey(end = 0.5),
                  scale_fill_manual(values = usecolors),
                  theme_bw(base_size = 9),
                  facet_wrap(~location_name, scales = 'free_y', nrow = 1),
                  scale_x_continuous(limits = c(1992.5,2024), breaks = c(2000,2010,2020)), 
                  theme(axis.title.x = element_blank(),
                        legend.position = 'none',
                        legend.title = element_blank(),
                        strip.background = element_rect(fill = "white", color = NA),  # Set background to white and remove border
                        strip.text = element_text(color = "black", face = "bold", size = 10))
)