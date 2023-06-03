estilo_paula <- function() {
  theme_minimal(base_family = "Belleza",
                base_size = 18) +
    theme(axis.line.y = element_line(),
          axis.line.x = element_line(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          plot.title = element_text(hjust = .5, 
                                    family = "Belleza"),
          plot.subtitle = element_text(hjust = .5),
          plot.caption = element_text(family = "Belleza", hjust = 1, 
                                      margin = margin(t = 10)),
          panel.grid.major.y = element_line(),
          legend.position = "bottom")
}

theme_set(estilo_paula())
update_geom_defaults("text", list(family = theme_get()$text$family))
