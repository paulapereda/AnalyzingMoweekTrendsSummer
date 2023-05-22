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


category
color_value
color
1
Blazers y chaquetas
Negro
19.03
2
Blazers y chaquetas
Verde
7.61
3
Blazers y chaquetas
Azul
6.92
4
Blazers y chaquetas
Beige
6.92
5
Blazers y chaquetas
Gris
6.23
6
Blazers y chaquetas
Visón
5.19
7
Blazers y chaquetas
Chocolate
4.84
8
Blazers y chaquetas
Marrón
4.84
9
Blazers y chaquetas
Camel
4.50
10
Blazers y chaquetas
Blanco
4.15