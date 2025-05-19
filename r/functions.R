plot_theme <- function(type = c("light", "dark")) { 
  
  base_theme <- theme(plot.title = element_text(face = "bold",
                                                hjust = 0.5, 
                                                margin = margin(0,0,20,0)),
                      axis.title = element_text(face = "bold", size = rel(1)),
                      axis.title.y = element_text(angle = 90, vjust = 2),
                      axis.title.x = element_text(vjust = -0.2),
                      panel.grid.minor = element_blank(),
                      legend.position = "right",
                      legend.direction = "vertical",
                      legend.box = "vertical",
                      legend.key.size= unit(0.5, "cm"),
                      legend.title = element_blank(),
                      plot.margin=unit(c(10,5,5,5),"mm"))
  
  light_theme <- theme(plot.title = element_text(colour = "black"),
                       panel.background = element_rect(fill = "white"),
                       plot.background = element_rect(fill = NA),
                       panel.border = element_rect(colour = "black", fill = NA),
                       axis.line.x = element_line(colour="black"),
                       axis.line.y = element_line(colour="black"),
                       panel.grid.major = element_line(colour="#f0f0f0"),
                       legend.key = element_rect(colour = NA),
                       strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
                       strip.text = element_text(face = "bold"),
                       plot.caption = element_text(color = "black", face = "italic"))
  
  dark_theme <- theme(plot.title = element_text(colour = "white"),
                      panel.background = element_rect(colour = NA, fill = 'grey20'),
                      plot.background = element_rect(colour = NA, fill = '#2d2d2d'),
                      panel.border = element_rect(fill = NA, colour = NA),
                      axis.title = element_text(colour = 'white'),
                      axis.text = element_text(colour = 'grey70'),
                      axis.line.x = element_line(colour="grey70"),
                      axis.line.y = element_line(colour="grey70"),
                      axis.ticks = element_line(colour="grey70"),
                      panel.grid.major = element_line(colour="#262626"),
                      legend.background = element_rect(fill ='#262626'),
                      legend.text = element_text(color = 'white'),
                      legend.key = element_rect(colour = NA, fill = '#262626'),
                      strip.background = element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
                      strip.text = element_text(face = "bold", colour = 'white'),
                      plot.caption = element_text(color = "grey70", face = "italic"))
  
  if (type == "light") {
    theme_minimal(base_family = "sans",
                  base_size = 20) +
      base_theme +
      light_theme
  } else if (type == "dark") {
    theme_minimal(base_family = "sans",
                  base_size = 20) +
      base_theme +
      dark_theme
  } 
}

make_valuebox <- function(icon, color, value) {
  list(
    icon = icon,
    color = color,
    value = value
  )
}

get_overall_open <- function(country) {
  pubs_all |>
    filter(country_code == country,
           publication_year == prev_year) |>
    group_by(item_id) |> 
    mutate(duplicate = n()) |> 
    filter(duplicate == 1) |> 
    group_by(is_oa) |> 
    summarise(n = n()) |> 
    mutate(freq = n / sum(n)) |>
    filter(is_oa == TRUE) |> 
    select(freq) |> 
    pull()
}

get_annual_open <- function(country) {
  pubs_all |>
    filter(country_code == country) |> 
    group_by(item_id) |> 
    mutate(duplicate = n()) |> 
    filter(duplicate == 1) |>
    group_by(is_oa, publication_year) |> 
    summarise(n = n()) |>
    group_by(publication_year) |> 
    mutate(freq = n / sum(n)) |> 
    filter(is_oa == TRUE)
}

plot_trend <- function(x) {
  ggplot(x, aes(x = publication_year, y = pc_open, colour = institution)) +
    geom_line(size = 1) +
    xlab("") +
    ylab("Proportion Open Access (%)") +
    scale_x_continuous(n.breaks = prev_year-min(pubs_all$publication_year))
}

plot_inst <- function(x, plot_theme) {
  plot_title <- (x$institution) 
  
  plot <- ggplot(x, aes(x = publication_year, y = pc, fill = oa_status)) +
    geom_area() +
    scale_fill_manual(name = "Access Type",
                      values = plot_colours) +
    xlab("") +
    ylab("") +
    scale_x_continuous(n.breaks = prev_year-min(pubs_all$publication_year)) +
    scale_y_continuous(labels = label_percent()) +
    plot_theme(plot_theme)
  
  plot_final <- plot + ggtitle(plot_title)
  
  return(plot_final)
}

