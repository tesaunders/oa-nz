plot_theme <- function(base_size=20, base_family="sans") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold", colour = 'white',
                                     hjust = 0.5, margin = margin(0,0,20,0)),
           text = element_text(),
           panel.background = element_rect(colour = NA, fill = 'grey20'),
           plot.background = element_rect(colour = NA, fill = '#2d2d2d'),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1), colour = 'white'),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(colour = 'grey70'),
           axis.line.x = element_line(colour="grey70"),
           axis.line.y = element_line(colour="grey70"),
           axis.ticks = element_line(colour="grey70"),
           panel.grid.major = element_line(colour="#262626"),
           panel.grid.minor = element_blank(),
           legend.background = element_rect(fill ='#262626'),
           legend.text = element_text(color = 'white'),
           legend.key = element_rect(colour = NA, fill = '#262626'),
           legend.position = "right",
           legend.direction = "vertical",
           legend.box = "vertical",
           legend.key.size= unit(0.5, "cm"),
           legend.title = element_blank(),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#2D3A4C",fill="#2D3A4C"),
           strip.text = element_text(face="bold", colour = 'white')))
}

make_valuebox <- function(icon, color, value) {
  list(
    icon = icon,
    color = color,
    value = value
  )
}

get_open <- function(country) {
  pubs_summary |>
    filter(country_code == country,
           publication_year == prev_year) |> 
    mutate(
      is_oa = case_when(oa_status != "closed" ~ TRUE, .default = FALSE),
    ) |> 
    filter(is_oa == TRUE) |> 
    group_by(institution) |> 
    summarise(
      pc = sum(pc)
    ) |> 
    arrange(desc(pc))
}

annual_open <- function(country) {
  pubs_summary |>
    filter(country_code == country) |> 
    mutate(
      is_oa = case_when(oa_status != "closed" ~ TRUE, .default = FALSE),
    ) |> 
    filter(is_oa == TRUE) |> 
    group_by(institution, publication_year) |> 
    summarise(
      pc = sum(pc)
    ) |> 
    group_by(publication_year) |> 
    summarise(
      pc = mean(pc)
    )
}

plot_trend <- function(x) {
  ggplot(x, aes(x = publication_year, y = percent, colour = institution)) +
    geom_line(size = 1) +
    xlab("") +
    ylab("Proportion Open Access (%)") +
    scale_x_continuous(n.breaks = prev_year-min(pubs_summary$publication_year)) +
    plot_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

plot_inst <- function(x) {
  plot_title <- (x$institution) 
  
  plot <- ggplot(x, aes(x = publication_year, y = pc, fill = oa_status)) +
    geom_area() +
    scale_fill_manual(name = "Access Type",
                      values = plot_colours) +
    xlab("") +
    ylab("Proportion (%)") +
    scale_x_continuous(n.breaks = prev_year-min(pubs_summary$publication_year)) +
    plot_theme()
  
  plot_final <- plot + ggtitle(plot_title)
  
  return(plot_final)
}

