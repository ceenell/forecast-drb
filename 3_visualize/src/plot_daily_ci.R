
#' @description Plot of single lead time for temperature predictions across many sites
#' showing confidence intervals around predictions
#' @param ci_data forecast data filtered to 1-day out lead time
#' @param ci_list a list of confidence intervals to find upper and lower bounds 
#' must be a 0.1 degree increment ranging 0.1-0.9
plot_daily_ci <- function(ci_interval_data, ci_list, plot_date){
  
  plot_month <- lubridate::month(plot_date, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(plot_date)
  
  # import fonts
  font_fam <- 'Source Sans Pro'
  font_add_google(font_fam)
  showtext_opts(dpi = 300, regular.wt = 100, bold.wt = 700)
  showtext_auto(enable = TRUE)
  
  text_color <- "#444444"
  
  p <- ci_interval_data %>%
    filter(.width %in% ci_list) %>%
    ggplot(aes(y = reorder(seg_id_nat, temp),
               group = seg_id_nat,
               x = temp,
               xmin = .lower, xmax = .upper
    )) +
    geom_interval(
      aes(
        #interval_color = ..y..
      )
    ) +
    geom_tile(fill = 'white',
              width = 0.1) +
    theme_ci(font_fam)+
    scale_color_brewer() +
    labs(y = 'Stream Segment', x = 'Predicted Stream Temperature (F)') +
    scale_x_continuous(position = 'top') +
    guides(fill = guide_legend(
      "CI"
    ))
  
  # add threshold line if approaching 74F
  max_temp <- max(ci_interval_data$.upper)
  if ( max_temp > 70){
    plot_final <- p +
      geom_vline(xintercept = 74, linetype = "dotted")
  } else {
    plot_final <- p
  }
  
  return(plot_final)
 
}

theme_ci <- function(font_fam){
  theme_classic(base_size = 14)+
    theme(legend.position = c(0.8, 0.1),
          text = element_text(family = font_fam),
          axis.title = element_text(hjust = 0, face = "bold", lineheight = 1.5),
          axis.text = element_text(size = 8),
          legend.title = element_text(face = "bold"))
}
