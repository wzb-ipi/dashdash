
#' ft country graphs
#'
#' Make Financial Times style country graphs
#'
#' @param my_country_code Three letter country code
#' @param df dataframe with corona cases containing outcome
#' @param plot_title optional plot title
#' @param lab_q_x quantile for labelling on x axis
#' @param lab_q_y quantile for labelling on y axis
#' @export

ft_country_graphs <- function(my_country_code, df,
                              lab_q_x = 0.95, lab_q_y = 0.95,
                              plot_title = '' ) {
  df <- filter(df, !is.na(region))
  data_date <- max(as.Date(df$DateRep), na.rm = TRUE)
  df_today  <- df %>% filter(as.Date(df$DateRep) == data_date)
  my_region <- df %>% filter(GeoId2 == my_country_code) %>% pull(region) %>% as.character() %>%   unique
  parts <- df_today %>%
    mutate(my_group  = ifelse(region != my_region, "Rest of World", ""),
           my_group  = ifelse(region == my_region, my_region, my_group),
           my_group  = ifelse(GeoId2 == my_country_code, my_region, my_group),
           my_group  = factor(my_group, levels=c(my_region,"Rest of World"), labels=c(my_region,"Rest of World")) ) %>%
    dplyr::select(GeoId2, my_group)

  ## Plot data

  plot_data <- left_join(df, parts) %>%
    filter(elapsed_rel > -5 & !is.na(my_group)) %>%
    select(elapsed_rel, deaths_cumul, GeoId2, my_group)



  ## Label data

  label_data <- plot_data %>%
    arrange(elapsed_rel) %>%
    group_by(GeoId2) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    group_by(my_group) %>%
    mutate(high_x = ifelse(elapsed_rel > quantile(elapsed_rel, lab_q_x, na.rm = T),
                           1, 0)) %>%
    mutate(high_y = ifelse(deaths_cumul > quantile(deaths_cumul, lab_q_y, na.rm = T),
                           1, 0)) %>%
    mutate(sparse = (n() < 20)) %>%
    ungroup() %>%
    mutate(h_country= ifelse(GeoId2 == my_country_code, 1, 0)) %>%
    filter(h_country == 1 | (high_x == 1) | (high_y == 1) | sparse ==1)

  ## BW Countries
  plot_data_bw <- plot_data %>%
    filter(!(GeoId2 %in% label_data$GeoId2))

  ## Color countries
  plot_data_col <- plot_data %>%
    filter(GeoId2 %in% label_data$GeoId2)

  ## Plot
  ggplot(plot_data_bw, aes(x=elapsed_rel,
                           y=deaths_cumul,
                           group = GeoId2)) +
    #geom_point() +
    geom_line(color = 'grey70', alpha = 0.4) +
    geom_line(data = dplyr::filter(plot_data_col, GeoId2 != my_country_code),
              mapping = aes(x=elapsed_rel,
                            y=deaths_cumul,
                            color = GeoId2)) +
    #my_country in my_region
    geom_line(data = dplyr::filter(plot_data_col, GeoId2 == my_country_code),
              mapping = aes(x=elapsed_rel,
                            y=deaths_cumul), color="black", size=1)  +
    #my_country in rest of the world
    geom_line(data = plot_data_col %>%
                filter(GeoId2 == my_country_code) %>%
                mutate(my_group=replace(my_group, my_group==my_region, "Rest of World")),
              mapping = aes(x=elapsed_rel,
                            y=deaths_cumul), color="black", size=1) +

    ggrepel ::geom_text_repel(data = dplyr::filter(label_data, GeoId2 != my_country_code),
                              mapping  =aes(label = GeoId2, x = elapsed_rel,
                                            y = deaths_cumul, color = GeoId2),
                              nudge_x = 5) +

    ggrepel ::geom_text_repel(data = label_data %>%
                                filter(GeoId2 == my_country_code) %>%
                                mutate(my_group=replace(my_group, my_group==my_region, "Rest of World")),
                            mapping  =aes(label = GeoId2, x = elapsed_rel,
                                          y = deaths_cumul),
                            nudge_x = 5,
                            segment.colour = "black", segment.size = 1) +

    ggrepel ::geom_text_repel(data = dplyr::filter(label_data, GeoId2 == my_country_code),
                              mapping  =aes(label = GeoId2, x = elapsed_rel,
                                            y = deaths_cumul),
                              nudge_x = 5,
                              segment.colour = "black",
                              segment.size = 1) +


    xlab("Days since 10th reported case") +
    ylab("Deaths (log scale)") +
    facet_grid( ~ my_group) +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    ggtitle(plot_title)+
    scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = FALSE))

}

