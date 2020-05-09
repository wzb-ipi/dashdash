

#' plot aggregates over time
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param vars vector of variable names
#' @param var_labs vector of variable labels
#' @param pd ggplot dodge parameter
#' @export


plot_aggregates <- function(df, my_vars, vars, var_labs, pd = ggplot2::position_dodge(.1)){

   national_plot <- df %>%

    select(all_of(c("date", vars))) %>%
    reshape2::melt(id.vars= c("date"))  %>%
    group_by(date, variable) %>%
    summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~length(.))) %>%
    mutate(se = sd / sqrt(length)) %>%

    ggplot(aes(x=date, y=mean)) +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    facet_wrap(~variable, scales = "free_y",
               labeller = labeller(variable = var_labs),
               strip.position = "top") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  if (!is.null(my_vars$min) & !is.null(my_vars$max)){

    datalist <-  list()

    for (index in vars) {
      selection <- filter(my_vars, variable == index) %>%
        select(variable, min, max)
      grid <- expand.grid(vars = selection$variable,
                          x = c(min(my_data$date), max(my_data$date)),
                          y = c(selection$min, selection$max))
      datalist[[index]] <- grid

    }

    ranges <- do.call(rbind, datalist)

    national_plot <- national_plot +  geom_blank(data = ranges, aes(x = x, y = y))

  }

  national_plot

}

