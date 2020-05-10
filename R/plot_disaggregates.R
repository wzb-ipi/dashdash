#' plot disaggregates over time
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param pd ggplot dodge parameter
#' @export

plot_disaggregates <- function(df, my_vars, pd = ggplot2::position_dodge(.1), switch = "y"){

  vars <- pull(my_vars, variable)
  var_labs <- my_vars  %>% pull(short_label)
  names(var_labs) <- vars

  # Gather mins amd maxs
  ranges <- my_vars %>% select(one_of("variable", "min", "max"))
   if(!("min" %in% names(ranges))) ranges$min <- NA
   if(!("max" %in% names(ranges))) ranges$max <- NA

  # Reshape and bring in mins and maxs
  df2 <- df %>%
    select(all_of(c("id", "date", vars))) %>%
    reshape2::melt(id.vars= c("id", "date"))  %>%
    group_by(id, date, variable) %>%
    summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~length(.))) %>%
    mutate(se = sd / sqrt(length), ymin=mean-1.96*se, ymax=mean+1.96*se) %>%
    left_join(ranges)

  # Prep plot
  g <- df2 %>%
    ggplot(aes(x=date, y=mean)) +
      geom_point(position=pd) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1, position=pd) +
      geom_line(position=pd) +
      facet_grid(variable ~ id, scales = "free_y",
                 labeller = labeller(variable = var_labs),
                 switch = switch) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # Add floor and ceiling as ghost layer
  if(!(all(is.na(df2$max)))) g <- g + geom_blank(aes(x=df2$date, y=df2$max))
  if(!(all(is.na(df2$min)))) g <- g + geom_blank(aes(x=df2$date, y=df2$min))

  g

}
