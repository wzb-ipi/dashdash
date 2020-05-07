

#' plot aggregates over time
#'
#'
#' @param df dataframe
#' @param vars vector of variable names
#' @param var_labs vector of variable labels
#' @param pd ggplot dodge parameter
#' @export


plot_aggregates <- function(df, vars, var_labs, pd = ggplot2::position_dodge(.1))

df %>%

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
