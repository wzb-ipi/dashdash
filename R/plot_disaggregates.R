#' plot disaggregates over time
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param vars vector of variable names
#' @param pd ggplot dodge parameter
#' @export

plot_disaggregates <- function(df, my_vars, pd = ggplot2::position_dodge(.1)){

# ids <- droplevels(df$id) %>% levels()
ids <- unique(df$id)

vars <- pull(my_vars, variable)
var_labs <- my_vars  %>% pull(short_label)
names(var_labs) <- vars
date_range = c(min(df$date), max(df$date))

# New facet label names for supp variable

  datalist <-  lapply(vars, function(j){

    selection <- filter(my_vars, variable == j)

    expand.grid(variable = j,
                id = ids,
                date = date_range,
                min = ifelse(is.null(selection$min), NA, selection$min),
                max = ifelse(is.null(selection$max), NA, selection$max)
                )
  })

  ranges <- do.call(rbind, datalist)

  df %>%
    select(all_of(c("id", "date", vars))) %>%
    reshape2::melt(id.vars= c("id", "date"))  %>%
    group_by(id, date, variable) %>%
    summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~length(.))) %>%
    mutate(se = sd / sqrt(length),
           ymin=mean-1.96*se,
           ymax=mean+1.96*se) %>%
    left_join(ranges)  %>%
    mutate(floor = min-.01, ceiling = max+.01)  %>%

    ggplot(aes(x=date, y=mean)) +
      geom_point(position=pd) +
      geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1, position=pd) +
      geom_line(position=pd) +
      facet_grid(variable ~ id, scales = "free_y",
                 labeller = labeller(variable = var_labs),
                 switch = switch) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      geom_blank(aes(x=date, y=ceiling)) +
      geom_blank(aes(x=date, y=floor))

}
