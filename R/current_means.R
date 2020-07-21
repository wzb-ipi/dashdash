#' current means
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param recent how many days previous to most recent date to include (in days)
#' @export

get_current_means <- function(df, my_vars, recent = 1){

  vars <- pull(my_vars, variable)

  date <- df %>%
    select(id, date) %>%
    group_by(id) %>%
    summarize(date = max(date))

  avgs <- df[, c("id", vars)] %>%
    group_by(id)  %>%
    summarise_all( function(x)  mean(x, na.rm = T))

  left_join(date, avgs)
}

