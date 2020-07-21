#' current means
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @param recent how many days previous to most recent date to include (in days)
#' @export

get_current_means <- function(df, my_vars, recent = 1){

  vars <- pull(my_vars, variable)

  mx_date <- max(df$date)

  df[, c("id", "date", vars)] %>%
    filter(date <= max(date) + 1 -recent) %>%
    group_by(id)  %>%
    summarise_all( function(x)  mean(x, na.rm = T)) %>%
    mutate(date=mx_date)
}
