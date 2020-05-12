.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

  context(desc = "Testing current_means")

  testthat::test_that(
    desc = "Testing get_current_means",
    code = {
      df <-  expand_grid(id = c("Bo", "Bombali", "Bonthe"), date = as.Date(c("2020-04-18", "2020-04-19", "2020-04-20"))) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(n = 3) %>%
        uncount(n) %>%
        dplyr::mutate(
          market_open	 = rbinom(n(), 1, .5),
          price_rice	 = rbinom(n(), 1, .5),
          aware	 = rbinom(n(), 1, .5),
          water	 = rbinom(n(), 1, .5))
      my_vars <- data.frame(
        variable = c("market_open", "price_rice", "aware", "water"),
        family = c("Markets", "Markets", "Actions", "Actions"),
        short_label = c("Is market open?", "Price of a rice", "Aware of Covid19", "Access to water"),
        description = c("details on market open", "Price of a cup of rice", "Details on aware of Covid19", "Details on access to water"),
        stringsAsFactors = FALSE
      )
      vars <- pull(my_vars, variable)

      df[, c("id", "date", vars)] %>%
        filter(date <= max(date) + 1 -recent) %>%
        group_by(id)  %>%
        summarise_all( function(x)  mean(x, na.rm = T))

      expect_that(get_current_means(df, my_vars))
    }
  )

}
