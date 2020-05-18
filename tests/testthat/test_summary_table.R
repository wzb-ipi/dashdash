.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

  context(desc = "Testing summary_table")

  testthat::test_that(
    desc = "Testing summary_table",
    code = {
      df <- data_generator()[[1]]
      my_vars <- data_generator()[[2]]


      mean_df <- setDT(df)[, lapply(.SD, mean), by = id]


      expect_equal(get_current_means(as.data.frame(df), my_vars), mean_df)
    }
  )

}
