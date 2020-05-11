#' summary table
#'
#'
#' @param df dataframe
#' @param my_vars df with info about variables
#' @export

summary_table <- function(df, my_vars){
  # select numeric variables
  num_var <- names(select_if(df, is.numeric))
  # relabel every numeric variables by my_vars
  for (x in num_var){
    table1::label(df[[x]]) <- my_vars[my_vars$variable == x,]$short_label
  }
  # formula for setting col and row
  formula <- as.formula(paste( "~", paste(paste(num_var, collapse=" + "), names(select_if(df, is.character)), sep=" | ")))
  # get the summary table
  table1::table1(formula, data = df)

}
