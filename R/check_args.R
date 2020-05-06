#' Check my_vars
#'
#' @param my_vars dataframe with mapping from variable names to variable families and labels. One row per variable.
#'
#' @examples
#' my_vars <- data.frame(
#' variable = c("var1", "var2",  "var1", NA),
#' family = c("fam1", "fam2", "fam1", NA),
#' short_label = c("label1", "label2", "label1", NA),
#' description = c("d1", "d2", "d1", NA),
#' stringsAsFactors = FALSE
#' )
#' \dontrun{check_my_vars(my_vars)}
#'

check_my_vars <- function(my_vars) {
  coll = makeAssertCollection()
  assert_data_frame(my_vars, add = coll)
  assert_character(my_vars$variable, any.missing = FALSE, add = coll)
  assert_character(my_vars$variable, unique = TRUE, add = coll)
  assert_character(my_vars$family, any.missing = FALSE, add = coll)
  reportAssertions(coll)
}

#' Check my_data
#'
#' @param my_data dataframe with data on variables listed in `my_vars`. Should contain a `id` variable that connects with units in map files and a `date` variable.
#'
#' @examples
#' my_vars <- data.frame(
#' variable = c("var1", "var2",  "var1", NA),
#' family = c("fam1", "fam2", "fam1", NA),
#' short_label = c("label1", "label2", "label1", NA),
#' description = c("d1", "d2", "d1", NA),
#' stringsAsFactors = FALSE
#' )
#' \dontrun{check_my_data(my_vars)}
#'

check_my_data <- function(my_data) {
  coll = makeAssertCollection()
  assert_duplicates(my_data, add = coll)
  reportAssertions(coll)
}

#' Check if duplicates in data.frame
#'

check_duplicates = function(x) {
  if (!isTRUE(any(duplicated(x))))
    return(TRUE)
  if (any(duplicated(x)))
    return("there are duplicate observations in data.frame")
  return(TRUE)
}


#' Assert if duplicates in data.frame
#'

assert_duplicates = assertDuplicates = makeAssertionFunction(check_duplicates)


