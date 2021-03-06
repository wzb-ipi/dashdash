% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_args.R
\name{check_my_data}
\alias{check_my_data}
\title{Check my_data}
\usage{
check_my_data(my_data)
}
\arguments{
\item{my_data}{dataframe with data on variables listed in `my_vars`. Should contain a `id` variable that connects with units in map files and a `date` variable.}
}
\description{
Check my_data
}
\examples{
my_vars <- data.frame(
variable = c("var1", "var2",  "var1", NA),
family = c("fam1", "fam2", "fam1", NA),
short_label = c("label1", "label2", "label1", NA),
description = c("d1", "d2", "d1", NA),
stringsAsFactors = FALSE
)
\dontrun{check_my_data(my_vars)}

}
