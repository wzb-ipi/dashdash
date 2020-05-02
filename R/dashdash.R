#' dashdash
#'
#' Make a .html dashboard. Key required inputs are datasets with 1. outcomes 2. variable interpretations 3. other text 4. GIS information.
#'
#' Note I had a problem with `Error: isTRUE(gpclibPermitStatus()) is not TRUE` which was solved
#' with `install.packages('rgeos', type='source')` and `install.packages('rgdal', type='source')`
#'
#' @param title Dashboard title
#' @param subtitle Dashboard subtitle
#' @importFrom dplyr mutate filter
#' @importFrom skimr skim
#' @export
#' @examples
#' library(dplyr)
#'
#' my_data <-
#'   expand_grid(id = c("Bo", "Bombali", "Bonthe"), date = as.Date(c("2020-04-18", "2020-04-19", "2020-04-20"))) %>%
#'   data.frame(stringsAsFactors = FALSE) %>%
#'     mutate(n = 3) %>%
#'     uncount(n) %>%
#'     dplyr::mutate(
#'     market_open	 = rbinom(n(), 1, .5),
#'     price_rice	 = rbinom(n(), 1, .5),
#'     aware	 = rbinom(n(), 1, .5),
#'     water	 = rbinom(n(), 1, .5))
#'
#' my_vars <- data.frame(
#'   variable = c("market_open", "price_rice", "aware", "water"),
#'   family = c("markets", "markets", "actions", "actions"),
#'   short_label = c("Is market open?", "Price of a rice", "Aware of Covid19", "Access to water"),
#'   description = c("details on market open", "Price of a cup of rice", "Details on aware of Covid19", "Details on access to water"),
#'   stringsAsFactors = FALSE
#' )
#'
#' my_text <- data.frame(
#'   para = "Intro text",
#'   datanote = "Study specific note about data source"
#'   )
#'
#' my_maps <- "c:/temp/shapefiles"
#'
#' dashdash(output_file = "c:/temp/dashtest.html",
#'          title = "title2",
#'          subtitle = "subtitle here",
#'          author   = "D Ash",
#'          my_data = my_data,
#'          my_vars = my_vars,
#'          my_text = my_text,
#'          my_maps = my_maps)
#'

dashdash <- function(output_file,
                     title = NULL,
                     subtitle = NULL,
                     author = NULL,
                     my_data,
                     my_vars,
                     my_text,
                     my_maps,
                     ...){

  if(is.null(title)) title <- my_text$title
  if(is.null(title)) title <- "No title provided"

  if(is.null(subtitle)) subtitle <- my_text$subtitle
  if(is.null(subtitle)) subtitle <- " "

  if(is.null(author)) author <- my_text$author
  if(is.null(author)) author <- " "

  dashRmd  <- system.file("rmd", "dashdash.Rmd", package = "dashdash")
  childRmd <- system.file("rmd", "child.Rmd", package = "dashdash")
  rmarkdown::render(dashRmd,
                    output_file = output_file,
                    params = list(
                      set_title = title,
                      set_subtitle = subtitle
                      ),
                    ...)
}

