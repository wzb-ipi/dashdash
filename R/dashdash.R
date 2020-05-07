#' dashdash
#'
#' Make a .html dashboard. Key required inputs are datasets with 1. outcomes 2. variable interpretations 3. other text 4. GIS information.
#'
#' Note I had a problem with `Error: isTRUE(gpclibPermitStatus()) is not TRUE` which was solved
#' with `install.packages('rgeos', type='source')` and `install.packages('rgdal', type='source')`
#'
#' @param output_file Path to where ouutput should be written, e.g. "docs/index.html"
#' @param my_vars dataframe with mapping from variable names to variable families and labels. One row per variable.
#' @param my_data dataframe with data on variables listed in `my_vars`. Should contain a `id` variable that connects with units in map files and a `date` variable.
#' @param my_args dataframe with arguments to customize a dashboard.
#' @param title Dashboard title
#' @param subtitle Dashboard subtitle
#' @param author String, names of authors.
#' @param map_path String, path to map shapefiles.
#' @param map_region String, map region.
#' @param map_layer String, map layer.
#' @param switch ggplot2 argument: By default, the labels are displayed on the top and right of the plot. If "x", the top labels will be displayed to the bottom. If "y", the right-hand side labels will be displayed to the left. Can also be set to "both".
#' @param scale_vars Logical. Whether to scale_vars variabels before map plotting.
#' @param ft_plot Add a Financial Times type plot
#' @param country_code three letter country code used for Financial Times plot
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
#'   family = c("Markets", "Markets", "Actions", "Actions"),
#'   short_label = c("Is market open?", "Price of a rice", "Aware of Covid19", "Access to water"),
#'   description = c("details on market open", "Price of a cup of rice", "Details on aware of Covid19", "Details on access to water"),
#'   stringsAsFactors = FALSE
#' )
#'
#' my_args <- data.frame(
#'   para = "Intro text",
#'   datanote = "Study specific note about data source",
#'   map_path = "c:/temp/shapefiles",
#'   map_layer = "SLE_adm3",
#'   map_region =  "NAME_2",
#'   stringsAsFactors = FALSE
#'   )
#'
#' dashdash(output_file = "dashtest.html",
#'          title = "title2",
#'          subtitle = "subtitle here",
#'          my_data = my_data,
#'          my_vars = my_vars,
#'          my_args = my_args)
#'

dashdash <- function(output_file,
                     my_vars,
                     my_data,
                     my_args,
                     title = NULL,
                     group = NULL,
                     subtitle = NULL,
                     author = NULL,
                     add_maps = NULL,
                     map_path = NULL,
                     map_region = NULL,
                     map_layer = NULL,
                     scale_vars = NULL,
                     pd_width = .1,
                     ft_plot = NULL,
                     country_code = NULL,
                     ...){

  # Check integirty of inputs
  check_my_vars(my_vars)
  check_my_data(my_data)

  if(is.null(title)) title <- my_args$title
  if(is.null(title)) title <- "No title provided"

  if(is.null(group)) group <- my_args$group
  if(is.null(group)) group <- "Group"

  if(is.null(subtitle))   subtitle <- my_args$subtitle

  if(is.null(author))  author <- my_args$author

  if(is.null(map_path)) map_path <- my_args$map_path
  if(is.null(map_path)) map_path <-  system.file("shapefiles", package = "dashdash")

  if(is.null(add_maps)) add_maps <- my_args$add_maps
  if(is.null(add_maps)) add_maps <- ifelse(is.null(my_args$map_path), FALSE, TRUE)

  if(add_maps){
    if(is.null(map_region)) map_region <- my_args$map_region
    if(is.null(map_layer)) map_layer   <- my_args$map_layer
    if(is.null(map_layer)) stop("Map layer should be provided; e.g. `SLE_adm3`")

    # Prep maps

    if (!isTRUE(gpclibPermitStatus())){
      gpclibPermit()
    }

    shp <- readOGR(dsn = map_path,
                   layer=map_layer,
                   verbose=FALSE,
                   stringsAsFactors = FALSE)

    shp_df <- broom::tidy(shp, region = map_region)

    }

  if(is.null(scale_vars)) scale_vars <- my_args$scale_vars
  if(is.null(scale_vars)) scale_vars <- FALSE

  # Prep FT plot arguments
  if(is.null(ft_plot)) ft_plot <- my_args$ft_plot
  if(is.null(ft_plot)) ft_plot <- FALSE
  if(ft_plot){
    if(is.null(country_code)) country_code <- my_args$country_code
    if(is.null(country_code)) stop("ft graphic requires country_code argument")
    ft_data <- read.csv("https://wzb-ipi.github.io/corona/df_full.csv")
  }

  # Prep graph options
  switch  <- my_args$switch
  pd <- ggplot2::position_dodge(pd_width)


  # Data checks
  if(!all(c("date", "id") %in% names(my_data))) stop("my_data should include date and id variables")
  my_data <- mutate(my_data, date = as.Date(date))

  # Get Rmd paths
  dashRmd     <- system.file("rmd", "dashdash.Rmd", package = "dashdash")
  childRmd    <- system.file("rmd", "child.Rmd", package = "dashdash")
  ftplotRmd   <- system.file("rmd", "ft_plot.Rmd", package = "dashdash")
  add_mapsRmd <- system.file("rmd", "add_maps.Rmd", package = "dashdash")

  rmarkdown::render(dashRmd,
                    output_file = output_file,
                    params = list(
                      set_title = title,
                      set_subtitle = subtitle
                      ),
                    ...)
}

