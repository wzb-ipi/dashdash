my_data <-
  expand_grid(id = c("Bo", "Bombali", "Bonthe"), date = as.Date(c("2020-04-18", "2020-04-19", "2020-04-20"))) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(n = 3) %>%
  uncount(n) %>%
  dplyr::mutate(
    market_open	 = rbinom(n(), 1, .5),
    price_rice	 = rbinom(n(), 1, .5),
    aware	 = rbinom(n(), 1, .5),
    water	 = rbinom(n(), 1, .5))

my_data <- unique(my_data)

my_vars <- data.frame(
  variable = c("market_open", "price_rice", "aware", "water"),
  family = c("Markets", "Markets", "Actions", "Actions"),
  short_label = c("Is market open?", "Price of a rice", "Aware of Covid19", "Access to water"),
  description = c("details on market open", "Price of a cup of rice", "Details on aware of Covid19", "Details on access to water"),
  stringsAsFactors = FALSE
)

my_args <- data.frame(
  para = "Intro text",
  datanote = "Study specific note about data source",
  map_path = "./docs/shapefiles",
  map_layer = "SLE_adm3",
  map_region =  "NAME_2",
  switch = "y",
  stringsAsFactors = FALSE,
  intro_text = "This dashboard provides real time data on current economic conditions and trends from a phone based survey being rolled out in 195 towns and villages across Sierra Leone.",
  intro_note = "Niccolo Meriggi (IGC Sierra Leone), Macartan Humphreys (WZB Berlin and Columbia Univerisity), Abou Bakarr Kamara (IGC Sierra Leone), Matthew Krupoff (Y-Rise), Madison Levine (Wageningen U), Mushfiq Mobarak (Yale University), Wilson Prichard (U Toronto), Ashwini Shridhar (Wageningen U), Peter van der Windt (NYU AD), Maarten Voors (Wageningen University). Feedback welcome! Please get in touch via email at tracking.corona.sl@gmail.com."

)

# dashdash(output_file = "C:/Users/julia/desktop/dasdash.html",
#          title = "title2",
#          subtitle = "subtitle here",
#          my_data = my_data,
#          my_vars = my_vars,
#          my_args = my_args)
