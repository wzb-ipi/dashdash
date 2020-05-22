#' do_maps
#'
#' Pass your shapefile and function opens it
#'

do_maps <- function(map_path, map_region){

  if(!is.null(map_path)) map_layer <- tools::file_path_sans_ext(basename(map_path))

  if(is.null(map_region)) map_region <- my_args$map_region

  # Prep maps

  if (!isTRUE(gpclibPermitStatus())){
    gpclibPermit()
  }

  shp <<- readOGR(dsn = map_path,
                 layer = map_layer,
                 verbose = FALSE,
                 stringsAsFactors = FALSE)

  shp_df <<- broom::tidy(shp, region = map_region)

}

#' auto_maps
#'
#' Get shapefile from GADMtools and open it
#'

auto_maps <- function(country_code, level=1, map_region="NAME_1"){
  shp <<- GADMTools::gadm_sp_loadCountries(fileNames = country_code, level = 1, basefile = './shapefiles', simplify=0.01)
  shp_df <<-broom::tidy(shp$spdf, region = map_region)
}

#' add_maps
#'
#' Make maps and plot them
#'

add_maps <- function(my_subset, current_means, shp_df){
  vars <- pull(my_subset, variable)
  var_labs <- my_subset  %>% pull(short_label)
  names(var_labs) <- vars


  scale2 <- function(x, na.rm = FALSE) {
    x <- as.numeric(x)
    (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)}

  par(mar=c(0,0,0,0))

  current_means_map <- current_means %>% data.frame()
  if(scale_vars)
    current_means_map <- current_means_map %>%
    mutate_at(my_subset$variable, scale2)

  df_map <- left_join(shp_df, current_means_map)  %>% select(-date) %>%
    reshape2::melt(id.vars= c("id", "long", "lat", "order", "hole", "piece", "group"))

  if (nrow(filter(df_map, is.na(value)))!=0){
    check_na <- filter(df_map, is.na(value))
    check_na <- unique(check_na$id)
    for (place in check_na) {
      warning(paste0(place, " did not find matches "))
    }
  }

  ggplot() +
    geom_polygon(data = df_map,
                 aes( x = long, y = lat,  group = group, fill=value), color="white") +
    theme_void() + coord_map() + facet_wrap(~variable, labeller = labeller(variable = var_labs))

}
