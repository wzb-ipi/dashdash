#' do_maps
#'
#' Pass your shapefile and function opens it
#'

do_maps <- function(map_path, map_region){

  if(is.null(map_path)) map_layer <- tools::file_path_sans_ext(basename(map_path))

  if(is.null(map_region)) map_region <- my_args$map_region

  # Prep maps

  if (!isTRUE(gpclibPermitStatus())){
    gpclibPermit()
  }

  shp <- readOGR(dsn = map_path,
                 layer = map_layer,
                 verbose = FALSE,
                 stringsAsFactors = FALSE)

  shp_df <- broom::tidy(shp, region = map_region)

}

#' auto_maps
#'
#' Get shapefile from GADMtools and open it
#'

auto_maps <- function(country_code, level=1, map_region="NAME_1"){
  shp <- GADMTools::gadm_sp_loadCountries(fileNames = country_code, level = 1, basefile = './shapefiles', simplify=0.01)
  shp_df <-broom::tidy(shp$spdf, region = map_region)
}
