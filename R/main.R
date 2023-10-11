###########################################
#         Mapa de áreas habitadas
#             Carlos Marcos
#              2023/10/10
#
#  Basado en Tutorial de Milos Popovic
#
###########################################



# 0. LIBRARY
#--------------------------------------------------------
libs <- c("tidyverse", "terra", "giscoR", "ggrepel", "ggthemes")

installed_libs <- libs %in% rownames(installed.packages())

if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = TRUE))



# 1. DOWNLOAD GHSL DATA - Global Human Settlement Layer
#--------------------------------------------------------

# Parameters
# Details in https://ghsl.jrc.ec.europa.eu/download.php?ds=pop
product <- "POP"          # population
epoch <- "2020"           # year
resolution <- "30 arcsec" # resolution in meters or arcsec / 30 arcsec ~ 900 meters

mapear_coord_sys <- function(coord_sys) {
  mapeo <- list(
    "100m" = "54009_100",
    "1km" = "54009_1000",
    "3 arcsec" = "4326_3ss",
    "30 arcsec" = "4326_30ss"
  )
  
  resultado <- mapeo[[coord_sys]]
  
  if (is.null(resultado)) {
    resultado <- "Valor no reconocido"
  }
  
  return(resultado)
}


param_name <-
  paste0("GHS_POP_E",
         epoch,
         "_GLOBE_R2023A_",
         mapear_coord_sys(resolution))

file_name <- paste0(param_name, "_V1_0.zip")

url <-
  paste0(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/",
    param_name,
    "/V1-0/"
  )

path_file_name <- paste0("data/", file_name)

getOption("timeout")     # current timeout
options(timeout = 1200)  # extend timeout

download.file(
  url = paste0(url, file_name),
  destfile = path_file_name,
  method = "auto",
  cacheOK = TRUE
)

options(timeout = 60)    # reset timeout



# 2. LOAD GHSL DATA
#--------------------------------------------------------

unzip(zipfile = path_file_name, exdir = "data/")

raster_name <- gsub(".zip", ".tif",
                    path_file_name)

pop <- terra::rast(raster_name)



# 3. COUNTRY SHAPEFILE
#--------------------------------------------------------

code_country <- c("AR", "FK")      # character vector of country codes
resolution_country <- "01"  # geospatial data's resolution

get_country_borders <-
  function(code = code_country, res = resolution_country) {
    country <- giscoR::gisco_get_countries(
      year = "2020",
      epsg = "4326",
      country = code_country,
      resolution = resolution_country)
    
    return(country)
  }

country <- get_country_borders()



# 4. CROP COUNTRY GHSL
#--------------------------------------------------------

country_pop <- terra::crop(pop,
                           terra::vect(country),
                           snap = "in",
                           mask = TRUE)



# 5. RASTER TO DATAFRAME
#-----------------------

country_pop_df <- as.data.frame(
  country_pop,
  xy = TRUE, na.rm = TRUE
)


names(country_pop_df)[3] <- "val"

min_density <- 20  # Minimum inhabitants per square kilometer

country_pop_df <- country_pop_df |>
  dplyr::mutate(
    hab = dplyr::if_else(val >= min_density, log(val), 0),
    intensity = ifelse(hab > 0, hab/max(hab), 1)
    )



# 6. MAP
#-------

cities <- data.frame(    # argentina customized cities
  city = c(
    "Buenos Aires", "Córdoba", "Santiago del Estero", "S. M. de Tucumán", "Salta",
    "San Juan", "Santa Fe", "Corrientes", "S. S. de Jujuy", "Resistencia",
    "Posadas", "Paraná", "Formosa", "Neuquén", "La Plata", "La Rioja", "San Luis",
    "Catamarca", "Mendoza", "Santa Rosa", "Río Gallegos", "Viedma", "Ushuaia", "Rawson",
    "Río Grande", "Comodoro Rivadavia", "Bahía Blanca", "Mar del Plata", "San Rafael",
    "Rosario", "Puerto Argentino", "Puerto Madryn"
  ),
  lat = c(
    -34.5997, -31.4167, -27.7833, -26.8167, -24.7833,
    -31.5342, -31.6333, -27.4833, -24.1833, -27.4514,
    -27.3667, -31.7331, -26.1833, -38.9525, -34.9211, -29.4125,
    -33.3000, -28.4667, -32.8833, -36.6167, -51.6233,
    -40.8000, -54.8019, -43.3000, -53.78769, -45.86413,
    -38.71959, -38.00042, -34.61772, -32.94682, -51.693861, -42.7692
  ),
  lng = c(
    -58.3819, -64.1833, -64.2667, -65.2167, -65.4167,
    -68.5261, -60.7000, -58.8167, -65.3000, -58.9867,
    -55.9000, -60.5297, -58.1833, -68.0642, -57.9544, -66.8542,
    -66.3333, -65.7833, -68.8167, -64.2833, -69.2161,
    -63.0000, -68.3031, -65.1000, -67.70946, -67.49656,
    -62.27243, -57.5562, -68.33007, -60.63932, -57.849556, -65.03851
  )
)
cities <- cities %>% 
  mutate(across(c(lat, lng), round, digits = 4))

p <- ggplot() +
  geom_raster(
    data = country_pop_df,
    aes(x = x,
        y = y,
        fill = hab
        )
    ) +
  geom_text_repel(
    data = cities,
    aes(x = lng, y = lat, label = city),
    fontface = "bold",
    color = "steelblue",              # Color del texto
    size = 2,                       # Tamaño del texto
    segment.color = "steelblue") +  # Transparencia del contorno de las etiquetas) +
  scale_fill_gradient(name = "",
                      low = "#0a1c29", high = "#FFFF00", na.value = "#0a1c29") +
  theme_map() +
  theme(
    legend.position = "none",
    plot.caption = element_text(
      size = 8, color = "grey10",
      hjust = .25, vjust = 10
    ),
    plot.margin = unit(
      c(
        t = 0, b = 0,
        l = -1, r = 0
      ), "lines"
    )
  ) +
  labs(
    title = "",
    caption = paste0(
      code_country,
      " - Pop. density (min ", min_density, " hab/km²) Data: Global Human Settlement Layer at ",
      resolution
    )
  )



# 7. SAVE
#--------------------------------------------------------

path_file_name <-
  paste0("plots/",
         code_country,
         "_",
         format(Sys.time(), format = "%Y%m%d_%H%M%S"),
         ".jpg")


ggsave(
  filename = path_file_name,
  plot = p,
  dpi = 1200,
  width = 6,
  height = 10,
  units = "in"
)

