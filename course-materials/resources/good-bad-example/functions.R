# define function to load in spefied csv filename
# from anywhere in specified directory
load_csv <- function(filename = NULL, directory = here()) {
  
  library(here)
  library(tools)
  
  # if filename or directory are not strings, convert them
  if (!all(is.character(c(filename, directory)))) {
    filename <- as.character(filename)
    directory <- as.character(directory)
  }
  
  # error if file is not a csv
  if (file_ext(filename) != "csv") {
    stop("File is not a csv")
  }
  
  # communicate which full filepath is found
  sprintf("Loading file: '%s'\n", filename)
  
  # read in csv
  read_csv(list.files(path = here(directory),
                      pattern = filename,
                      recursive = TRUE,
                      full.names = TRUE),
           show_col_types = FALSE)
}



# define function to convert dataframe to spatial points dataframe
to_spatial_points <- function(data = NULL, 
                              latitude_col = NULL, 
                              longitude_col = NULL) {
  
  library(sf)
  
  # error if file is not a dataframe
  if (is.data.frame(data) == FALSE) {
    stop("Input data is not a dataframe")
  }
  
  # error if lat or lon cols do not exist in data 
  if (!all(c(latitude_col, longitude_col) %in% colnames(data))) {
    stop(paste("Either the latitude or longitude column is not", 
                "present in the dataframe"))
  }
  
  # if colnames are not strings, convert them
  if (!all(is.character(c(latitude_col, longitude_col)))) {
    filename <- as.character(filename)
    directory <- as.character(directory)
  }
  
  # convert data to spatial points and set CRS
  point_data <- sf::st_as_sf(data, 
                             coords = c(longitude_col, latitude_col),
                             crs = st_crs(4326)) %>%
                    filter(st_is_valid(.))

  # insert warning if conversion to point spatial data failed for
  # at least 1 lat & long pair
  if (length(point_data) < length(data)){
    warning(paste("Conversion to point spatial data failed", 
                   "for at least 1 lat & long pair"))
  }

  paste0("Converted file to spatial point data\n",
         "Set CRS to 4326")
  
  return(point_data)
  
}






