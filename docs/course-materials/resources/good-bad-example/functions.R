# Author: Juliet Cohen
# Date: September 17th, 2024

# Custom functions to source into `good_example.qmd`
# to demonstrate how to produce clean, succinct,
# professional materials. This doc serves as a
# resource for EDS 223.


# define function to load in spefied csv filename
# from anywhere in specified directory
load_csv <- function(filename = NULL, 
                     directory = here(),
                     communicate = FALSE) {
  
  library(here)
  library(tools)
  
  # if filename is not provided, prompt for it
  if (is.null(filename)) {
    stop("Filename must be provided")
  }
  
  if (file_ext(filename) == "") {
    stop("File extension is not provided")
  } else if (file_ext(filename) != "csv") {
    stop("File is not a csv")
  }
  
  if (communicate) {
    # communicate which full filepath is found
    cat(sprintf("Loading file '%s'\nfrom within directory '%s'\n", 
                filename,
                here(directory)))
  }
  
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
  
  # if any parameter is not provided, prompt for it
  parameters <- list(data = data, 
                     latitude_col = latitude_col, 
                     longitude_col = longitude_col)
  missing <- names(parameters)[sapply(parameters, is.null)]
  
  if (length(missing) > 0) {
    stop(sprintf("The following parameter(s) are missing: %s", 
                 paste(missing, collapse = ", ")))
  }
  
  # error if file is not a dataframe
  if (is.data.frame(data) == FALSE) {
    stop("Input data is not a dataframe")
  }
  
  # error if lat or lon cols do not exist in data 
  if (!all(c(latitude_col, longitude_col) %in% colnames(data))) {
    stop(paste("Either the latitude or longitude column is not", 
                "present in the dataframe"))
  }
  
  # convert data to spatial points and set CRS
  point_data <- sf::st_as_sf(data, 
                             coords = c(longitude_col, latitude_col),
                             crs = st_crs(4326)) %>%
                    filter(st_is_valid(.))

  # insert warning if conversion to point spatial data failed for
  # at least 1 lat & long pair
  if (nrow(point_data) < nrow(data)){
    warning(paste("Conversion to point spatial data failed", 
                   "for at least 1 lat & long pair"))
  }

  paste0("Converted file to spatial point data\n",
         "Set CRS to 4326")
  
  return(point_data)
  
}






