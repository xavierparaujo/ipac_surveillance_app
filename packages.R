# Function to load or install packages
load_or_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      message(paste0("Installing package '", package, "'..."))
      install.packages(package, repos = "https://cloud.r-project.org/")
    }
    
    message(paste0("Loading package '", package, "'..."))
    library(package, character.only = TRUE)
  }
}

# List the required packages
required_packages <- c(
  "tidyverse", 
  "lubridate",
  "glue",
  "shiny",
  "shinydashboard"
)

# Call the function to load or install packages
load_or_install_packages(required_packages)
