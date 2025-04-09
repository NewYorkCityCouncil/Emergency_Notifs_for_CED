## Load Libraries -----------------------------------------------
#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.
list.of.packages <- c("shiny", "readxl", "writexl", "dplyr", "tidyr", "ggplot2", 
                      "lubridate", "stringr", "knitr", "kableExtra", "sf", "leaflet", 
                      "htmltools", "htmlwidgets", "Kendall", "glue")
# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]
councilcount.check <- "councilcount" %in% installed.packages()[,"Package"]
# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
if(councilverse.check == FALSE) remotes::install_github("newyorkcitycouncil/councilverse")
if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councilcount")

# packages are loaded
lapply(c(list.of.packages,"councildown","councilverse", "councilcount"), require, character.only = TRUE)
# remove created variables for packages
rm(list.of.packages, new.packages, councildown.check, councilverse.check, councilcount.check)

