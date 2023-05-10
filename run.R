setwd(getwd())
source('functions.R')
source('app/pages/home.R')

PATH_TO_DATABASE = 'data/infection_database.csv'

write.csv(generate_database(10000), PATH_TO_DATABASE, row.names = FALSE)

INFECTION_DB <<- import_database(PATH_TO_DATABASE)

shiny::runApp('app/')
