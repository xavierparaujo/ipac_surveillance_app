setwd(getwd())
source('functions.R')

PATH_TO_DATABASE = 'data/infection_database.csv'

write.csv(generate_database(), PATH_TO_DATABASE, row.names = FALSE)

INFECTION_DB <<- import_database(PATH_TO_DATABASE)

PT_DAYS <<- INFECTION_DB %>%
  select(fiscal_quarter, collected_facility) %>%
  unique() %>%
  complete(fiscal_quarter, collected_facility) %>%
  mutate(pt_days = sample(1000:3000, 156))
  
FACILITIES <<- INFECTION_DB$collected_facility %>% unique()

UNITS <<- INFECTION_DB$collected_unit %>% unique()

source('app/pages/covid.R')
source('app/pages/cdi.R')
source('app/pages/mrsa.R')
source('app/pages/esbl.R')
source('app/pages/vre.R')

shiny::runApp('app/')
