source('packages.R')

# Generates a random infection database for the app to use
generate_database = function(number_of_records){
  # Set the number of records you want
  number_of_recordss = 10000
  
  # Create Medical_Record_Numbers
  set.seed(123)
  medical_record_number = sample(100000:999999, number_of_recordss)
  
  # Infection Type
  infection_type = sample(c("COVID-19", "CDI", "MRSA", "ESBL", "VRE"), number_of_recordss, replace = TRUE)
  
  # Classification
  classification = sample(c("HA", "CA"), number_of_recordss, replace = TRUE)
  
  # Create Sample_Datetimes
  sample_datetime = as.POSIXct(sample(seq(as.Date("2010/01/01"), as.Date("2022/12/31"), by="day"), number_of_recordss, replace =  TRUE))
  
  # Create Collected_Facility
  collected_facility = sample(c("Facility_A", "Facility_B", "Facility_C"), number_of_recordss, replace = TRUE)
  
  # Create Acquired_Facility
  acquired_facility = sample(c("Facility_A", "Facility_B", "Facility_C"), number_of_recordss, replace = TRUE)
  
  # Create Acquired_Unit
  acquired_unit = sample(c("Unit_1", "Unit_2", "Unit_3"), number_of_recordss, replace = TRUE)
  
  # Create fiscal_period
  fiscal_period = sample(1:12, number_of_recordss, replace = TRUE)
  
  # Create fiscal_quarter
  fiscal_quarter = ceiling(fiscal_period / 3)
  
  # Create fiscal_year
  fiscal_year = sample(2010:2022, number_of_recordss, replace = TRUE)
  
  # Create the data frame
  infection_db = data.frame(
    medical_record_number,
    sample_datetime,
    infection_type,
    classification,
    collected_facility,
    acquired_facility,
    acquired_unit,
    fiscal_period,
    fiscal_quarter,
    fiscal_year) %>%
    mutate(
      classification = case_when(
        classification == 'HA' & acquired_facility == collected_facility ~ 'HA-Your',
        classification == 'HA' & acquired_facility != collected_facility ~ 'HA-Another',
        TRUE ~ 'CA'),
      acquired_facility = case_when(
        classification != 'CA' ~ acquired_facility
      ),
      acquired_unit = case_when(
        classification != 'CA' ~ acquired_unit
      )
    )
  
  
  # Print the first few rows
  return(infection_db)
}

# import and clean data
import_database = function(path_to_data){
  
  infection_db = read.csv(path_to_data) %>%
    mutate(
      sample_datetime = as.POSIXct(sample_datetime),
      fiscal_period = factor(glue('{fiscal_year}-{sprintf("%02d", fiscal_period)}')),
      fiscal_quarter = factor(glue('{fiscal_year}-Q{fiscal_quarter}')),
      fiscal_year = factor(fiscal_year),
      infection_type = factor(infection_type),
      collected_facility = factor(collected_facility),
      acquired_facility = factor(acquired_facility),
      acquired_unit = factor(acquired_unit),
      classification = factor(classification)
    )
  
  return(infection_db)
}