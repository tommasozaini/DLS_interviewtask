#Functions to better visualize results

clean_variable_name <- function(variable_vector) {
  variable_vector <- str_replace(variable_vector, 'Final Energy\\|', '')
  return(variable_vector)
}

clean_model_name <- function(model_vector) {
  model_vector <- str_replace(model_vector, 'IMAGE 3.3', 'IMAGE')
  model_vector <- str_replace(model_vector, 'REMIND-MAgPIE 3.0-4.4','REMIND')
  return(model_vector)
}

clean_scenario_name <- function(scenario_vector) {
  scenario_vector <- str_replace(scenario_vector, 'SDP_', '')
  scenario_vector <- str_replace(scenario_vector, '-', ' ')
  return(scenario_vector)
}


#ISO to country names

iso_file_path <- here('iso3.inc')
iso_lines <- read_lines(iso_file_path)

# Process the lines to extract ISO codes and country names
iso_data <- iso_lines %>%
  str_subset("^[A-Z]{3}\\s+'") %>% # Keep only lines that contain 'ISO code' and country name
  str_match("([A-Z]{3})\\s+'(.*?)'") %>% # Extract ISO code and country name
  as.data.frame() %>% 
  select(iso = V2, country = V3)  # Rename columns for clarity (and keep only ISO and country columns)

iso_to_country <- function(iso_vector) {
  
  iso_vector <- as.character(iso_vector)
  result <- iso_data %>%
    filter(iso %in% iso_vector) %>%
    right_join(tibble(iso = iso_vector), by = "iso") %>%
    mutate(country = ifelse(is.na(country), iso, country)) %>%
    pull(country)
  
  return(result)
}