library(tibble)
library(stringr)
library(dplyr)
library(readxl)

# Function to parse individual data entries
parse_data <- function(text) {
  # Clean up text to remove unwanted characters and spaces
  text_cleaned <- str_squish(text)
  
  # Extract each component using regex
  date <- str_extract(text_cleaned, "(?<=Date: )\\d{4}\\d{2}\\d{2}")
  kampong <- str_extract(text_cleaned, "(?<=kampong: )[\\w\\s]+")
  price <- str_extract(text_cleaned, "(?<=price: )[^\\n\\s]+")  
  type <- str_extract(text_cleaned, "(?<=type: )[\\w\\s]+")
  storey <- str_extract(text_cleaned, "(?<=storey: )[\\w\\s]+")
  status <- str_extract(text_cleaned, "(?<=status: )[\\w\\s]+")
  land_size <- str_extract(text_cleaned, "(?<=land_size: )[\\d.]+\\s[\\w]+")
  floor_size <- str_extract(text_cleaned, "(?<=floor_size: )[\\d]+\\s[\\w]+")
  beds <- str_extract(text_cleaned, "(?<=beds: )[\\d]+\\s[\\w]+")
  baths <- str_extract(text_cleaned, "(?<=baths: )[\\d]+\\s[\\w]+")
  land_type <- str_extract(text_cleaned, "(?<=land_type: )[\\w\\s]+")
  
  # Extract Additional Remark section
  additional_remark_start <- str_locate(text_cleaned, "Additional Remark:")[, "end"]
  if (!is.na(additional_remark_start)) {
    additional_remark <- substr(text_cleaned, additional_remark_start + 1, nchar(text_cleaned))
    additional_remark <- str_squish(additional_remark)  # Remove extra white spaces
  } else {
    additional_remark <- NA
  }
  
  list(
    date = date,
    kampong = kampong,
    price = price,
    type = type,
    storey = storey,
    status = status,
    land_size = land_size,
    floor_size = floor_size,
    beds = beds,
    baths = baths,
    land_type = land_type,
    additional_remark = additional_remark
  )
}


# Function to parse all entries from the text data
parse_all_entries <- function(text_data) {
  # Initialize lists to store parsed results
  dates <- c()
  kampongs <- c()
  prices <- c()
  types <- c()
  storeys <- c()
  statuses <- c()
  land_sizes <- c()
  floor_sizes <- c()
  beds <- c()
  baths <- c()
  land_types <- c()
  additional_remarks <- c()
  
  # Split the text data into lines
  lines <- str_split(text_data, "\n")[[1]]
  
  # Initialize a variable to hold each entry
  current_entry <- ""
  
  # Loop through each line to process entries
  for (line in lines) {
    if (str_detect(line, "\\*\\*\\d+\\. Atlasproperties_bn")) {
      if (nchar(current_entry) > 0) {
        # Parse the current entry
        parsed <- parse_data(current_entry)
        # Append the results
        dates <- c(dates, parsed$date)
        kampongs <- c(kampongs, parsed$kampong)
        prices <- c(prices, parsed$price)
        types <- c(types, parsed$type)
        storeys <- c(storeys, parsed$storey)
        statuses <- c(statuses, parsed$status)
        land_sizes <- c(land_sizes, parsed$land_size)
        floor_sizes <- c(floor_sizes, parsed$floor_size)
        beds <- c(beds, parsed$beds)
        baths <- c(baths, parsed$baths)
        land_types <- c(land_types, parsed$land_type)
        additional_remarks <- c(additional_remarks, parsed$additional_remark)
      }
      # Start a new entry
      current_entry <- line
    } else {
      # Append the line to the current entry
      current_entry <- paste0(current_entry, "\n", line)
    }
  }
  
  # Don't forget to parse the last entry
  if (nchar(current_entry) > 0) {
    parsed <- parse_data(current_entry)
    dates <- c(dates, parsed$date)
    kampongs <- c(kampongs, parsed$kampong)
    prices <- c(prices, parsed$price)
    types <- c(types, parsed$type)
    storeys <- c(storeys, parsed$storey)
    statuses <- c(statuses, parsed$status)
    land_sizes <- c(land_sizes, parsed$land_size)
    floor_sizes <- c(floor_sizes, parsed$floor_size)
    beds <- c(beds, parsed$beds)
    baths <- c(baths, parsed$baths)
    land_types <- c(land_types, parsed$land_type)
    additional_remarks <- c(additional_remarks, parsed$additional_remark)
  }
  
  # Create a tibble from the results
  results_df <- tibble(
    date = dates,
    kampong = kampongs,
    price = prices,
    type = types,
    storey = storeys,
    status = statuses,
    land_size = land_sizes,
    floor_size = floor_sizes,
    beds = beds,
    baths = baths,
    land_type = land_types,
    additional_remark = additional_remarks
  )
  
  return(results_df)
}

# Example usage
extracted_data <- read.csv("/Users/amirabarizah/Documents/data-hp/data/results200.csv")
trial_result <- parse_all_entries(extracted_data)
print(trial_result)
