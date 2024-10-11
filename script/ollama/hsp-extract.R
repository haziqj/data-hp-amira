install.packages("ollamar")
library(ollamar)
library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)
library(stringr)
library(purrr)

#cleaning the raw ig data - only .txt files
my_files <- list.files(
  "/Users/amirabarizah/Documents/data/raw/2024-09-14/", 
  full.names = TRUE, 
  recursive = TRUE, 
  pattern = "*.txt"
)

#creating a tibble (hsp) from captions (desc) in my_files & read each lines
hsp <- 
  tibble(
    file = my_files
  ) |>
  mutate(
    desc = unlist(
      map(file, \(x) paste0(readLines(x), collapse = "\n"), .progress = TRUE)
    )
  )

#extracting the dates 
dates <- gsub("raw/2024-09-14//", "", my_files)
dates <- sub(".*([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", dates)
hsp$date <- lubridate::date(dates)

#test connection - ollama
test_connection()
pull("llama3.1")

#filtering hsp file from unnecessary captions
hsp_filtered <- hsp %>%
  filter(
    str_count(desc, "\\w+") > 5, 
    !str_detect(desc, regex("thank you|land for sale", ignore_case = TRUE)),  # Exclude these phrases
    str_detect(desc, "\\d"),       # Must contain at least one digit
    str_detect(desc, "\\$")        # Must contain a dollar sign
  )


#creating a vector of captions
hsp_captions <- hsp_filtered$desc 

#function with ollama prompt 
clean_desc <- function(desc) {
  desc_clean <- gsub('"', '', desc)
  
  prompt <- paste0(
    "Extract the following information from a real estate description in Brunei:
    1. Kampong name in Brunei: [CHARACTER]
    2. Price: [NUMERIC, no commas for thousands]
    3. Storey of the property: [Double Storey = 2, Single Storey or Bungalow = 1]
    4. Status of the property: [One of Complete, Under-construction, Proposed] [CHARACTER]
    5. Built-up area: [NUMERIC, in square feet]
    6. Land size: [NUMERIC, in acres]
    7. Types of property: [Residential: [detached or semi-detached or terrace or apartment, other] or Commercial/land: [Return 'commercial' or 'land' if the property is not a residential building]] [CHARACTER]
    8. Number of bedrooms: [NUMERIC]
    9. Number of bathrooms: [NUMERIC]
    
    Output Format:
    - Return values as semicolon-separated
    - If information is missing or not applicable, use NA
    - Return only one listing (the first one, if there are multiple)
    - NO EXTRA INFORMATION other than the specified values provided
    
    ONLY RETURN THE RESULT AS IN THIS FORMAT:
    kelugos; 200000; 1; under-construction; 1500; 0.14; semi-detached; 2; 3
    NA; NA; 2; completed; 1800; 0.18; detached; 2; 2
    NA; NA; NA; NA; NA; NA; commercial; NA; NA

    Special Instructions:

    If the listing is for land or commercial properties, use NA for the built-up area and set the type to 'land' or 'commercial'.
    If the description is missing, return only NA.
    ----------

    Caption: '", desc_clean, "'"
  )
  
  result <- generate("llama3.1", prompt, output = "text", temperature = 0.1)
  
  return(result)
}

#func. to loop all captions - using map to track progress
hsp_scraped <- map(
  .x = hsp_captions[1410:min(1420, length(hsp_captions))], 
  .f = possibly(clean_caption, NA),
  .progress = TRUE
)

#check results
print(hsp_scraped)

#unlist
results_df <- data.frame(hsp_scraped = unlist(hsp_scraped))

#write into excel
library(writexl)
write_xlsx(results_df, "/Users/amirabarizah/Documents/data-hp/data/hsp_trial.xlsx")



