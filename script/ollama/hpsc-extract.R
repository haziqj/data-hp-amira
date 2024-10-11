install.packages("ollamar")
library(ollamar)
library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)
library(stringr)
library(purrr)

#test connection - ollama
test_connection()
pull("llama3.1")

#filtering hsp file from unnecessary captions
hpsc_filtered <- hpsc %>%
  mutate(caption = as.character(caption)) %>%
  filter(
    !str_detect(caption, regex("land for sale|thank you", ignore_case = TRUE)),  # Omit rows with "land for sale" or "thank you"
    str_detect(caption, "\\d")  # Omit rows without digits
  )

#creating a vector of captions
hpsc_captions <- hpsc_filtered$caption 

#function with ollama prompt 
clean_caption <- function(caption) {
  caption_clean <- gsub('"', '', caption)
  
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

    Caption: '", caption_clean, "'"
  )
  
  result <- generate("llama3.1", prompt, output = "text")
  
  return(result)
}

#func. to loop all captions - using map to track progress
hpsc_scraped <- map(
  .x = hpsc_captions[1:min(1300, length(hpsc_captions))], 
  .f = possibly(clean_caption, NA),
  .progress = TRUE
)

#unlist
results_df <- data.frame(hpsc_scraped = unlist(hpsc_scraped))

#write into excel
write_xlsx(results_df, "/Users/amirabarizah/Documents/data-hp/data/ig/hpsc_process.xlsx")
write_xlsx(hpsc_filtered, "/Users/amirabarizah/Documents/data-hp/data/ig/hpsc-filtered.xlsx")


#parsing the extracted data
hpsc_extracted <- read_excel("/Users/amirabarizah/Documents/data-hp/data/ig/hpsc_extracted.xlsx")

# View the filtered rows
print(filtered_df)


