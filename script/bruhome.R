install.packages("rvest")
library(rvest)
library(tidyverse)
library(httr)
library(dplyr)
library(writexl)
library()

start_id <- 2171
end_id <- 18805

urls <- sprintf("https://www.bruhome.com/v3/propertydetail.asp?p_=buy&id=%d&req=", start_id:end_id)

results <- data.frame(
  URL = character(),
  Location = character(),
  Price = character(),
  ContactInformation = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(urls)) {
  url <- urls[i]
  
  #progress tracking
  cat(sprintf("Processing URL %d of %d: %s\n", i, length(urls), url))
  
  #extracting page content
  page <- tryCatch(read_html(GET(url)), error = function(e) NA)
  
  if (!is.na(page)) {
    #extract the kampong and district
    location <- tryCatch(
      page %>%
        html_node('span.d-block') %>%
        html_text(trim = TRUE),
      error = function(e) NA
    )
    
    #extract price
    price <- tryCatch(
      page %>%
        html_node('span.price.font-xll.text-primary.mr-2') %>%
        html_text(trim = TRUE),
      error = function(e) NA
    )
    
    #extract contact information
    contact_info <- tryCatch(
      page %>%
        html_node('div.col-sm-9 > p') %>%
        html_text(trim = TRUE),
      error = function(e) NA
    )
    
    #transfer data
    results <- results %>%
      add_row(
        URL = url,
        Location = ifelse(is.na(location), "Not Available", location),
        Price = ifelse(is.na(price), "Not Available", price),
        ContactInformation = ifelse(is.na(contact_info), "Not Available", contact_info)
      )
  } else {
    #alternative to possibly
    results <- results %>%
      add_row(
        URL = url,
        Location = "Failed to load page",
        Price = "Failed to load page",
        ContactInformation = "Failed to load page"
      )
  }
}

#transfering the results to bruhome_data
bruhome_data <- results
write_xlsx(results, "/Users/amirabarizah/Documents/data-hp/data/raw/bruhome-raw.xlsx")

#creating a backup
bruhome_data_bp <- bruhome_data

#removing the failed pages
bruhome_data <- bruhome_data %>%
  filter(
    Location != "Failed to load page" & Location != "Not Available",
    Price != "Failed to load page" & Price != "Not Available"
  )

#cleaning/removing prices for rent
bruhome_data <- bruhome_data %>%
  mutate(Price = gsub("BND", "", Price),         
         Price = gsub(",", "", Price)) %>%       
  mutate(Price = as.numeric(Price)) %>%          
  filter(!is.na(Price) & Price >= 10000) %>%    
  rename(Description = ContactInformation) %>%
  separate(Location, into = c("Kampung", "District"), sep = ",\\s*")

#filtering from 'land for sale' and NA kampung
bruhome_data <- bruhome_data %>%
  mutate(Description = as.character(Description)) %>%
  filter(
    !str_detect(Description, regex("land for sale", ignore_case = TRUE)),  # Omit rows with "land for sale"
    !str_detect(Kampung, regex("not specified", ignore_case = TRUE))       # Remove rows where "kampung" is "not specified"
  )

write_xlsx(bruhome_data, "/Users/amirabarizah/Documents/data-hp/data/bruhome/bruhome-filtered.xlsx")
