library(tidyverse)
library(stringr)
library(dplyr)

#----PARSING BRUHOME--------------------------------------------------------------------
#sqft & acre ✅
bruhome_data <- bruhome_data |>
  #remove desc with <= 5 words
  mutate(Description = ifelse(str_count(Description, "\\w+") <= 5, "", Description)) |>
  
  #parsing the built-up area
  mutate(
    BuiltUpAreaExtract = str_extract(Description, "\\d+[,.]?\\d*\\s*(sqft|SQ|Sq|SQFT|sq ft)")
  ) |>
  
  #extract only the numeric part and convert to numeric type
  mutate(
    BuiltUpAreaExtract = str_extract(BuiltUpAreaExtract, "\\d+[,.]?\\d*") %>%
      as.numeric()  
  ) |>
  
  #parsing the land size
  mutate(
    LandSizeExtract = str_extract(Description, "\\d+[,.]?\\d*\\s*(AC|ac|acre|Acre)")
  )

#landsize

bruhome_data <- bruhome_data %>%
  mutate(
    LandSizeExtract = str_extract(Description, "\\d+[,.]?\\s?\\d*\\s*(AC|ac|acre|Acre)")
  ) %>%
  mutate(
    LandSizeExtract = str_extract(LandSizeExtract, "\\d+[,.]?\\s?\\d*") %>%
      str_replace_all("\\s", "") %>% 
      as.numeric()
  )



#property type ✅
#using agrep to extract the keywords even with typos
fuzzy_extract <- function(description, keywords) {
  matched_word <- NA
  for (keyword in keywords) {
    if (length(agrep(keyword, description, max.distance = 0.1, ignore.case = TRUE)) > 0) {
      matched_word <- keyword
      break
    }
  }
  return(matched_word)
}

property_keywords <- c("bungalow","apartment", "appartment", "terrace", "terace", "semi-detached", "detached")

bruhome_data <- bruhome_data %>%
  mutate(
    PropertyTypeExtract = sapply(Description, fuzzy_extract, keywords = property_keywords)
  )


bruhome_data <- bruhome_data %>%
  mutate(
    KampongExtract = sapply(Kampung, fuzzy_extract, keywords = kampong)
  )

#storey ✅
bruhome_data <- bruhome_data %>%
  mutate(StoreyCount = str_extract(Description, "(\\d+\\.?\\d*|\\d+\\s*\\d+/\\d+|Double|Single|Triple|Quadruple|Three|Four|Five|Six|Seven|Eight|Nine|Ten|DOUBLE|SINGLE|THREE|FOUR|FIVE|SIX|TRIPLE|QUADRUPLE|SEVEN|EIGHT|NINE|TEN)\\s*-?\\s*[Ss][Tt][Oo][Rr][Ee][Yy]"))  

#property status ✅
status_keywords <- c("complete","proposed", "ready", "approved", "under-construction")

bruhome_data <- bruhome_data %>%
  mutate(
    StatusExtract = sapply(Description, fuzzy_extract, keywords = status_keywords)
  )

#bedroom & bathroom ✅

#extracting the common format i.e. "...R/...T" or "..R ..T"
bruhome_data <- bruhome_data %>%
  mutate(
    CombinedNumbers = str_extract_all(Description, "(\\d+)\\s*[RrTt]|(\\d+)\\s*/\\s*[RrTt]") %>% 
      sapply(paste, collapse = ", ")  
  )

#removing anomalies
bruhome_data <- bruhome_data %>%
  mutate(
    CombinedNumbers = str_remove_all(CombinedNumbers, "\\n|00|00 \n|8938588|6738938588|74|0733|111|09|132|0566|0653|2018|0652|65|15|67|2200")  # Remove unwanted patterns
  )

#extracting Bedroom
bruhome_data <- bruhome_data %>%
  mutate(
    Bedroom = str_extract(CombinedNumbers, "(\\d+)\\s*[Rr]") %>%
      str_remove("[Rr]")
  )

unique(bruhome_data$Bedroom)

#extracting Bathroom
bruhome_data <- bruhome_data %>%
  mutate(
    Bathroom = str_extract(CombinedNumbers, "(\\d+)\\s*[Tt]") %>%
      str_remove("[Tt]")  
  )

unique(bruhome_data$Bathroom)

#extracting the coefficient of "... Bedroom/bedroom/BEDROOM"
bruhome_data <- bruhome_data %>%
  mutate(
    BedroomsExtracted = str_extract(Description, "(\\d+)\\s*(?=\\s*[Bb]edroom[s]?)")  # Extract number before "Bedroom" variants
  )

bruhome_data <- bruhome_data %>%
  mutate(
    BathroomsExtracted = str_extract(Description, "(\\d+)\\s*(?=\\s*[Bb]athroom[s]?)")  # Extract number before "Bedroom" variants
  )

#using coalesce to create the ultimate column for bd and bt
bruhome_data <- bruhome_data %>%
  mutate(
    CombinedBedrooms = coalesce(Bedroom, BedroomsExtracted)
  )

unique(bruhome_data$CombinedBedrooms)

bruhome_data <- bruhome_data %>%
  mutate(
    CombinedBathrooms = coalesce(Bathroom, BathroomsExtracted)
  )

unique(bruhome_data$CombinedBathrooms)

#tidying up
bruhome_data <- bruhome_data %>%
  select(-CombinedNumbers, -BedroomsExtracted, -BathroomsExtracted, -Bedroom, -Bathroom)

#land type

landtype_keywords <- c("In perpetuity", "Leasehold", "Lease", "Kekal")

bruhome_data <- bruhome_data %>%
  mutate(
    LandType = sapply(Description, fuzzy_extract, keywords = landtype_keywords)
  )


#----CLEANING BRUHOME--------------------------------------------------------------------

#storey ✅
#property type ✅
#landtype ✅
#bedrooms & bathrooms ✅
#status ✅

#note: further cleaned up in excel

bruhome_data <- read_excel("/Users/amirabarizah/Documents/data-hp/bruhome-cl.xlsx")

unique(bruhome_data$LandType)

bruhome_data <- bruhome_data %>%
  mutate(
    Storey = str_replace_all(Storey, regex("\\bOne\\b|\\bSingle\\b", ignore_case = TRUE), "1"),
    Storey = str_replace_all(Storey, regex("\\bTwo\\b", ignore_case = TRUE), "2"),
    Storey = str_replace_all(Storey, regex("\\bThree\\b", ignore_case = TRUE), "3"),
    Storey = str_replace_all(Storey, regex("\\bFour\\b", ignore_case = TRUE), "4"),
    Storey = str_replace_all(Storey, regex("\\bFive\\b", ignore_case = TRUE), "5"),
    Storey = str_replace_all(Storey, regex("\\bSeven\\b", ignore_case = TRUE), "7"),
    Storey = str_replace_all(Storey, regex("\\bTwelve\\b", ignore_case = TRUE), "12"),
    Storey = str_replace_all(Storey, regex("\\bDouble\\b", ignore_case = TRUE), "2"),
    Storey = str_replace_all(Storey, regex("\\bDOUBLE\\b", ignore_case = TRUE), "2"),
    
    
    
    Storey = str_replace_all(Storey, regex("1 1/2|1\\.5", ignore_case = TRUE), "1.5"),
    Storey = str_replace_all(Storey, regex("2 1/2|2\\.5", ignore_case = TRUE), "2.5"),
    Storey = str_replace_all(Storey, regex("3 1/2|3\\.5", ignore_case = TRUE), "3.5"),
    
    Storey = str_replace_all(Storey, regex("-|\\s+", ignore_case = TRUE), " "),
    
    Storey = str_replace_all(Storey, regex("\\bstorey\\b", ignore_case = TRUE), ""),
    
    Storey = trimws(Storey)
  )

# View the cleaned data
unique(bruhome_data$Storey)

#removing anomalies (double checked)
bruhome_data <- bruhome_data %>%
  mutate(
    Storey = str_remove_all(Storey, "12|7") 
  )

#land type 
unique(bruhome_data$LandType)
bruhome_data <- bruhome_data %>%
  mutate(
    LandType = str_replace_all(LandType, "Lease", "Leasehold"),
    LandType = str_replace_all(LandType, "Kekal", "In perpetuity") 
    
  )

#bedrooms
unique(bruhome_data$Bedrooms)
unique(bruhome_data$Bathrooms)

bruhome_data <- bruhome_data %>%
  mutate(
    Bathrooms = str_replace_all(Bathrooms, "1  \r\n", "1"),
  
  )

#kampung

kampong

bruhome_data <- bruhome_data %>%
  mutate(
    Kampung = paste0("Kg. ", Kampung)
  )

write_xlsx(bruhome_data, "/Users/amirabarizah/Documents/data-hp/data/bruhome/bruhome.xlsx")

#----HPSC-----------------------------------------------------------------------
hpsc_bp <- hpsc

#storey ✅
#property type ✅
#landtype ✅
#bedrooms & bathrooms ✅
#status ✅
#sqft & acre ✅
#land type

landtype_keywords <- c("In perpetuity", "Leasehold", "Kekal")

hpsc <- hpsc %>%
  mutate(
    LandType = sapply(caption, fuzzy_extract, keywords = landtype_keywords)
  )


#sqft
hpsc <- hpsc |>
  #remove desc with <= 5 words
  mutate(caption = ifelse(str_count(caption, "\\w+") <= 5, "", caption)) |>
  
  #parsing the built-up area
  mutate(
    BuiltUpAreaExtract = str_extract(caption, "\\d+[,.]?\\d*\\s*(sqft|SQ|Sq|SQFT|sq ft)")
  ) |>
  
  #extract only the numeric part and convert to numeric type
  mutate(
    BuiltUpAreaExtract = str_extract(BuiltUpAreaExtract, "\\d+[,.]?\\d*") %>%
      as.numeric()  
  ) 

#acre
hpsc <- hpsc %>%
  mutate(
    LandSizeExtract = str_extract(caption, "\\d+[,.]?\\s?\\d*\\s*(?=\\s*(acre|ac))") %>%  
      str_replace_all("\\s", "") %>%  
      as.numeric()  
  )


#property type ✅
#using agrep to extract the keywords even with typos
fuzzy_extract <- function(description, keywords) {
  matched_word <- NA
  for (keyword in keywords) {
    if (length(agrep(keyword, description, max.distance = 0.1, ignore.case = TRUE)) > 0) {
      matched_word <- keyword
      break
    }
  }
  return(matched_word)
}

property_keywords <- c("bungalow","apartment", "appartment", "terrace", "terace", "semi-detached", "detached")

hpsc <- hpsc %>%
  mutate(
    PropertyTypeExtract = sapply(caption, fuzzy_extract, keywords = property_keywords)
  )

#storey ✅
hpsc <- hpsc %>%
  mutate(StoreyCount = str_extract(caption, "(\\d+\\.?\\d*|\\d+\\s*\\d+/\\d+|double|Double|Single|Triple|Quadruple|Three|Four|Five|Six|Seven|Eight|Nine|Ten|DOUBLE|SINGLE|THREE|FOUR|FIVE|SIX|TRIPLE|QUADRUPLE|SEVEN|EIGHT|NINE|TEN)\\s*-?\\s*[Ss][Tt][Oo][Rr][Ee][Yy]"))  

#price 
hpsc <- hpsc %>%
  mutate(PriceExtracted = str_extract(caption, 
                                      "\\b(\\d+k|\\d{1,3}(,\\d{3})|\\d{1,3}+xxx|\\d{1,3}x\\s+\\w+|\\d{6}|\\d{5})\\b"))
hpsc <- hpsc %>%
  drop_na(PriceExtracted)

#property status ✅
status_keywords <- c("complete","proposed", "ready", "approved", "under-construction")

hpsc <- hpsc %>%
  mutate(
    StatusExtract = sapply(caption, fuzzy_extract, keywords = status_keywords)
  )



#bedroom & bathroom ✅

#extracting the common format i.e. "...R/...T" or "..R ..T"
hpsc <- hpsc %>%
  mutate(
    CombinedNumbers = str_extract_all(caption, "([1-9]|10)[RrTt]") %>%
      sapply(paste, collapse = ", ")
  )

unique(hpsc$CombinedNumbers)

#extracting No. of Bedroom
hpsc <- hpsc %>%
  mutate(
    BedroomExtracted = str_extract(CombinedNumbers, "(\\d+)\\s*[Rr]") %>%
      str_remove("[Rr]")
  )

unique(hpsc$Bedroom)

#extracting No. of Bathroom
hpsc <- hpsc %>%
  mutate(
    BathroomExtracted = str_extract(CombinedNumbers, "(\\d+)\\s*[Tt]") %>%
      str_remove("[Tt]")  
  )

unique(hpsc$BathroomExtracted)

#extracting the coefficient of "... Bedroom/bedroom/BEDROOM"
hpsc <- hpsc %>%
  mutate(
    Bedrooms = str_extract(caption, "(\\d+)\\s*(?=\\s*[Bb]edroom[s]?)")  # Extract number before "Bedroom" variants
  )

hpsc <- hpsc %>%
  mutate(
    Bathrooms = str_extract(caption, "(\\d+)\\s*(?=\\s*[Bb]athroom[s]?)")  # Extract number before "Bedroom" variants
  )

#using coalesce to create the ultimate column for bd and bt
hpsc <- hpsc %>%
  mutate(
    CombinedBedrooms = coalesce(Bedrooms, BedroomsExtracted)
  )

unique(hpsc$CombinedBedrooms)

hpsc <- hpsc %>%
  mutate(
    CombinedBathrooms = coalesce(Bathrooms, BathroomsExtracted)
  )

unique(hpsc$CombinedBathrooms)

#tidy up
hpsc <- hpsc %>%
  mutate(
    CombinedBedrooms = str_trim(CombinedBedrooms),  # Step 1: Trim whitespace
    CombinedBedrooms = as.numeric(CombinedBedrooms) # Step 2: Convert to numeric
  )

hpsc <- hpsc %>%
  mutate(
    CombinedBathrooms = str_trim(CombinedBathrooms),  # Step 1: Trim whitespace
    CombinedBathrooms = as.numeric(CombinedBathrooms) # Step 2: Convert to numeric
  )

hpsc <- hpsc %>%
  select(-CombinedNumbers, -Bathrooms, -BathroomsExtracted, -Bathroom, -BathroomExtracted, -Bedroom, -BedroomsExtracted, -Bedrooms, -BedroomExtracted)

write_xlsx(hpsc, "/Users/amirabarizah/Documents/data-hp/hpsc_data.xlsx")

#kampung -trial*****


#try ollama

#----HSP------------------------------------------------------------------------

hsp_data <- hsp_filtered

#sqft

hsp_data <- hsp_data |>
  mutate(desc = ifelse(str_count(desc, "\\w+") <= 5, "", desc)) |>
  
  mutate(
    Sqft = str_extract(desc, "\\d{1,4}(?:,\\d{3})*(?:\\.\\d{0,2})?\\s*[+]*\\s*(sqft|sq ft|sq|ft|SQFT|sf|Sq Ft)")
  ) |>
  
  mutate(
    Sqft = gsub(",", "", Sqft),  
    Sqft = gsub("[+]*", "", Sqft),  
    Sqft = as.numeric(str_extract(Sqft, "\\d+(?:\\.\\d{0,2})?")) 
  )


#acre
hsp_data <- hsp_data %>%
  mutate(
    LandSizeExtract = str_extract(desc, "\\d+[,.]?\\s?\\d*\\s*(?=\\s*(acre|ac|ACRE|Acre))") %>%  # Corrected lookahead
      str_replace_all("\\s", "") %>%  # Remove any whitespace
      as.numeric()  # Convert to numeric
  )


#property type ✅
#using agrep to extract the keywords even with typos
fuzzy_extract <- function(description, keywords) {
  matched_word <- NA
  for (keyword in keywords) {
    if (length(agrep(keyword, description, max.distance = 0.1, ignore.case = TRUE)) > 0) {
      matched_word <- keyword
      break
    }
  }
  return(matched_word)
}

property_keywords <- c("bungalow","apartment", "appartment", "terrace", "terace", "semi-detached", "detached")

hsp_data <- hsp_data %>%
  mutate(
    PropertyTypeExtract = sapply(desc, fuzzy_extract, keywords = property_keywords)
  )

#storey ✅
hsp_data <- hsp_data %>%
  mutate(StoreyCount = str_extract(desc, "(\\d+\\.?\\d*|\\d+\\s*\\d+/\\d+|double|Double|Single|Triple|Quadruple|Three|Four|Five|Six|Seven|Eight|Nine|Ten|DOUBLE|SINGLE|THREE|FOUR|FIVE|SIX|TRIPLE|QUADRUPLE|SEVEN|EIGHT|NINE|TEN)\\s*-?\\s*[Ss][Tt][Oo][Rr][Ee][Yy]"))  

#price 
hsp_data <- hsp_data %>%
  mutate(PriceExtracted = str_extract(desc, 
                                      "\\b(\\d+k|\\d{1,3}(,\\d{3})|\\d{1,3}+xxx|\\d{1,3}x\\s+\\w+|\\d{6}|\\d{5})\\b"))
hsp_data <- hsp_data %>%
  drop_na(PriceExtracted)

#property status ✅
status_keywords <- c("complete","proposed", "ready", "approved", "under-construction")

hsp_data <- hsp_data%>%
  mutate(
    StatusExtract = sapply(desc, fuzzy_extract, keywords = status_keywords)
  )

#bedroom & bathroom ✅

#extracting the common format i.e. "...R/...T" or "..R ..T"
hsp_data <- hsp_data %>%
  mutate(
    CombinedNumbers = str_extract_all(desc, "([1-9]|10)[RrTt]|([1-9]|10)[BRTB]") %>%
      sapply(paste, collapse = ", ")
  )

unique(hsp_data$CombinedNumbers)

#extracting No. of Bedroom
hsp_data <- hsp_data %>%
  mutate(
    BedroomExtracted = str_extract(CombinedNumbers, "(\\d+)\\s*[Rr]|(\\d+)\\s*[B]") %>%
      str_remove("[Rr]|[B]")
  )

unique(hsp_data$BedroomExtracted)

#extracting No. of Bathroom
hsp_data <- hsp_data %>%
  mutate(
    BathroomExtracted = str_extract(CombinedNumbers, "(\\d+)\\s*[Tt]") %>%
      str_remove("[Tt]")  
  )

unique(hsp_data$BathroomExtracted)

#extracting the coefficient of "... Bedroom/bedroom/BEDROOM"
hsp_data <- hsp_data %>%
  mutate(
    Bedrooms = str_extract(desc, "(\\d+)\\s*(?=\\s*[Bb]edroom[s]?)")  # Extract number before "Bedroom" variants
  )

hsp_data <- hsp_data %>%
  mutate(
    Bathrooms = str_extract(desc, "(\\d+)\\s*(?=\\s*[Bb]athroom[s]?)")  # Extract number before "Bedroom" variants
  )

#using coalesce to create the ultimate column for bd and bt
hsp_data <- hsp_data %>%
  mutate(
    CombinedBedrooms = coalesce(Bedrooms, BedroomExtracted)
  )

unique(hsp_data$CombinedBedrooms)

hsp_data <- hsp_data %>%
  mutate(
    CombinedBathrooms = coalesce(Bathrooms, BathroomExtracted)
  )

unique(hpsc$CombinedBathrooms)

#tidy up
hsp_data <- hsp_data |>
  mutate(CombinedBedrooms = trimws(CombinedBedrooms)) |> # Remove leading/trailing whitespaces
  mutate(CombinedBedrooms = ifelse(CombinedBedrooms %in% c("00", "00 \n", "00 \n "), NA, CombinedBedrooms)) |> # Replace '00' with NA
  mutate(CombinedBedrooms = as.numeric(CombinedBedrooms)) # Convert to numeric

hsp_data <- hsp_data |>
  select(-BathroomExtracted, -Bathrooms, -Bedrooms, -BedroomExtracted, -CombinedNumbers)

landtype_keywords <- c("In perpetuity", "Leasehold", "Kekal")

hsp_data <- hsp_data %>%
  mutate(
    LandType = sapply(desc, fuzzy_extract, keywords = landtype_keywords)
  )

write_xlsx(hsp_data, "/Users/amirabarizah/Documents/data-hp/data/hsp_data.xlsx")
