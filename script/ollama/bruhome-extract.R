install.packages("ollamar")
library(ollamar)
library(dplyr)
library(readxl)
library(tidyverse)
library(writexl)

?generate
#test connection - ollama
test_connection()
pull("llama3.1")

#using ollama to extract property characteristics from Description
bruhome_description <- bruhome_data$Description

#function to extract characteristics from the Description
clean_description <- function(desc) {
  tryCatch({
    desc_clean <- gsub('"', '', desc)
    
    if (nchar(desc_clean) == 0) {
      return(NA)
    }

    prompt <- paste0(
      "Extract the following information from a real estate description in Brunei:\n",
      "1. Storey of the property: [Double Storey = 2, Single Storey or Bungalow = 1]\n",
      "2. Status of the property: [One of Complete, Under-construction, Proposed] [CHARACTER]\n",
      "3. Built-up area: [NUMERIC, in square feet]\n",
      "4. Land size: [NUMERIC, in acres]\n",
      "5. Types of property: [Residential: [detached or semi-detached or terrace or apartment, other] or Commercial/land: [Return 'commercial' or 'land' if the property is not a residential building]] [CHARACTER]\n",
      "6. Number of bedrooms: [NUMERIC]\n",
      "7. Number of bathrooms: [NUMERIC]\n\n",
      "Output Format:\n",
      "- Return values as semicolon-separated\n",
      "- If information is missing or not applicable, use NA\n",
      "- PROVIDE NO OTHER EXTRA INFORMATION other than the specified values provided\n\n",
      "ONLY RETURN THE RESULT AS IN THIS FORMAT:\n",
      "1; under-construction; 1500; 0.14; semi-detached; 2; 3\n",
      "2; completed; 1800; 0.18; detached; 2; 2\n",
      "NA; NA; NA; NA; commercial; NA; NA\n\n",
      "Special Instructions:\n",
      "If the listing is for land or commercial properties, use NA for the built-up area and set the type to 'land' or 'commercial'.\n",
      "IF I DONT PROVIDE THE DESCRIPTION, JUST SAY NA!!\n",
      "----------\n\n",
      "Caption: '", desc_clean, "'"
    )
    
    result <- generate("llama3.1", prompt, output = "text", temperature = 0.1)
    return(result)
    
  }, error = function(e) {
    return(NA)
  })
}


#bruhome_extracted contains AI extracted characteristics in
Description <- map(
  .x = bruhome_description,
  .f = possibly(clean_description, NA),
  .progress = TRUE
)

#----PARSING THE DATA-----
parse_pattern <- function(desc) {
  pattern <- "(\\d);\\s*(\\w+);\\s*(\\d+);\\s*([0-9.]+);\\s*([^;]+);\\s*(\\d);\\s*(\\d)"
  matches <- str_match(desc, pattern)
  
  if (!is.na(matches[1])) {
    return(matches[-1]) 
  } else {
    return(rep(NA, 7))  #if none, return NA
  }
}

bruhome_parsed <- lapply(bruhomedesc_df$Description, parse_pattern)

#row binding
bruhome_df <- do.call(rbind, bruhome_parsed)
colnames(bruhome_df) <- c("Storey", "Status", "Built-up Area", "Land Size", "Property Type", "Bedrooms", "Bathrooms")
bruhome_df <- as.data.frame(bruhome_df, stringsAsFactors = FALSE)

bruhome_df$`Built-up Area`<- as.numeric(bruhome_df$`Built-up Area`)
bruhome_df$`Land Size` <- as.numeric(bruhome_df$`Land Size`)
bruhome_df$Bedrooms <- as.numeric(bruhome_df$Bedrooms)
bruhome_df$Bathrooms <- as.numeric(bruhome_df$Bathrooms)

bruhome <- cbind(bruhome_data, bruhome_df)

#cleaning bruhome data
unique(bruhome$Status) #✅
unique(bruhome$`Built-up Area`)
288000
22500
265000

#directing some content to the right column - correcting typos, etc.
bruhome <- bruhome %>%
  mutate(
    `Land Type` = case_when(
      str_to_lower(Status) %in% c("kekal", "kекal", "ketal", "kkal", "kekil") ~ "In perpetuity",
      str_to_lower(Status) %in% c("lease", "leased", "leasehold") ~ "Leasehold",
      TRUE ~ NA_character_  # Default case
    ),
    Status = case_when(
      str_to_lower(Status) %in% c("kekal", "kекal", "ketal", "kkal", "kekil", "lease", "leased", "leasehold") ~ NA_character_,
      TRUE ~ Status  # Keep the original Status if no conditions are met
    )
  )


bruhome <- bruhome %>%
  mutate(
    Status = case_when(
      Status %in% c("Ready", "complete", "completed", "Completed", "new") ~ "Complete",
      Status == "used" ~ "Used",
      Status == "approved" ~ "Approved",
      Status == "proposed" ~ "Proposed",
      TRUE ~ Status  # Keep the original value if no conditions are met
    ),
    Status = na_if(Status, "NA")  # Convert the string "NA" to actual NA
  ) %>%
  mutate(Status = ifelse(is.na(Status) | Status == "NA", NA, Status))  # Remove any NA values

bruhome <- bruhome %>%
  mutate(
    `Property Type` = ifelse(str_to_lower(Status) == "terrace", "terrace", `Property Type`),
    Status = ifelse(str_to_lower(Status) == "terrace", NA, Status)
  )

write_excel(bruhome, )



