library(readxl)
install.packages("writexl")
library(writexl)
library(ollamar)
test_connection()
pull("llama3.1")


captions <- read.csv("/Users/amirabarizah/Documents/data-hp/data/hpsc_10.csv")

process_caption <- function(caption) {
  caption_clean <- gsub('"','', caption)
  
  prompt <- paste("Extract the following details from this caption: 
        - kampong: Should be a village in Brunei Darussalam.
        - price: Identify the price details.
        - type: Categorize into one of the following: apartment, bungalow, detached, semi-detached.
        - storey: Mention if it is a single/double storey.
        - status: new, proposed, or under-construction.
        - land_size: Extract the land size.
        - floor_size: Extract the floor size.
        - beds: Number of bedrooms.
        - baths: Number of bathrooms.
        - land_type: Leasehold or In perpetuity.
        - Additional Remark: Any remaining details.
        Caption: '", caption_clean, "'")
  
  result <- generate("llama3.1", prompt, output = "text")
  
  return(result)
  
}

captions$processed_caption <- sapply(captions$caption, process_caption)

write.csv (captions, "/Users/amirabarizah/Documents/data-hp/data/processed_captions.csv")


