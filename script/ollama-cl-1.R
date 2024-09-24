install.packages("ollamar")
library(ollamar)
library(dplyr)
library(readxl)
library(writexl)

#testing connection & ensure the model is pulled
test_connection()
pull("llama3.1")

#vector of captions
hpsc <- read.csv("/Users/amirabarizah/Documents/data-hp/data/houseprice_scraped.csv")
hpsc_captions <- hpsc$caption  

#create a function to process the rest of the captions
process_caption <- function(caption) {
  caption_clean <- gsub('"', '', caption)
  
  prompt <- paste0(
    "Extract the following details from this: 
    - kampong: Identify the kampong (village in Brunei) mentioned. 
    - price: Identify the price details.
    - type: Identify the words - apartment, bungalow, detached, semi-detached.
    - storey: Identify if it is a single storey or a double storey.
    - status: Identify whther it is new, proposed, or under-construction.
    - land_size: Extract the land size.
    - floor_size: Extract the floor size.
    - beds: Number of bedrooms.
    - baths: Number of bathrooms.
    - land_type: Identify if it Leasehold or In perpetuity or Kekal
    - Additional Remark: The remaining details.
    Caption: '", caption_clean, "'"
  )
  
  # Run the Ollama model on the prompt
  result <- generate("llama3.1", prompt, output = "text")
  
  return(result)
}

#loop    
for (i in 1001:min(length(hpsc_captions), 1100)) {
  results[[i]] <- process_caption(hpsc_captions[i])
}       
                  
# Convert the list of results to a dataframe
results_df <- data.frame(results = unlist(results))
write.csv(results_df, "/Users/amirabarizah/Documents/data-hp/data/results200.csv")
write.csv(results_df, "/Users/amirabarizah/Documents/data-hp/data/results.csv")
write.csv(results_df, "/Users/amirabarizah/Documents/data-hp/data/trial_results.csv")



                  