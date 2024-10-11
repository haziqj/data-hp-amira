library(bruneimap)
library(dplyr)
library (tidyverse)
library(readxl)
library(writexl)
library(zoo)

data_hp <- read_excel("data/data-hp.xlsx")
bn_kpg_level_data <- read.csv("data/bn_kpg_level_data.csv")

kampong_bruhome <- bruhome_data$Kampung 
kampong <- unique(kpg_sf$kampong)
kampong_am <- unique(data_hp$kampong)

#identifying difference of kampong
identical(sort(kampong), sort(kampong_am))
diff_kampongs <- setdiff(kampong_bruhome, kampong)
diff_kampongs


#replacing "Kg. Berakas" by randomising 
berakas_list <- kpg_sf %>%
  filter(mukim == c("Mukim Berakas A", "Mukim Berakas B")) %>%
  pull (kampong)

berakas_indices <- which(data_hp$kampong == "Kg. Berakas")
set.seed(123)
data_hp$kampong[berakas_indices] <- sample(berakas_list, length(berakas_indices), replace = TRUE)
print(data_hp$kampong[berakas_indices])

#replacing "Kg. Sengkurong"
sengkurong_list <- c("Kg. Sengkurong A", "Kg. Sengkurong B")

sengkurong_indices <- which(data_hp$kampong == "Kg. Sengkurong")
set.seed(123)
data_hp$kampong[sengkurong_indices] <- sample(sengkurong_list, length(sengkurong_indices), replace = TRUE)
print(data_hp$kampong[sengkurong_indices])

#replacing "Kg. Lambak Kanan"
lk_list <- c("Perumahan Negara Lambak Kanan Kawasan 1", "Perumahan Negara Lambak Kanan Kawasan 2", "Perumahan Negara Lambak Kanan Kawasan 3", "Perumahan Negara Lambak Kanan Kawasan 4", "Perumahan Negara Lambak Kanan Kawasan 5")
lk_indices <- which(data_hp$kampong == "Kg. Lambak Kanan")
set.seed(123)
data_hp$kampong[lk_indices] <- sample(lk_list, length(lk_indices), replace = TRUE)

#replacing the rest
data_hp <- data_hp %>%
  mutate(kampong = if_else(kampong == "Kg, Jerudong", "Kg. Jerudong", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg, Kiulap", "Kg. Kiulap", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg. Pingai Berakas", "Kg. Burong Pingai Berakas", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg. Subok", "Kg. Subok Kota Batu", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg. Tanjung Bunut", "Kg. Tanjong Bunut", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg. Jerudng", "Kg. Jerudong", kampong)) %>%
  mutate(kampong = if_else(kampong == "Kg. Sungai Tampoi", "Kg. Kilanas", kampong))

#replacing "Kg. Muara"
muara_list <- c("Kg. Kapok","Kg. Sabun","Kg. Serasa", "Pekan Muara")

muara_indices <- which(data_hp$kampong == "Kg. Muara")
set.seed(123)
data_hp$kampong[muara_indices] <- sample(muara_list, length(muara_indices), replace = TRUE)
print(data_hp$kampong[muara_indices])

#replacing Kg. Lambak
lambak_list <- c("Perumahan Negara Lambak Kanan Kawasan 1", "Perumahan Negara Lambak Kanan Kawasan 2", "Perumahan Negara Lambak Kanan Kawasan 3", "Perumahan Negara Lambak Kanan Kawasan 4", 
                 "Perumahan Negara Lambak Kanan Kawasan 5", "Kg. Lambak A", "Kg. Lambak B", "Kg. Lambak Kiri","STKRJ Lambak Kiri")
lambak_indices <- which(data_hp$kampong == "Kg. Lambak")
set.seed(123)
data_hp$kampong[lambak_indices] <- sample(lambak_list, length(lambak_indices), replace = TRUE)

#identifying difference of mukim
mukim <- unique(kpg_sf$mukim)
mukim_am <- unique(data_hp$mukim)

identical(sort(mukim), sort(mukim_am))
diff_mukim <- setdiff(mukim_am, mukim)
diff_mukim

#replacing Mukim Gadong with Mukim Gadong B
data_hp <- data_hp %>%
  mutate(mukim = if_else(mukim == "Mukim Gadong", "Mukim Gadong B", mukim)) 
  
#replacing Mukim Berakas with randomised A/B
mb_list <- c("Mukim Berakas A", "Mukim Berakas B")
mb_indices <- which(data_hp$mukim == "Mukim Berakas")
set.seed(123)
data_hp$mukim[mb_indices] <- sample(mb_list, length(mb_indices), replace = TRUE)

#checking for mukim from the 'NA' under kampong
mukim_for_na <- data_hp %>%
  filter(is.na(kampong)) %>%
  select(mukim)

unique(mukim_for_na)

#identifying which rows of Mukim Gadong A that has NA under kampong
gadongA_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Gadong A")
gadongA_indices

#extracting the kampong under Mukim Gadong A
gadongA_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Gadong A") %>%
  pull(kampong)

#sampling for kampong with NA under Mukim Gadong A
data_hp$kampong[gadongA_indices] <- sample(gadongA_list, length(gadongA_indices), replace = TRUE)

#identifying which rows of Mukim Gadong B that has NA under kampong
gadongB_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Gadong B")
gadongB_indices

#extracting the kampong under Mukim Gadong B
gadongB_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Gadong B") %>%
  pull(kampong)

#sampling for kampong with NA under Mukim Gadong B
data_hp$kampong[gadongB_indices] <- sample(gadongB_list, length(gadongB_indices), replace = TRUE)

#sampling for - Mukim Berakas A
berakasA_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Berakas A")
berakasA_indices

berakasA_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Berakas A") %>%
  pull(kampong)

data_hp$kampong[berakasA_indices] <- sample(berakasA_list, length(berakasA_indices), replace = TRUE)

#sampling for - Mukim Berakas B
berakasB_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Berakas B")
berakasB_indices

berakasB_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Berakas B") %>%
  pull(kampong)

data_hp$kampong[berakasB_indices] <- sample(berakasB_list, length(berakasB_indices), replace = TRUE)

#sampling for - Mukim Kota Batu
kotabatu_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Kota Batu")
kotabatu_indices

kotabatu_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Kota Batu") %>%
  pull(kampong)

data_hp$kampong[kotabatu_indices] <- sample(kotabatu_list, length(kotabatu_indices), replace = TRUE)

#Sampling for Mukim Sengkurong
sengkurong_indices <- which(is.na(data_hp$kampong) & data_hp$mukim == "Mukim Sengkurong")
sengkurong_indices

sengkurong_list <- bn_kpg_level_data %>%
  filter(mukim == "Mukim Sengkurong") %>%
  pull(kampong)

data_hp$kampong[sengkurong_indices] <- sample(sengkurong_list, length(sengkurong_indices), replace = TRUE)

#leftjoin with kpg_sf
data_hp <-
  data_hp |>
  rename(mukim_old = mukim) |>
  left_join(
    kpg_sf |> 
      sf::st_set_geometry(NULL) |>
      select(kampong, mukim)
  ) |>
  select(month, quarter, mukim, kampong:`Source(s)`)

#transfer cleaned version
write_xlsx(data_hp, path = "data/data-hp-cl.xlsx")

