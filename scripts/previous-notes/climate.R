library(tidyverse)
climate_data <- read_csv("data/BOM_data.csv")
station_data <- read_csv("data/BOM_stations.csv")
climate_data
station_data

Q1 <- climate_data %>%
  separate(col = Temp_min_max, into = c("Min", "Max"), sep = "/") %>% 
  group_by(Station_number) %>% 
  filter(Min != "-" & Max != "-" & Rainfall != "-") %>% 
  summarise(Day = n())

Q2 <- climate_data %>%
  separate(col = Temp_min_max, into = c("Min", "Max"), sep = "/") %>% 
  group_by(Month) %>% 
  filter(Min != "-" & Max != "-" & Rainfall != "-") %>% 
  mutate(Min = as.numeric(Min), Max = as.numeric(Max)) %>% #replace a col e.g type or add a new col 
  mutate(Temp_diff = Max - Min) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% #thinking about the new heading for each new dataframe
  filter(Mean_temp_diff == min(Mean_temp_diff))

#*************Q3*********************************************************************** 
tidy_station_data <- station_data %>% 
  gather(id, content, -info) %>%
  spread(key = info, value = content) %>% 
  mutate(id = as.numeric(id))

station_with_lowest_mean_temp_diff <- climate_data %>%
  rename(id = Station_number) %>% 
  mutate(id = as.numeric(id)) %>% 
  separate(col = Temp_min_max, into = c("Min", "Max"), sep = "/") %>% 
  group_by(id) %>% 
  filter(Min != "-" & Max != "-" & Rainfall != "-") %>% 
  mutate(Min = as.numeric(Min), Max = as.numeric(Max)) %>% #replace a col e.g type or add a new col 
  mutate(Temp_diff = Max - Min) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% #thinking about the new heading for each new dataframe
  filter(Mean_temp_diff == min(Mean_temp_diff))

left_join(station_identification, tidy_station_data, by = "id")

#*************************Q4***********************************

solar_exposure <- climate_data %>%
  rename(id = Station_number) %>% 
  mutate(id = as.numeric(id)) %>% 
  separate(col = Temp_min_max, into = c("Min", "Max"), sep = "/") %>% 
  group_by(id) %>% 
  filter(Min != "-" & Max != "-" & Rainfall != "-" & Solar_exposure != "-") %>% 
  mutate(Min = as.numeric(Min), Max = as.numeric(Max), Solar_exposure = as.numeric(Solar_exposure)) %>% #replace a col e.g type or add a new col
  summarise(Solar_exposure = mean(Solar_exposure))

left_join(solar_exposure, tidy_station_data, by = "id") %>% 
  arrange(desc(Solar_exposure)) %>% 
  filter(lon == min(lon) | lon == max(lon))

#*************Q3*********************************************************************** 
tidy_station_data <- station_data %>% 
  gather(id, content, -info) %>%
  spread(key = info, value = content) %>% 
  mutate(id = as.numeric(id))

station_with_lowest_mean_temp_diff <- climate_data %>%
  rename(id = Station_number) %>% 
  mutate(id = as.numeric(id)) %>% 
  separate(col = Temp_min_max, into = c("Min", "Max"), sep = "/") %>% 
  group_by(id) %>% 
  filter(Min != "-" & Max != "-" & Rainfall != "-") %>% 
  mutate(Min = as.numeric(Min), Max = as.numeric(Max)) %>% #replace a col e.g type or add a new col 
  mutate(Temp_diff = Max - Min) %>% 
  summarise(Mean_temp_diff = mean(Temp_diff)) %>% #thinking about the new heading for each new dataframe
  filter(Mean_temp_diff == min(Mean_temp_diff))

left_join(station_identification, tidy_station_data, by = "id")
