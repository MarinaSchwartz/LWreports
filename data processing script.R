
#this script is for the purpose of making a dataset that can be looped over easily. 

library(tidyverse)
library(readxl)

lake = read_csv("All_Data.csv")

lake = lake %>%
  mutate(Lake_County = paste(Lake, County))

lake_2 = read_xlsx("Lakewatch Base File 10-24-2023.xlsx")


lake_2 <- lake_2 %>%
  mutate(Lake_County = paste(Lake, County)) %>% filter(Study == "LW") %>% filter(`water type` == "Lake")


write.csv(lake, "all_data_lake_county.csv")

write.csv(lake_2, "lakewatch_lake_county_LW_lake only.csv")


##for loop test data

select_data = list("Alice Alachua", "Alto Alachua")

for_loop_all_data = lake %>% filter(Lake_County %in% select_data)

write.csv(for_loop_all_data, "for_loop_all_data.csv")


