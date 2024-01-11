### Set WD ###
path <- "C:/Users/amber.riner/Documents/LWreports"
setwd(path)


### Import Libraries ###
library(readxl)
library(tidyverse)


### Loading and Preparing Data
data_1 = read_xlsx("Lakewatch Base File 10-24-2023.xlsx")
data_1 <- data_1 %>%
  mutate(Lake_County = paste(Lake, County)) %>%
  filter(Study == "LW") %>%
  filter(`water type` == "Lake") 
head(data_1)

data_2 = read.csv("All_Data.csv")
data_2 = data_2 %>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County = paste(Lake, County)) 
head(data_2)

data_all <- data_1 %>% full_join(data_2, by = c("Lake_County", "Station"))

Lakes = unique(data_all$Lake_County)
print(Lakes)

n = 1
N = length(Lakes)

### for loop with counter ###

for(l in Lakes){
 #subsets data for lake of interest
 Lake <- data_all %>%
  filter(Lake_County == l) 
  file_name = paste(Lake[1], ".pdf")

 ########################################################
  ###put code for knitting pdf from markdown code here###
 
 #rmarkdown::render("LWReport Markdown Code.Rmd", params = list(
   #file = filename)) 
  ###this section doesn't work right now, but the for loop works
  
 ########################################################
 
 #counter
  print(Lake$Lake_County[1])
  print(paste(n, "/", N))
  n <- n+1
}

