### Set WD ###
path <- "C:/Users/amber.riner/Documents/LWreports"
setwd(path)


### Import Libraries ###
library(readxl)
library(tidyverse)
library(tinytex)

#tinytex::install_tinytex()
#tinytex::tlmgr_update()


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

#data_all <- data_1 %>% full_join(data_2, by = c("Lake_County", "Station"))

#Lakes = unique(data_2$Lake_County)
Lakes = c("Alice Alachua", "Bivans Arm Alachua")
print(Lakes)

n = 1
N = length(Lakes)

### for loop with counter ###

for(l in Lakes){
 #subsets data for lake of interest
  
Lake_1 <- data_1 %>%
  filter(Lake_County == l)
  
Lake_2 <- data_2 %>%
  filter(Lake_County == l) 
  file_name = paste(Lake_2[1], ".pdf")

 ########################################################
  ###put code for knitting pdf from markdown code here###
 
 rmarkdown::render(input = "LWReport Markdown Code.Rmd", output_format = "pdf_document",         # 2. Establish the format
                   output_file = paste0(l ,"_report.pdf"), # 3. Define the output file name
                   output_dir = "output",                       # 4. Define an output folder/directory
                   params = list(Lakes = l))  
  
 ########################################################
 
 #counter
  print(Lake_2$Lake_County[1])
  print(paste(n, "/", N))
  n <- n+1
}


