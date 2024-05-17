# Project Notes ----
#

### WD and Packages ----

#setwd("/Documents/GitHub/LWreports/.RData")

library(tidyverse)
library(gt)
library(SciViews)
library(broom)
library(glue)
library(ggtext)
library(readxl)
library(tinytex)

#tinytex::install_tinytex()
#tinytex::tlmgr_update()



### Data Import ---- 
data_1 = read_xlsx("Data/Lakewatch Base File RMD.xlsx")
data_2 = read.csv("Data/All_Data_RMD.csv")

### Functions ----

gmean <- function(x){
  xc <- x[!is.na(x)]
  xg <- exp(mean(log(xc[xc>0])))
  xg <- round(xg)
  return(xg)
}

### Data Prep ----

data_1 <- data_1 %>%
  mutate(Lake_County = paste(Lake, County)) %>%
  filter(Study == "LW") %>%
  filter(County == "Charlotte") %>%
  filter(`water type` == "Lake" | `water type` == "River/Stream" | `water type` == "Estuary") 
head(data_1)


data_2 = data_2 %>%
  mutate(Station = as.character(Station)) %>%
  mutate(Lake_County = paste(Lake, County)) 
head(data_2)

#add na.ignore here to avoid loop error when no data exists for a lake
data_all <- data_1 %>% full_join(data_2, by = c("Lake_County", "Station"))
head(data_all)

### for loop with counter ----

#Run ONE of these two lines
Lakes = unique(data_1$Lake_County)
#Lakes = c("Cudjoe-9 Monroe")
         #  ,
         #  "Grand Haven 12 Flagler",
         #  "Grand Haven 13 Flagler",
         #  "Grand Haven 18A Flagler",
         #  "Grand Haven 28 Flagler",
         #  "Grand Haven 4 Flagler",
         #  "Grand Haven 5 Flagler",
         # "Grand Haven W6 Flagler")

print(Lakes)

n_count = 1
N_count = length(Lakes)


for(l in Lakes){
  
 #subsets data for lake of interest
  
  Lake_1 <- data_1 %>%
  filter(Lake_County == l)
  
  Lake_2 <- data_2 %>%
  filter(Lake_County == l) 
  file_name = paste(Lake_2[1], ".pdf")
  
  
    #adding a column using if/else for lake classification
    ##this code currently has no fallback for if the data is missing.
    Lake_2 = Lake_2 %>% mutate(lake_class = ifelse(
      gmean(`Color`) > 40, "Colored", ifelse(
        gmean(`Color`) <= 40 & gmean(`Cond_uS`) <= 20, "Clear Soft Water","Clear Hard Water")))
    
    #adding a column using if/else for trophic state
    Lake_2 = Lake_2 %>% mutate(trophic_state = ifelse(
      gmean(CHL) < 3, "Oligotrophic", ifelse(
        gmean(CHL) >= 3 & gmean(CHL) < 7, "Mesotrophic", ifelse(
          gmean(CHL) >= 7 & gmean(CHL) < 40, "Eutrophic", "Hypereutrophic"))))
  
    if(Lake_1$`water type`[1] == "Lake") {
      
      
      rmarkdown::render(input = "Scripts/LWReport Markdown Code Lake.Rmd", output_format = "pdf_document",         # 2. Establish the format
                        output_file = paste0(l ,"_report.pdf"), # 3. Define the output file name
                        output_dir = "Output/Lake",                       # 4. Define an output folder/directory
                        params = list(Lakes = l))  }
    
    if(Lake_1$`water type`[1] == "Estuary") {
      
      
      rmarkdown::render(input = "Scripts/LWReport Markdown Code Estuary.Rmd", output_format = "pdf_document",         # 2. Establish the format
                        output_file = paste0(l ,"_report.pdf"), # 3. Define the output file name
                        output_dir = "Output/Estuary",                       # 4. Define an output folder/directory
                        params = list(Lakes = l))  }
    
    if(Lake_1$`water type`[1] == "River/Stream") {
      
      
      rmarkdown::render(input = "Scripts/LWReport Markdown Code RiverStream.Rmd", output_format = "pdf_document",         # 2. Establish the format
                        output_file = paste0(l ,"_report.pdf"), # 3. Define the output file name
                        output_dir = "Output/RiverStream",                       # 4. Define an output folder/directory
                        params = list(Lakes = l))  }

  
 
 #counter
  print(Lake_2$Lake_County[1])
  print(paste(n_count, "/", N_count))
  n_count <- n_count+1
}

