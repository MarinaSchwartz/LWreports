### Import Libraries ###
###IMPORTING LIBRARIES AND LOADING DATA###
##########################################################################################
#Commenting out this code to add to the for loop file
#importing libraries 
library(tidyverse)
library(gt)
library(SciViews)
library(broom)
#broom is just to get values form lm function in easier to reference from
library(glue)
library(ggtext)
#glue and ggtext are for making labels. trying to make labels and get default ggplot functions to correctly parse those labels is a nightmare. ggtext helps by allowing you to use html tags in the labels. glue helps by allowing you to use variables in the labels.
library(readxl)
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
  
  gmean <- function(x){
    xc <- x[!is.na(x)]
    xg <- exp(mean(log(xc[xc>0])))
    xg <- round(xg)
    return(xg)
  } 
  
  
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


