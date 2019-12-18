
library(rstudioapi); library(readxl); library(tidyverse); library(data.table)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# FIRST, In excel I:
# 1. Deleted all rows without names
# 2. Deleted item number labels
# 3. Did a "find and replace" to get rid of question prompt, leaving only subject matter
# 4. Renamed the Name row to "Name"
# 5. Corrected capitalizaion of names
# TO DO: Add missing last names


# Import data
DATA_RAW <- read_csv("../data/DATA.csv")

# Remove useless columns
DATA_WithText <- DATA_RAW[,18:(ncol(DATA_RAW))]

#Add row numbers to keep it all straight
DATA_WithText <- mutate(DATA, ID = row_number())



# ____________________________________________
# Public facing version has "Text" columns deleted

# Remove text input columns
DATA_NoText <- DATA_WithText %>%
  select(-ends_with("Text"))
DATA_NoText <- DATA_NoText[,c(27,1:24)]

DATA_LONG_NoText <- DATA_NoText %>%
  # Wide to long format conversion
  gather(Skill,Familiarity,2:24) 

# Export external version data
write_excel_csv(DATA_LONG_NoText, "Cleaned_Data_Long_NoText.csv")
# ____________________________________________



# ____________________________________________
# Internal facing version does not have "Text" columns deleted

names(DATA_WithText)[48]<-"Education - Text2"
names(DATA_WithText)[49]<-"Connections - Text3"


# ------------------------------------------------------------------------------------
# StackOverflow Solution to column reshaping
library(dplyr)
library(tidyr)

DATA_WithText_Long <-  bind_cols(DATA_WithText %>% 
            select(-contains('text')) %>% 
            gather(Skill, Familiarity, -1),
          DATA_WithText %>% select(contains('text')) %>% 
            gather(var, Details) %>% 
            select(-var)
)

# ------------------------------------------------------------------------------------


# Wide to long conversion
write_excel_csv(DATA_WithText_Long, "Cleaned_Data_Long_WithText.csv")



# ------------------------------------------------------------------------------------
# Unusued Code Snippets

# ends_with(DATA_WithText ,("Text"))

# DATA_TextOnly <- DATA_WithText %>%
#  select(1, (ends_with("Text")))


