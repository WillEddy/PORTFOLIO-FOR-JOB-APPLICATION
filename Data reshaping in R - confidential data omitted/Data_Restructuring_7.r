library(rstudioapi); library(readxl); library(tidyverse); library(data.table)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
DATA_RAW <- read_csv("../data/Joyner Library Data Services Survey_10-3-19_CLEANED.csv")

# Add response numbers - row numbers
DATA <- mutate(DATA_RAW, ID = row_number())

# Put ID as the first column
DATA <- DATA[,c(39, 1:38)]

# Wide to Tall format conversion for Power BI dashboard\
DATA_LONG <- DATA %>%
    gather(Question,Response,2:39)

# Write to CSV
write.csv(DATA_LONG, "Joyner Library Data Services Survey_10-3-19_CLEANED_LONG.csv", row.names = FALSE)

