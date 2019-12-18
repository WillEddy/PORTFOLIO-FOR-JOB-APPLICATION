
library(rstudioapi); library(readxl); library(tidyverse); library(data.table)
library(stringr)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# Import data
DATA_RAW <- read.csv("../data/AlternativeTextbookSavings_AllYears_NoText.csv")

# Create aggregate columns
DATA <- DATA_RAW %>%
  mutate(`Fall Total` = (Estimated.Textbook.Cost * Est.Students.Fall)) %>%
  mutate(`Spring Total` = (Estimated.Textbook.Cost * Est.Students.Spring))

# Removing # characters to create a "department" variable
DATA$Department <- gsub('[[:digit:]]+', '', DATA$Course.Number)

# Manually fixing the single department string issue
DATA[4,9] <- "GEOL"

# Manually removing 2 "unspecified" rows infecting dashboard
DATA <- DATA[-54,]
DATA <- DATA[-44,]


# Write dataset
write.csv(DATA, "ATS_With_Totals_and_Dept.csv", row.names = FALSE)
