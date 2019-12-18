
library(rstudioapi); library(readxl); library(tidyverse); library(data.table)
library(stringr); library(dplyr); library(openxlsx)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# __________________________________________________________________________
# Initial data sheet - join 3 yr use data to larger dataset
# __________________________________________________________________________

# Import data
DATA_BIG <- read_excel("../data/JournCollFreedomColl.xlsx", 1)
DATA_SMALL <- read_excel("../data/JournCollFreedomColl.xlsx", 2)

# Remove unidentified columns from small data sheet
DATA_SMALL <- DATA_SMALL[,c(1,2,8)]

# Remove the "-" in the ISSN in the DATA_SMALL sheet to make ISSN match DATA_BIG sheet
DATA_SMALL$ISSN_STRIPPED <- gsub('-', '', DATA_SMALL$ISSN)

# Left outer join to retain all information from SHEET 1, and only include data with matching ISSN from SHEET 2
DATA_JOINED <- merge(x=DATA_BIG,y=DATA_SMALL,by.x="ISSN",by.y="ISSN_STRIPPED",all.x=TRUE)

# Re-order columns
DATA_JOINED <- DATA_JOINED[,c(2,3,10,1,9,4:7)]

# Rename columns for sorting (required for dplyr::arrange function to work without error)
names(DATA_JOINED)[1]<-"Subject_Collection_2020"
names(DATA_JOINED)[2]<-"Full_Title"
names(DATA_JOINED)[3]<-"3_yr_use"

# Re-order rows
DATA_JOINED <- arrange(DATA_JOINED, Subject_Collection_2020 , Full_Title)

# write_csv(DATA_JOINED, "Joined_Data_Prototype.csv")

# __________________________________________________________________________
# End first data sheet
# Begin "counts" data sheet
# __________________________________________________________________________

# __________________________________________________________________________
# Part 1: Create count of NA values in 3 year use, grouped by Subject Collection
DATA_NA_COUNTS <- DATA_JOINED %>%
  group_by(Subject_Collection_2020) %>%
  add_count(`3_yr_use`, name="NA_Count")

# Identify and isolate rows have NA values in 3 year use
NA_ONES <- which(is.na(DATA_NA_COUNTS[,3]))
# Return only the rows with NA as identified immediately above
NA_ONES <- DATA_NA_COUNTS[NA_ONES,]
# Give the first example of each Subject Collection
NA_ONES <- NA_ONES %>%
  group_by(Subject_Collection_2020) %>%
  slice(1)
# __________________________________________________________________________

# __________________________________________________________________________
# Part 2: Identify and Label rows that have NOT-NA values in 3 year use
NOT_NA_ROWS <- which(!is.na(DATA_JOINED[,3]))
NOT_NA_ONES <- DATA_JOINED
NOT_NA_ONES[NOT_NA_ROWS,"Not_NA"] <- "1"

NOT_NA_ONES <- NOT_NA_ONES %>%
  group_by(Subject_Collection_2020) %>%
  add_count(`Not_NA`, name="NOT_NA_Count")

# Isolate only those subject collections that do no have NA in the ISSN.y
NOT_NA_ONES <- NOT_NA_ONES[,c(1,4,5,11)]
NOT_NA_ONES <- NOT_NA_ONES[complete.cases(NOT_NA_ONES),]

# Give the first example of each Subject Collection
NOT_NA_ONES <- NOT_NA_ONES %>%
  group_by(Subject_Collection_2020) %>%
  slice(1)
# __________________________________________________________________________

# __________________________________________________________________________
# Part 3: Total use counts, grouped by Subject Collection

TOTAL_USE_COUNTS <- DATA_JOINED %>%
  group_by(Subject_Collection_2020) %>%
  add_tally(`3_yr_use`, name="Total_3_yr_use")

# Give the first example of each Subject Collection
TOTAL_USE_COUNTS <- TOTAL_USE_COUNTS %>%
  group_by(Subject_Collection_2020) %>%
  slice(1)

# __________________________________________________________________________

# Joining counts data frames
DATA_COUNTS_SHEET <- merge(x=NA_ONES,y=NOT_NA_ONES,by.x="Subject_Collection_2020",by.y="Subject_Collection_2020")

DATA_COUNTS_SHEET <- merge(x=DATA_COUNTS_SHEET,y=TOTAL_USE_COUNTS,by.x="Subject_Collection_2020",by.y="Subject_Collection_2020")

DATA_COUNTS_SHEET <- DATA_COUNTS_SHEET[,c(1,10,13,22)]


write_csv(DATA_COUNTS_SHEET, "DATA_COUNTS_Prototype.csv")

# Attempted to write directly to xlsx, but decided to just do csv instead
#write.xlsx(DATA_COUNTS_SHEET, "Data_Joined_Prototype.xlsx", addWorksheet = TRUE, sheetname = "Counts")
#write.xlsx(DATA_JOINED, "Data_Joined_Prototype.xlsx", addWorksheet = TRUE, sheetname = "Data Joined")



