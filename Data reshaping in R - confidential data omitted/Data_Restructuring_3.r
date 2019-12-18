
library(rstudioapi); library(readxl); library(tidyverse)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


#Import data

GSES_RAW <- read_excel("GSES_1819_RAW.xlsx")


GSES_FULL_KYLE <- read_excel("GSES_1819_RAW-kyle_split.xlsx")
D4_10_KYLE <- GSES_FULL_KYLE[c("ID","D4_10")]

# Renaming and restructuring data

# Create dummy variables for item D4 - immitating way it is done in previous year
# if GSES_FULL contains 1, create new variable D4_1 = 1
# iterate over D4_1 to D4_10
GSES_RAW <- GSES_RAW %>%
  mutate("D4_1A" = case_when(grepl("^1$",D4) ~ "1")) %>%
  mutate("D4_1B" = case_when(grepl("1,",D4) ~ "1")) %>%
  mutate("D4_1" = coalesce(D4_1A, D4_1B)) %>%

  mutate("D4_2" = case_when(grepl("2",D4) ~ "1")) %>%
  mutate("D4_3" = case_when(grepl("3",D4) ~ "1")) %>%
  mutate("D4_4" = case_when(grepl("4",D4) ~ "1")) %>%
  mutate("D4_5" = case_when(grepl("5",D4) ~ "1")) %>%
  mutate("D4_6" = case_when(grepl("6",D4) ~ "1")) %>%
  mutate("D4_7" = case_when(grepl("7",D4) ~ "1")) %>%
  mutate("D4_8" = case_when(grepl("8",D4) ~ "1")) %>%
  mutate("D4_9" = case_when(grepl("9",D4) ~ "1"))

#  mutate("D4_10A" = case_when(grepl("^10$",D4) ~ "1")) %>%
#  mutate("D4_10B" = case_when(grepl("10,",D4) ~ "1")) %>%
#  mutate(D4_10 = coalesce(D4_10A, D4_10B))
GSES_FULL <- semi_join(GSES_RAW, D4_10_KYLE)


#D4_1_NO_FULL <- which(is.na(GSES_FULL$D4_1))
# 
# D4_10_NO_FULL <- which(is.na(GSES_FULL$D4_10))
# 
# D4_1_NO_UNI <- which(is.na(GSES_UNI_WIDE$D4_1))
# 
# D4_10_NO_UNI <- which(is.na(GSES_UNI_WIDE$D4_10))
# 
# 
# Yes_will <- GSES_UNI_WIDE[-D4_10_NO_UNI,]


# DO EVERYTHING ABOVE FIRST


## University-level analysis - we want examples with distnct student IDs only.
## Responses to Likert are identical for both examples with same student ID. This code deletes all but the first one.

# Remove unneessary variables
GSES_UNI_WIDE <- GSES_FULL %>%
  # Remove examples with duplicate student ID (the other code here is same as GSES_DEP)
  distinct (ID, .keep_all = TRUE) %>%
  # Remove Unnecesssary columns
  select(-ends_with("TEXT")) %>%
  select(c(-L2,-BIRTH_DATE,-Email,-D4,-L1,-L3, -D4_1A, -D4_1B)) %>%
# Renaming Columns
  rename(Gender = GENDER) %>%
  rename(Department = IR_DEPARTMENT_DESC) %>%
  rename(College = IR_COLLEGE_DESC) %>%
  rename(Program = IR_PROGRAM_DESC) %>%

# Reorder columns for later syntax
  subset(select=c(1:28,97:106,29:96))

#write to CSV
###### write.csv(GSES_UNI_WIDE,"GSES_UNI_WIDE.csv", row.names=FALSE)

GSES_UNI_LONG <- GSES_UNI_WIDE %>%
    # Wide to long format conversion
  gather(Question_ID,Likert_Response,1:99) 

glimpse(GSES_UNI_LONG)

#Write to CSV
write.csv(GSES_UNI_LONG,"GSES_UNI_LONG.csv", row.names=FALSE)

#################### NOW USE SAS SCRIPT TO RECODE VARIABLES in GSES_UNI.LONG.CSV, CREATING "Response" Variable for Tableau.
#################### Could redo this in R but too much work.



## Dataset for department-level analyses - keep examples with duplicate student ID numbers because this means they are double majors.
## We want to count them in both programs.

GSES_PROG_WIDE <- GSES_FULL %>%
    # Remove Unnecesssary columns
  subset(select=c(1:28,97:106,29:96)) %>%
  select(-ends_with("TEXT")) %>%
  select(c(-L2,-BIRTH_DATE,-Email,-D4,-L1,-L3, -D4_1A, -D4_1B)) %>%
  # Renaming Columns
  rename(Gender = GENDER) %>%
  rename(Department = IR_DEPARTMENT_DESC) %>%
  rename(College = IR_COLLEGE_DESC) %>%
  rename(Program = IR_PROGRAM_DESC)
  
  # Reorder columns for later syntax
glimpse(GSES_FULL)  

#write to CSV
write.csv(GSES_PROG_WIDE,"GSES_PROG_WIDE_LAST.csv", row.names=FALSE)

GSES_PROG_LONG <- GSES_PROG_WIDE %>%
  # Wide to long format conversion
  gather(Question_ID,Likert_Response,1:99) 

#write to CSV
write.csv(GSES_PROG_LONG,"GSES_PROG_LONG_LAST.csv", row.names=FALSE)

#################### NOW CREATE EXCEL DOC, THEN USE SAS SCRIPT TO RECODE VARIABLES in GSES_PROG.LONG.xlsx, CREATING "Response" Variable for Tableau.
#################### Could redo this in R but too much work.




# Comments on translating for Tableau
#
# Calculated columns; equations for Tableau:
# 1. Avg w/o 5 = "AVG( IIF( [Likert Response] = 5, Null, [Likert Response] ))"
# 2. Avg w/o 6 = "AVG( IIF( [Likert Response] = 6, Null, [Likert Response] ))"
# 3. Avg w/o 0 = "AVG( IIF( [Likert Response] = 0, Null, [Likert Response] ))"#
# 4. "Response" = "[Response  ]"
#
#
#
#
#