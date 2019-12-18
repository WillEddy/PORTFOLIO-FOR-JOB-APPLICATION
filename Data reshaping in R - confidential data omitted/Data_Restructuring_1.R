
# We need you to add a yellow highlight in Column L for any outcomes (Unit Objective Name - Column B) 
# that are marked as Active (Column G)
# but do not have content entered in the Actions, Taken Result & Quality Enhancement/Improvement (Column J)
# or Actions Planned (Column K).

library(rstudioapi); library(readxl); library(tidyverse); library(data.table)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
DATA <- read_excel("./REPORT.xls")
#DATA_SMALL <- DATA[1:100,]

# This data file has examples that encompass several rows. The following code fills in blank cells with the one above in certian columns
# until the next filled cell. Then it takes the data from that cell and fills below, and repeats to the end. Look at before and after tibbles for clarity.
DATA_FILLED <- DATA %>%
  fill_(c("Unit Name", "Unit Objective Name", "Outcome Type", "Outcome Status", "5-Year Assessment Cycle", "Means of Assessment"))

#Add row numbers to keep it all straight
DATA_FILLED <- mutate(DATA_FILLED, ID = row_number())

#__________________________________________________
# Part 1, "Which departrmnts?"

#which column 2 rows contain anything?
TWO <- !is.na(DATA_FILLED[,2])
TWO_VEC <- which(TWO)

#Which of those rows where YES to last qualification also contain ACTIVE in column 7?
TWO_ACTIVE <- !is.na(DATA_FILLED[,7])
TWO_VEC_ACTIVE <- which(TWO_ACTIVE)

#__________________________________________________
# Part 2, "activity in department"

#Which column 10 rows don't contain anything?
TEN <- is.na(DATA_FILLED[,10])
TEN_VEC <- which(TEN)

#Which column 11 rows don't contain anything?
ELEVEN <- is.na(DATA_FILLED[,11])
ELEVEN_VEC <- which(ELEVEN)

# What rows do the above two have in common?
NO_10_AND_11 <- intersect(TEN_VEC, ELEVEN_VEC)
NO_TEN_ELEVEN_ROWS <- DATA_FILLED[NO_10_AND_11,]


#__________________________________________________
# Part 3, "Part 1 and part 2 together"

# Identified final rows of concern
YELLOW_ONES <- intersect(TWO_VEC_ACTIVE,NO_10_AND_11)
YELLOW_ROWS <- DATA_FILLED[YELLOW_ONES,]

# NEW column in these rows containing 1
YELLOW_ROWS$CHECK_THESE_ONES <- rep(1,nrow(YELLOW_ROWS))

# Connect YELLOW with NOT YELLOW
WITH_YELLOW <- left_join(DATA_FILLED, YELLOW_ROWS, by = "ID")

#__________________________________________________
# Part 4, clean up for output

# Remove useless extra columns
LABELED <- WITH_YELLOW %>%
  select(-ends_with(".y")) %>%
  select(-ID)

# Remove ".x" suffix added in left join
colnames(LABELED) <- strsplit(colnames(LABELED), split = ".x")

# Remove label rows

LABELED <- LABELED[rowSums(is.na(LABELED[,7:12]))!=6,]

# Remove years before current 5 year cycle
LABELED_5YEAR <- LABELED %>%
  filter(`Reporting Year` != "2002-2003" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2003-2004" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2004-2005" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2005-2006" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2006-2007" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2007-2008" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2008-2009" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2009-2010" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2010-2011" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2011-2012" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2012-2013" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2013-2014" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2014-2015" | is.na(`Reporting Year`)) %>%
  filter(`Reporting Year` != "2015-2016" | is.na(`Reporting Year`))

LABELED <- LABELED_5YEAR

write.csv(LABELED_5YEAR, "Labeled_5-year.csv", col.names = FALSE)

# Susan wanted to know how many distinct programs (column A) had issues
count(unique(YELLOW_ROWS[1]))
count(unique(LABELED_5YEAR[1]))


# After this, in excel - Resize to 20 width and 120 height. Find and replace all match whole cell "NA" with BLANK instead. 
# Set all cells to wrap text. Label YELLOW column as yellow colow where YELLOW = 1

# END OF SUSAN / JEANETTE PART OF TASK

#_________________________________________________
# BEGIN YIHUI PART OF TASK

#Identify good rows
GOOD_ONES <- which(is.na(LABELED$CHECK_THESE_ONES))
GOOD_ROWS <- LABELED[GOOD_ONES,1:11]

# SORT THEM
GOOD_ORDERED <- GOOD_ROWS %>%
  arrange(`Unit Name`, `Unit Objective Name`, `Means of Assessment` , desc(`Reporting Year`))

# RETURN ONLY FIRST OF EACH OBJECTIVE - Will be most recent year
NEWEST_OBJECTIVES <- GOOD_ORDERED %>%
  group_by(`Means of Assessment`) %>%
  slice(1)

write.csv(NEWEST_OBJECTIVES, "5-year_Newest.csv", row.names = FALSE)

# Compare length to # of unique objectives
count(unique(GOOD_ORDERED[6]))

