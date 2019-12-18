
library(rstudioapi); library(readxl); library(tidyverse)
# library(stringr); library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Import raw data
GSS_FULL <- read_excel("./data/GSS_1819_FINAL.xlsx")

# REMOVE UNNECESSARY VARIABLES

GSS_FULL <- GSS_FULL %>%
  #distinct (ID, .keep_all = TRUE) %>%
  # LINE ABOVE WILL Remove examples with duplicate student ID. fOR USE WITH UNIVERITY-LEVEL REPORT FOR PUBLIC CONSUMPTION
  # WITHOPUT THIS LINE, DATA IS ONLY VALID AT DEPARTMENT / PROGRAM LEVEL
  
  # Remove Unnecesssary columns
    select(-ends_with("TEXT")) %>%
  select(-starts_with("Q3_5")) %>%
  select(c(-Q3_2,-Q3_6,-Q8_2,-Q8_3,-Q8_4,-Q8_5,-Q11_5,-Q11_6,-GENDER,-BIRTH_DATE)) %>%
  select(-starts_with("Q12")) %>%
  # Renaming Columns
  #rename(Gender = GENDER) %>%
  rename(Department = IR_DEPARTMENT_DESC) %>%
  rename(College = IR_COLLEGE_DESC) %>%
  rename(Program = IR_PROGRAM_DESC) %>%
  rename(Academic_Period = ACADEMIC_PERIOD_GRADUATION) %>%
  rename(Race_Ethnicity = IPEDS_RACE_ETH_CODE)

######## CHANGE THE Likert VALUES TO CORRESPONDING STRING VALUES, THEN APPEND TO EXISTING LIKERT VALUES

GSS_FULL_STRINGS <- GSS_FULL %>%

# Q1_1
  mutate_at(vars(starts_with('Q1_1')), list(~ ifelse(. == 1, '1: Strongly Disagree', .))) %>%
  mutate_at(vars(starts_with('Q1_1')), list(~ ifelse(. == 2, '2: Disagree', .))) %>%
  mutate_at(vars(starts_with('Q1_1')), list(~ ifelse(. == 3, '3: Neither Agree nor Disagree', .))) %>%
  mutate_at(vars(starts_with('Q1_1')), list(~ ifelse(. == 4, '4: Agree', .))) %>%
  mutate_at(vars(starts_with('Q1_1')), list(~ ifelse(. == 5, '5: Strongly Agree', .))) %>%
#Q2_1
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 0, '0: Don\'t Know', .))) %>%
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 1, '1: Not at all', .))) %>%
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 2, '2: Very Little', .))) %>%
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 3, '3: Somewhat', .))) %>%
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 4, '4: Very Much', .))) %>%
#Q2_2
  mutate_at(vars(starts_with('Q2_2')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q2_2')), list(~ ifelse(. == 2, '2: No', .))) %>%
#Q2_3  
  mutate_at(vars(starts_with('Q2_3')), list(~ ifelse(. == 1, '1: Not at all', .))) %>%
  mutate_at(vars(starts_with('Q2_3')), list(~ ifelse(. == 2, '2: Very Little', .))) %>%
  mutate_at(vars(starts_with('Q2_3')), list(~ ifelse(. == 3, '3: Somewhat', .))) %>%
  mutate_at(vars(starts_with('Q2_3')), list(~ ifelse(. == 4, '4: Very Much', .))) %>%
#Q2_4
  mutate_at(vars(starts_with('Q2_4')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q2_4')), list(~ ifelse(. == 2, '2: No', .))) %>%
#Q2_5
  mutate_at(vars(starts_with('Q2_5')), list(~ ifelse(. == 1, '1: Not at all', .))) %>%
  mutate_at(vars(starts_with('Q2_5')), list(~ ifelse(. == 2, '2: Very Little', .))) %>%
  mutate_at(vars(starts_with('Q2_5')), list(~ ifelse(. == 3, '3: Somewhat', .))) %>%
  mutate_at(vars(starts_with('Q2_5')), list(~ ifelse(. == 4, '4: Very Much', .))) %>%
#Q2_6
  mutate_at(vars(starts_with('Q2_6')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q2_6')), list(~ ifelse(. == 2, '2: No', .))) %>%
#Q2_7
  mutate_at(vars(starts_with('Q2_7')), list(~ ifelse(. == 1, '1: Not at all', .))) %>%
  mutate_at(vars(starts_with('Q2_7')), list(~ ifelse(. == 2, '2: Very Little', .))) %>%
  mutate_at(vars(starts_with('Q2_7')), list(~ ifelse(. == 3, '3: Somewhat', .))) %>%
  mutate_at(vars(starts_with('Q2_7')), list(~ ifelse(. == 4, '4: Very Much', .))) %>%
#Q3_1
  mutate_at(vars(starts_with('Q3_1')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q3_1')), list(~ ifelse(. == 2, '2: No', .))) %>%
#Q3_2 IS A TEXT STRING
#Q3_3  
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 1, '1: Employed full time (on average 30 hours or more per week)', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 2, '2: Employed part time (on average less than 30 hours per week)', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 3, '3: Participating in a volunteer or service program (e.g., Peace Corps)', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 4, '4: Serving in the U.S. military', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 5, '5: Enrolled full time in a program of continuing education (degree or non-degree program)', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 6, '6: Enrolled part-time in a program of continuing education (degree or non-degree program)', .))) %>%
  mutate_at(vars(starts_with('Q3_3')), list(~ ifelse(. == 7, '7: Not seeking employment or continuing education', .))) %>%
#Q3_4
  mutate_at(vars(starts_with('Q3_4')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q3_4')), list(~ ifelse(. == 2, '2: No', .))) %>%
  
#Q3_5 IS A TEXT STRING
#Q3_6 IS A TEXT STRING
  
#Q4_1
  mutate_at(vars(starts_with('Q4_1')), list(~ ifelse(. == 1, '1: Never', .))) %>%
  mutate_at(vars(starts_with('Q4_1')), list(~ ifelse(. == 2, '2: Once', .))) %>%
  mutate_at(vars(starts_with('Q4_1')), list(~ ifelse(. == 3, '3: 2-9 Times', .))) %>%
  mutate_at(vars(starts_with('Q4_1')), list(~ ifelse(. == 4, '4: 10 times or More', .))) %>%
#Q4_2
  mutate_at(vars(starts_with('Q4_2')), list(~ ifelse(. == 1, '1: Strongly Disagree', .))) %>%
  mutate_at(vars(starts_with('Q4_2')), list(~ ifelse(. == 2, '2: Disagree', .))) %>%
  mutate_at(vars(starts_with('Q4_2')), list(~ ifelse(. == 3, '3: Neither Agree nor Disagree', .))) %>%
  mutate_at(vars(starts_with('Q4_2')), list(~ ifelse(. == 4, '4: Agree', .))) %>%
  mutate_at(vars(starts_with('Q4_2')), list(~ ifelse(. == 5, '5: Strongly Agree', .))) %>%
#Q5_1
  mutate_at(vars(starts_with('Q5_1')), list(~ ifelse(. == 1, '1: Faculty Advisor', .))) %>%
  mutate_at(vars(starts_with('Q5_1')), list(~ ifelse(. == 2, '2: Full-time professional advisor', .))) %>%
  mutate_at(vars(starts_with('Q5_1')), list(~ ifelse(. == 3, '3: Both faculty and professional advisor', .))) %>%
#Q5_2
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 5, '5: Very Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 0, '0: Don\'t Know / Not Applicable', .))) %>%
#Q6_1
  mutate_at(vars(starts_with('Q6_1')), list(~ ifelse(. == 1, '1: Joyner Library', .))) %>%
  mutate_at(vars(starts_with('Q6_1')), list(~ ifelse(. == 2, '2: Laupus  Library', .))) %>%
  mutate_at(vars(starts_with('Q6_1')), list(~ ifelse(. == 3, '3: Music  Library', .))) %>%
  mutate_at(vars(starts_with('Q6_1')), list(~ ifelse(. == 4, '4: Did not use these libraries  ', .))) %>%
#Q6_2
  mutate_at(vars(starts_with('Q6_2')), list(~ ifelse(. == 1, '1: Never', .))) %>%
  mutate_at(vars(starts_with('Q6_2')), list(~ ifelse(. == 2, '2: Once', .))) %>%
  mutate_at(vars(starts_with('Q6_2')), list(~ ifelse(. == 3, '3: 2-9 Times', .))) %>%
  mutate_at(vars(starts_with('Q6_2')), list(~ ifelse(. == 4, '4: 10 times or More', .))) %>%
#Q6_3
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 5, '5: Very Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 0, '0: Don\'t Know / Not Applicable', .))) %>%
#Q7_1
  mutate_at(vars(starts_with('Q7_1')), list(~ ifelse(. == 1, '1: ECU Career Services', .))) %>%
  mutate_at(vars(starts_with('Q7_1')), list(~ ifelse(. == 2, '2: COB Career Center  ', .))) %>%
  mutate_at(vars(starts_with('Q7_1')), list(~ ifelse(. == 3, '3: Did not utilize either', .))) %>%
#Q7_2
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 5, '5: Very Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 0, '0: Don\'t Know / Not Applicable', .))) %>%
#Q7_3
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 1, '1: Before new student orientation', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 2, '2: At new student orientation', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 3, '3: Freshman year', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 4, '4: Sophomore year', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 5, '5: Junior year', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 6, '6: Senior year', .))) %>%
  mutate_at(vars(starts_with('Q7_3')), list(~ ifelse(. == 7, '7: Never', .))) %>%
#Q8_1
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 5, '5: Very Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 0, '0: Don\'t Know / Not Applicable', .))) %>%

  #Q8_2 IS A TEXT STRING
  #Q8_3 IS A TEXT STRING
  #Q8_3 and Q8_4 ARE IRRELEVANT

  #Q9_1
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 5, '5: Very Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 0, '0: No experience to judge', .))) %>%
#Q9_2
  mutate_at(vars(starts_with('Q9_2')), list(~ ifelse(. == 1, '1: Never', .))) %>%
  mutate_at(vars(starts_with('Q9_2')), list(~ ifelse(. == 2, '2: Seldom', .))) %>%
  mutate_at(vars(starts_with('Q9_2')), list(~ ifelse(. == 3, '3: Sometimes', .))) %>%
  mutate_at(vars(starts_with('Q9_2')), list(~ ifelse(. == 4, '4: Frequently', .))) %>%
#Q10_1
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 1, '1: Strongly Disagree', .))) %>%
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 2, '2: Disagree', .))) %>%
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 3, '3: Neither Agree nor Disagree', .))) %>%
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 4, '4: Agree', .))) %>%
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 5, '5: Strongly Agree', .))) %>%
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 0, '0: N/A', .))) %>%
#Q11_1
  mutate_at(vars(starts_with('Q11_1')), list(~ ifelse(. == 1, '1: Very Weak', .))) %>%
  mutate_at(vars(starts_with('Q11_1')), list(~ ifelse(. == 2, '2: Weak', .))) %>%
  mutate_at(vars(starts_with('Q11_1')), list(~ ifelse(. == 3, '3: Strong', .))) %>%
  mutate_at(vars(starts_with('Q11_1')), list(~ ifelse(. == 4, '4: Very Strong', .))) %>%
#Q11_2
  mutate_at(vars(starts_with('Q11_2')), list(~ ifelse(. == 1, '1: Poor', .))) %>%
  mutate_at(vars(starts_with('Q11_2')), list(~ ifelse(. == 2, '2: Fair', .))) %>%
  mutate_at(vars(starts_with('Q11_2')), list(~ ifelse(. == 3, '3: Good', .))) %>%
  mutate_at(vars(starts_with('Q11_2')), list(~ ifelse(. == 4, '4: Excellent', .))) %>%
#Q11_3
  mutate_at(vars(starts_with('Q11_3')), list(~ ifelse(. == 1, '1: Yes', .))) %>%
  mutate_at(vars(starts_with('Q11_3')), list(~ ifelse(. == 2, '2: Not Sure', .))) %>%
  mutate_at(vars(starts_with('Q11_3')), list(~ ifelse(. == 3, '3: No', .))) %>%
  #Q11_4
  mutate_at(vars(starts_with('Q11_4')), list(~ ifelse(. == 1, '1: Very Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q11_4')), list(~ ifelse(. == 2, '2: Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q11_4')), list(~ ifelse(. == 3, '3: Neither Satisfied nor Dissatisfied', .))) %>%
  mutate_at(vars(starts_with('Q11_4')), list(~ ifelse(. == 4, '4: Satisfied', .))) %>%
  mutate_at(vars(starts_with('Q11_4')), list(~ ifelse(. == 5, '5: Very Satisfied', .)))

#Q11_5 IS A TEXT STRING
#Q11_6 IS A TEXT STRING  

#Q12 group eliminated  
  
# Append "_S" to all column names
colnames(GSS_FULL_STRINGS) <- paste(colnames(GSS_FULL_STRINGS),"S",sep="_")

# Wide to long conversion
GSS_FULL_STRINGS_LONG <- GSS_FULL_STRINGS %>%
  gather(Question_ID,Likert_Response,1:110) 



############################ CREATE NEW VARIABLE FOR LIKERT WHERE N/A RESPONSES ETC WON'T MESS UP THE MEANS
GSS_FULL_NOZEROES <- GSS_FULL %>%
  
  #Q2_1
  mutate_at(vars(starts_with('Q2_1')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q5_2
  mutate_at(vars(starts_with('Q5_2')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q6_3
  mutate_at(vars(starts_with('Q6_3')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q7_2
  mutate_at(vars(starts_with('Q7_2')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q8_1
  mutate_at(vars(starts_with('Q8_1')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q9_1
  mutate_at(vars(starts_with('Q9_1')), list(~ ifelse(. == 0, 'NA', .))) %>%
  #Q10_1
  mutate_at(vars(starts_with('Q10_1')), list(~ ifelse(. == 0, 'NA', .)))

# Wide to long conversion
GSS_FULL_NOZEROES_LONG <- GSS_FULL_NOZEROES %>%
  gather(Question_ID,Likert_Response,1:110) 

############################ END NEW VARIABLE SET FOR MEANS

# Join strings data with Likert reponse data - want both Likert and String responses in same table - using bind_cols() because we know these datasets match

GSS_BOTH_LONG <- bind_cols(GSS_FULL_NOZEROES_LONG,GSS_FULL_STRINGS_LONG)

# Eliminate more unnecesssary columns
# Remove Unnecesssary columns and rename strings column
GSS_BOTH_LONG <- GSS_BOTH_LONG %>%
  select(c(-ID_S,-Department_S,-Program_S,-College_S,-Academic_Period_S,-Race_Ethnicity_S,-Question_ID1)) %>%
  rename(String_Response = Likert_Response1)

#WRITE TO FILE
write.csv(GSS_BOTH_LONG, "GSS_PROG_LIKERT_AND_STRINGS_LONG.csv")


