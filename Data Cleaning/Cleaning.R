rm(list=ls())
library(tidyverse)
library(caret)
load("nlsy97.RData")

# Inflation
inflation <- data.frame(Year = c(1997:2023), # compared to January 1st, 2024; data from BLS CPI Inflation Calculator
                        Buying.Power = c(1.94,
                                         1.91,
                                         1.88,
                                         1.83,
                                         1.76,
                                         1.74,
                                         1.70,
                                         1.67,
                                         1.62,
                                         1.56,
                                         1.52,
                                         1.46,
                                         1.46,
                                         1.42,
                                         1.40,
                                         1.36,
                                         1.34,
                                         1.32,
                                         1.32,
                                         1.30,
                                         1.27,
                                         1.24,
                                         1.23,
                                         1.20,
                                         1.18,
                                         1.10,
                                         1.03))

# CVC_ASSETS_RND_20_XRND
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==3] <- 1999
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==4] <- 2000
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==5] <- 2001
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==6] <- 2002
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==7] <- 2003
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==8] <- 2004
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==9] <- 2005
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==10] <- 2006
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==11] <- 2007
nlsy97$CVC_ASSETS_RND_20_XRND[nlsy97$CVC_ASSETS_RND_20_XRND==12] <- 2008
    
# CVC_ASSETS_RND_25_XRND
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==9] <- 2005
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==10] <- 2006
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==11] <- 2007
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==12] <- 2008
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==13] <- 2009
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==14] <- 2010
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==15] <- 2011
nlsy97$CVC_ASSETS_RND_25_XRND[nlsy97$CVC_ASSETS_RND_25_XRND==16] <- 2003
    
# CVC_ASSETS_RND_30_XRND
nlsy97$CVC_ASSETS_RND_30_XRND[nlsy97$CVC_ASSETS_RND_30_XRND==14] <- 2010
nlsy97$CVC_ASSETS_RND_30_XRND[nlsy97$CVC_ASSETS_RND_30_XRND==15] <- 2011
nlsy97$CVC_ASSETS_RND_30_XRND[nlsy97$CVC_ASSETS_RND_30_XRND==16] <- 2013
nlsy97$CVC_ASSETS_RND_30_XRND[nlsy97$CVC_ASSETS_RND_30_XRND==17] <- 2015
nlsy97$CVC_ASSETS_RND_30_XRND[nlsy97$CVC_ASSETS_RND_30_XRND==18] <- 2017
    
# CVC_ASSETS_RND_35_XRND
nlsy97$CVC_ASSETS_RND_35_XRND[nlsy97$CVC_ASSETS_RND_35_XRND==17] <- 2015
nlsy97$CVC_ASSETS_RND_35_XRND[nlsy97$CVC_ASSETS_RND_35_XRND==18] <- 2017
nlsy97$CVC_ASSETS_RND_35_XRND[nlsy97$CVC_ASSETS_RND_35_XRND==19] <- 2019
nlsy97$CVC_ASSETS_RND_35_XRND[nlsy97$CVC_ASSETS_RND_35_XRND==20] <- 2021
    
# CVC_ASSETS_RND_40_XRND
nlsy97$CVC_ASSETS_RND_40_XRND[nlsy97$CVC_ASSETS_RND_40_XRND==20] <- 2021
    
# id
nlsy97 <- nlsy97 %>% rename(id = PUBID_1997)

# sex
nlsy97 <- nlsy97 %>% rename(sex = KEY_SEX_1997)
nlsy97$sex <- ifelse(nlsy97$sex == 1, "Male", nlsy97$sex)
nlsy97$sex <- ifelse(nlsy97$sex == 2, "Female", nlsy97$sex)
nlsy97$sex <- factor(nlsy97$sex, levels=c("Male", "Female"))

# parent_income
nlsy97 <- nlsy97 %>% 
  rename(parent_income = CV_INCOME_GROSS_YR_1997) %>%
  mutate(parent_income = parent_income * filter(inflation, Year==1997)$Buying.Power) %>% # adjusting for inflation
  mutate(parent_income = parent_income / 1000) # in $1000s

# parent_net_worth
nlsy97 <- nlsy97 %>% 
  rename(parent_net_worth = CV_HH_NET_WORTH_P_1997) %>%
  mutate(parent_net_worth = parent_net_worth * filter(inflation, Year==1997)$Buying.Power) %>% # adjusting for inflation
  mutate(parent_net_worth = parent_net_worth / 1000) # in $1000s
  
# parent_education
nlsy97$CV_HGC_RES_DAD_1997[nlsy97$CV_HGC_RES_DAD_1997==95] <- NA # data entry error? 
nlsy97$CV_HGC_RES_MOM_1997[nlsy97$CV_HGC_RES_MOM_1997==95] <- NA # data entry error?
nlsy97$CV_HGC_RES_DAD_1997[is.na(nlsy97$CV_HGC_RES_DAD_1997)] <- 0
nlsy97$CV_HGC_RES_MOM_1997[is.na(nlsy97$CV_HGC_RES_MOM_1997)] <- 0
nlsy97$parent_education <- ifelse(nlsy97$CV_HGC_RES_MOM_1997 > nlsy97$CV_HGC_RES_DAD_1997, nlsy97$CV_HGC_RES_MOM_1997, nlsy97$CV_HGC_RES_DAD_1997)
nlsy97$parent_education[nlsy97$parent_education==0] <- NA # there are no residential mothers and no residential father's in the sample that have 0 education

# race
nlsy97 <- nlsy97 %>% rename(race = KEY_RACE_ETHNICITY_1997)
nlsy97$race[nlsy97$race==1] <- "Black"
nlsy97$race[nlsy97$race==2] <- "Hispanic"
nlsy97$race[nlsy97$race==3] <- "Multiracial (Non-Hispanic)"
nlsy97$race[nlsy97$race==4] <- "Non-Black / Non-Hispanic"
nlsy97$race <- factor(nlsy97$race, levels=c("Non-Black / Non-Hispanic", "Black", "Hispanic", "Multiracial (Non-Hispanic)"))

# debt_20
nlsy97 <- nlsy97 %>%
  rename(debt_20 = CVC_ASSETS_DEBTS_20_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_20_XRND==Year)) %>% # adjusting for inflation
  mutate(debt_20 = debt_20*Buying.Power) %>%
  select(-Buying.Power)

# debt_25
nlsy97 <- nlsy97 %>%
  rename(debt_25 = CVC_ASSETS_DEBTS_25_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_25_XRND==Year)) %>% # adjusting for inflation
  mutate(debt_25 = debt_25*Buying.Power) %>%
  select(-Buying.Power)

# debt_30
nlsy97 <- nlsy97 %>%
  rename(debt_30 = CVC_ASSETS_DEBTS_30_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_30_XRND==Year)) %>% # adjusting for inflation
  mutate(debt_30 = debt_30*Buying.Power) %>%
  select(-Buying.Power)

# debt_35
nlsy97 <- nlsy97 %>%
  rename(debt_35 = CVC_ASSETS_DEBTS_35_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_35_XRND==Year)) %>% # adjusting for inflation
  mutate(debt_35 = debt_35*Buying.Power) %>%
  select(-Buying.Power)

# debt_40
nlsy97 <- nlsy97 %>%
  rename(debt_40 = CVC_ASSETS_DEBTS_40_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_40_XRND==Year)) %>% # adjusting for inflation
  mutate(debt_40 = debt_40*Buying.Power) %>%
  select(-Buying.Power)

# age
nlsy97 <- nlsy97 %>%
  mutate(age_20 = CVC_ASSETS_RND_20_XRND - KEY_BDATE_Y_1997,
         age_25 = CVC_ASSETS_RND_25_XRND - KEY_BDATE_Y_1997,
         age_30 = CVC_ASSETS_RND_30_XRND - KEY_BDATE_Y_1997,
         age_35 = CVC_ASSETS_RND_35_XRND - KEY_BDATE_Y_1997,
         age_40 = CVC_ASSETS_RND_40_XRND - KEY_BDATE_Y_1997)

# education
nlsy97$educ_20 <- NA # creating variable
for (i in 1:nrow(nlsy97)) { # for every respondent
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i] # grab the year respondent was administered YAST20
  if (!is.na(round)) {
    nlsy97$educ_20[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,] # grab education from same round
    # Up to (and including) 2007, the highest grade completed variable was created for all respondents. After 2007, the variable was only created for
    # respondents that were enrolled at the date of the last interview. After 2007, if a valid value exists for YSCH_3112_YYYY (implying the respondent was enrolled at the date
    # of the last interview), the respondent is assigned this value for education. Otherwise, the respondent is assigned the last non-null value of YSCH_3112_YYY.
    if (round > 2007 & is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
      while(is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
        if (round == 1998) {
          break
        } else if (round >= 2013) {
          round <- round - 2
        } else {
          round <- round - 1
        }
      }
      nlsy97$educ_20[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,]
    }
  }
}
nlsy97$educ_20[nlsy97$educ_20 > 90] <- NA

nlsy97$educ_25 <- NA # creating variable
for (i in 1:nrow(nlsy97)) { # for every respondent
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i] # grab the year respondent was administered YAST25
  if (!is.na(round)) {
    nlsy97$educ_25[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,] # grab education from same round
    # Up to (and including) 2007, the highest grade completed variable was created for all respondents. After 2007, the variable was only created for
    # respondents that were enrolled at the date of the last interview. After 2007, if a valid value exists for YSCH_3112_YYYY (implying the respondent was enrolled at the date
    # of the last interview), the respondent is assigned this value for education. Otherwise, the respondent is assigned the last non-null value of YSCH_3112_YYY.
    if (round > 2007 & is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
      while(is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
        if (round == 1998) {
          break
        } else if (round >= 2013) {
          round <- round - 2
        } else {
          round <- round - 1
        }
      }
      nlsy97$educ_25[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,]
    }
  }
}
nlsy97$educ_25[nlsy97$educ_25 > 90] <- NA

nlsy97$educ_30 <- NA # creating variable
for (i in 1:nrow(nlsy97)) { # for every respondent
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i] # grab the year respondent was administered YAST30
  if (!is.na(round)) {
    nlsy97$educ_30[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,] # grab education from same round
    # Up to (and including) 2007, the highest grade completed variable was created for all respondents. After 2007, the variable was only created for
    # respondents that were enrolled at the date of the last interview. After 2007, if a valid value exists for YSCH_3112_YYYY (implying the respondent was enrolled at the date
    # of the last interview), the respondent is assigned this value for education. Otherwise, the respondent is assigned the last non-null value of YSCH_3112_YYY.
    if (round > 2007 & is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
      while(is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
        if (round == 1998) {
          break
        } else if (round >= 2013) {
          round <- round - 2
        } else {
          round <- round - 1
        }
      }
      nlsy97$educ_30[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,]
    }
  }
}
nlsy97$educ_30[nlsy97$educ_30 > 90] <- NA

nlsy97$educ_35 <- NA # creating variable
for (i in 1:nrow(nlsy97)) { # for every respondent
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i] # grab the year respondent was administered YAST35
  if (!is.na(round)) {
    nlsy97$educ_35[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,] # grab education from same round
    # Up to (and including) 2007, the highest grade completed variable was created for all respondents. After 2007, the variable was only created for
    # respondents that were enrolled at the date of the last interview. After 2007, if a valid value exists for YSCH_3112_YYYY (implying the respondent was enrolled at the date
    # of the last interview), the respondent is assigned this value for education. Otherwise, the respondent is assigned the last non-null value of YSCH_3112_YYY.
    if (round > 2007 & is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
      while(is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
        if (round == 1998) {
          break
        } else if (round >= 2013) {
          round <- round - 2
        } else {
          round <- round - 1
        }
      }
      nlsy97$educ_35[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,]
    }
  }
}
nlsy97$educ_35[nlsy97$educ_35 > 90] <- NA

nlsy97$educ_40 <- NA # creating variable
for (i in 1:nrow(nlsy97)) { # for every respondent
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i] # grab the year respondent was administered YAST40
  if (!is.na(round)) {
    nlsy97$educ_40[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,] # grab education from same round
    # Up to (and including) 2007, the highest grade completed variable was created for all respondents. After 2007, the variable was only created for
    # respondents that were enrolled at the date of the last interview. After 2007, if a valid value exists for YSCH_3112_YYYY (implying the respondent was enrolled at the date
    # of the last interview), the respondent is assigned this value for education. Otherwise, the respondent is assigned the last non-null value of YSCH_3112_YYY.
    if (round > 2007 & is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
      while(is.na(select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,])) {
        if (round == 1998) {
          break
        } else if (round >= 2013) {
          round <- round - 2
        } else {
          round <- round - 1
        }
      }
      nlsy97$educ_40[i] <- select(nlsy97, starts_with("YSCH") & ends_with(as.character(round)))[i,]
    }
  }
}
nlsy97$educ_40[nlsy97$educ_40 > 90] <- NA

# income
nlsy97$income20 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i] # select the round respondent was administered YAST20
  if (!is.na(round)) {
    nlsy97$income20[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
                                                                                                        # administered YAST20
    nlsy97$income20[i] <- nlsy97$income20[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}
nlsy97$income20 <- nlsy97$income20 / 1000 # in $1000s

nlsy97$income25 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i] # select the round respondent was administered YAST25
  if (!is.na(round)) {
    nlsy97$income25[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST25
    nlsy97$income25[i] <- nlsy97$income25[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}
nlsy97$income25 <- nlsy97$income25 / 1000 # in $1000s

nlsy97$income30 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i] # select the round respondent was administered YAST30
  if (!is.na(round)) {
    nlsy97$income30[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST30
    nlsy97$income30[i] <- nlsy97$income30[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}
nlsy97$income30 <- nlsy97$income30 / 1000 # in $1000s

nlsy97$income35 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i] # select the round respondent was administered YAST35
  if (!is.na(round)) {
    nlsy97$income35[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST35
    nlsy97$income35[i] <- nlsy97$income35[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}
nlsy97$income35 <- nlsy97$income35 / 1000 # in $1000s

nlsy97$income40 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i] # select the round respondent was administered YAST40
  if (!is.na(round)) {
    nlsy97$income40[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST40
    nlsy97$income40[i] <- nlsy97$income40[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}
nlsy97$income40 <- nlsy97$income40 / 1000 # in $1000s

# assets
nlsy97$assets20 <- nlsy97$CVC_ASSETS_FINANCIAL_20_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_20_XRND
for (i in 1:length(nlsy97$assets20)) {
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i]
  if (!is.na(round)) { nlsy97$assets20[i] <- nlsy97$assets20[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}
nlsy97$assets20 <- nlsy97$assets20 / 1000 # in $1000s

nlsy97$assets25 <- nlsy97$CVC_ASSETS_FINANCIAL_25_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_25_XRND
for (i in 1:length(nlsy97$assets25)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i]
  if (!is.na(round)) { nlsy97$assets25[i] <- nlsy97$assets25[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}
nlsy97$assets25 <- nlsy97$assets25 / 1000 # in $1000s

nlsy97$assets30 <- nlsy97$CVC_ASSETS_FINANCIAL_30_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_30_XRND
for (i in 1:length(nlsy97$assets30)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i]
  if (!is.na(round)) { nlsy97$assets30[i] <- nlsy97$assets30[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}
nlsy97$assets30 <- nlsy97$assets30 / 1000 # in $1000s

nlsy97$assets35 <- nlsy97$CVC_ASSETS_FINANCIAL_35_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_35_XRND
for (i in 1:length(nlsy97$assets35)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i]
  if (!is.na(round)) { nlsy97$assets35[i] <- nlsy97$assets35[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}
nlsy97$assets35 <- nlsy97$assets35 / 1000 # in $1000s

nlsy97$assets40 <- nlsy97$CVC_ASSETS_FINANCIAL_40_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_40_XRND
for (i in 1:length(nlsy97$assets40)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i]
  if (!is.na(round)) { nlsy97$assets40[i] <- nlsy97$assets40[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}
nlsy97$assets40 <- nlsy97$assets40 / 1000 # in $1000s

# marital_status

  # Creating 2-level factor ("Not Married", "Married")
  marstat_columns <- select(nlsy97, starts_with("CV_MARSTAT"))
  marstat_columns <- apply(marstat_columns, 2, as.character)
  marstat_columns <- ifelse(marstat_columns == "1", "Married", marstat_columns)
  marstat_columns <- ifelse(marstat_columns != "Married" & !is.na(marstat_columns), "Not Married", marstat_columns)
  marstat_columns <- apply(marstat_columns, 2, factor, levels=c("Not Married", "Married"))
  marstat_columns <- data.frame(marstat_columns)

nlsy97 <- nlsy97 %>%
  select(-starts_with("CV_MARSTAT")) %>% # removing unclean columns
  bind_cols(marstat_columns) # joining in cleaned columns

nlsy97$marital_status_20 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i] # selecting round respondent was administered YAST20
  if (!is.na(round)) { nlsy97$marital_status_20[i] <- select(nlsy97, starts_with("CV_MARSTAT") & ends_with(as.character(round)))[i,] } # marital_status is marital status from round
                                                                                                                                # respondent was administered YAST20
}

nlsy97$marital_status_25 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i] # selecting round respondent was administered YAST25
  if (!is.na(round)) { nlsy97$marital_status_25[i] <- select(nlsy97, starts_with("CV_MARSTAT") & ends_with(as.character(round)))[i,] } # marital_status is marital status from round
  # respondent was administered YAST25
}

nlsy97$marital_status_30 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i] # selecting round respondent was administered YAST30
  if (!is.na(round)) { nlsy97$marital_status_30[i] <- select(nlsy97, starts_with("CV_MARSTAT") & ends_with(as.character(round)))[i,] } # marital_status is marital status from round
  # respondent was administered YAST30
}

nlsy97$marital_status_35 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i] # selecting round respondent was administered YAST35
  if (!is.na(round)) { nlsy97$marital_status_35[i] <- select(nlsy97, starts_with("CV_MARSTAT") & ends_with(as.character(round)))[i,] } # marital_status is marital status from round
  # respondent was administered YAST35
}

nlsy97$marital_status_40 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i] # selecting round respondent was administered YAST40
  if (!is.na(round)) { nlsy97$marital_status_40[i] <- select(nlsy97, starts_with("CV_MARSTAT") & ends_with(as.character(round)))[i,] } # marital_status is marital status from round
  # respondent was administered YAST40
}

# num_children

  # cleaning
  nlsy97 <- nlsy97 %>%
    rename(CV_BIO_CHILD_NR_2019 = CV_BIO_CHILD_NR_U18_2019,
           CV_BIO_CHILD_HH_2019 = CV_BIO_CHILD_HH_U18_2019,
           CV_BIO_CHILD_NR_2021 = CV_BIO_CHILD_NR_U18_2021,
           CV_BIO_CHILD_HH_2021 = CV_BIO_CHILD_HH_U18_2021)
  
  num_children_columns <- nlsy97 %>%
    select(starts_with("CV_BIO_CHILD"))

  # respondents without any biological children were assigned a valid skip ("NA") - recoding to 0
  for (j in 1:ncol(num_children_columns)) {
    num_children_columns[,j] <- ifelse(is.na(num_children_columns[,j]), 0, num_children_columns[,j])
  }

  nlsy97 <- nlsy97 %>%
    select(-starts_with("CV_BIO_CHILD")) %>% # removing unclean columns
    bind_cols(num_children_columns) # joining in clean columns

nlsy97$num_children_20 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i] # selecting round respondent was administered YAST20
  if (!is.na(round)) {
    # num_children = total number of biological children (household + non-household) in round respondent was administered YAST20
    nlsy97$num_children_20[i] <- select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("HH_", as.character(round))))[i,] +
      select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("NR_", as.character(round))))[i,]
  }
}

nlsy97$num_children_25 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i] # selecting round respondent was administered YAST25
  if (!is.na(round)) {
    # num_children = total number of biological children (household + non-household) in round respondent was administered YAST25
    nlsy97$num_children_25[i] <- select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("HH_", as.character(round))))[i,] +
      select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("NR_", as.character(round))))[i,]
  }
}

nlsy97$num_children_30 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i] # selecting round respondent was administered YAST30
  if (!is.na(round)) {
    # num_children = total number of biological children (household + non-household) in round respondent was administered YAST30
    nlsy97$num_children_30[i] <- select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("HH_", as.character(round))))[i,] +
      select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("NR_", as.character(round))))[i,]
  }
}

nlsy97$num_children_35 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i] # selecting round respondent was administered YAST35
  if (!is.na(round)) {
    # num_children = total number of biological children (household + non-household) in round respondent was administered YAST35
    nlsy97$num_children_35[i] <- select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("HH_", as.character(round))))[i,] +
      select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("NR_", as.character(round))))[i,]
  }
}

nlsy97$num_children_40 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i] # selecting round respondent was administered YAST40
  if (!is.na(round)) {
    # num_children = total number of biological children (household + non-household) in round respondent was administered YAST40
    nlsy97$num_children_40[i] <- select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("HH_", as.character(round))))[i,] +
      select(nlsy97, starts_with("CV_BIO_CHILD") & ends_with(paste0("NR_", as.character(round))))[i,]
  }
}

# VSTRAT
nlsy97 <- nlsy97 %>%
  rename(VSTRAT = VSTRAT_1997)

# VPSU
nlsy97 <- nlsy97 %>%
  rename(VPSU = VPSU_1997)

# Selecting variables of interest
nlsy97 <- nlsy97 %>%
  select(id,
         sex,
         parent_income,
         parent_net_worth,
         parent_education,
         race,
         debt_20,
         debt_25,
         debt_30,
         debt_35,
         debt_40,
         age_20,
         age_25,
         age_30,
         age_35,
         age_40,
         educ_20,
         educ_25,
         educ_30,
         educ_35,
         educ_40,
         income20,
         income25,
         income30,
         income35,
         income40,
         assets20,
         assets25,
         assets30,
         assets35,
         assets40,
         marital_status_20,
         marital_status_25,
         marital_status_30,
         marital_status_35,
         marital_status_40,
         num_children_20,
         num_children_25,
         num_children_30,
         num_children_35,
         num_children_40,
         VSTRAT,
         VPSU)

# Imputation
set.seed(1997)

  # Converting marital status (across all ages) to a {0, 1} numeric because imputation can't handle factor variables
  for (j in which(startsWith(colnames(nlsy97), "marital"))) { 
    nlsy97[,j] <- ifelse(nlsy97[,j]=="Married", "1", nlsy97[,j])
    nlsy97[,j] <- ifelse(nlsy97[,j]=="Not Married", "0", nlsy97[,j])
    nlsy97[,j] <- as.numeric(nlsy97[,j])
  }

impute <- preProcess(nlsy97, method=c("bagImpute")) # imputation using bagging (decision tree)
predictors <- predict(impute, nlsy97)
predictors <- predictors %>%
  select(-starts_with("debt")) # removing imputed dependent variable

  # If probability of being married >= 50% then "Married", if < 50% then "Not Married"
  for (j in which(startsWith(colnames(predictors), "marital"))) {
    predictors[,j] <- ifelse(predictors[,j] >= 0.5, 1, 0)
    predictors[,j] <- as.character(predictors[,j])
    predictors[,j] <- ifelse(predictors[,j]=="1", "Married", "Not Married")
    predictors[,j] <- factor(predictors[,j], levels=c("Not Married", "Married"))
  }

  # Fixing nonsensical imputed values 
  predictors$parent_income <- ifelse(predictors$parent_income < 0, 0, predictors$parent_income)
  for (j in which(startsWith(colnames(predictors), "assets"))) {
    predictors[,j] <- ifelse(predictors[,j] < 0, 0, predictors[,j])
  }
  for (j in which(startsWith(colnames(predictors), "parent_education"))) {
    predictors[,j] <- round(predictors[,j])
  }
  for (j in which(startsWith(colnames(predictors), "educ"))) {
    predictors[,j] <- round(predictors[,j])
  }
  for (j in which(startsWith(colnames(predictors), "num_children"))) {
    predictors[,j] <- round(predictors[,j])
  }

nlsy97 <- nlsy97 %>%
  select(starts_with("debt")) %>% # select unimputed dependent variable
  bind_cols(predictors) %>% # join in imputed independent variables and control variables
  relocate(id)

# Restricting data
total_debt <- nlsy97 %>% # respondents with debt at at least one measurement interval
  filter(debt_20 > 0 |
           debt_25 > 0 |
           debt_30 > 0 |
           debt_35 > 0 |
           debt_40 > 0)

# Pivoting longer

  # Time variant control variables need to be separated, pivoted, and then joined back in
  # Function to separate and pivot time variant controls
  # keywords = "age", "educ", "income", "assets", "marital_status", "num_children"
  pivot_tvc <- function(keyword) {
    tvc <- select(nlsy97, id, starts_with(keyword))
    tvc <- tvc %>%
      pivot_longer(!id,
                   values_to=keyword,
                   names_to="time")
    tvc$time[tvc$time==paste0(keyword, "_20")] <- 0
    tvc$time[tvc$time==paste0(keyword, "_25")] <- 1
    tvc$time[tvc$time==paste0(keyword, "_30")] <- 2
    tvc$time[tvc$time==paste0(keyword, "_35")] <- 3
    tvc$time[tvc$time==paste0(keyword, "_40")] <- 4
    tvc$time[tvc$time==paste0(keyword, "20")] <- 0
    tvc$time[tvc$time==paste0(keyword, "25")] <- 1
    tvc$time[tvc$time==paste0(keyword, "30")] <- 2
    tvc$time[tvc$time==paste0(keyword, "35")] <- 3
    tvc$time[tvc$time==paste0(keyword, "40")] <- 4
    return(tvc)
  }
  age <- pivot_tvc("age")
  educ <- pivot_tvc("educ")
  income <- pivot_tvc("income")
  assets <- pivot_tvc("assets")
  marital_status <- pivot_tvc("marital_status")
  num_children <- pivot_tvc("num_children")
  
  # total_debt
  total_debt <- total_debt %>%
    select(-starts_with("age"), # removing time variant controls
           -starts_with("educ"),
           -starts_with("income"),
           -starts_with("assets"),
           -starts_with("marital_status"),
           -starts_with("num_children"))
  total_debt <- total_debt %>%
    pivot_longer(cols=starts_with("debt"),
                 values_to="debt",
                 names_to="time")
  total_debt$time[total_debt$time=="debt_20"] <- 0
  total_debt$time[total_debt$time=="debt_25"] <- 1
  total_debt$time[total_debt$time=="debt_30"] <- 2
  total_debt$time[total_debt$time=="debt_35"] <- 3
  total_debt$time[total_debt$time=="debt_40"] <- 4
  
  total_debt <- total_debt %>% # joining in time variant controls
    inner_join(age, by=c("id", "time")) %>%
    inner_join(educ, by=c("id", "time")) %>%
    inner_join(income, by=c("id", "time")) %>%
    inner_join(assets, by=c("id", "time")) %>%
    inner_join(marital_status, by=c("id", "time")) %>%
    inner_join(num_children, by=c("id", "time"))
  total_debt$time <- as.numeric(total_debt$time)
  
  total_debt <- total_debt %>%
    relocate(id,
             debt)
  
total_debt <- na.omit(total_debt) # removing respondent-measurements with NA values for debt (no other missing values due to imputation)
  
# weight
weights <- read.table("Custom Weights.dat")
total_debt <- total_debt %>%
  inner_join(weights, by=c("id"="V1")) %>% # joining in weights
  rename(weight = V2) %>%
  mutate(weight = weight / sum(weight)) # standardizing weights
  
# Creating variables for model
  
  # debt_logged (dependent variable)
  total_debt$debt_logged <- ifelse(total_debt$debt == 0, 1, total_debt$debt) # giving $1 to respondents with $0 in debt so they're not dropped when taking log
  total_debt$debt_logged <- log(total_debt$debt_logged)

total_debt$time_sq <- total_debt$time^2 # quadratic growth term

  # Indicator for having debt
  total_debt$has_debt <- ifelse(total_debt$debt > 0, 1, 0) 
  total_debt$has_debt <- factor(total_debt$has_debt, levels=c(0, 1))

save(total_debt, file="total_debt.RData")
