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
  mutate(parent_income = parent_income * filter(inflation, Year==1997)$Buying.Power) # adjusting for inflation

# parent_net_worth
nlsy97 <- nlsy97 %>% 
  rename(parent_net_worth = CV_HH_NET_WORTH_P_1997) %>%
  mutate(parent_net_worth = parent_net_worth * filter(inflation, Year==1997)$Buying.Power) # adjusting for inflation

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

# student_debt_20
nlsy97$student_debt_20 <- 0
nlsy97$student_debt_20[is.na(nlsy97$CVC_ASSETS_RND_20_XRND)] <- NA

  # YAST20 student debt showcard 2003 (recoded to midpoint)
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==1] <- 500
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==2] <- 1750
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==3] <- 3750
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==4] <- 7500
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==5] <- 17500
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==6] <- 37500
  nlsy97$`YAST-5018_2003`[nlsy97$`YAST-5018_2003`==7] <- 50000 # "More than $50,000" coded to $50,000
  
  # YAST20 student debt showcard 2004 (recoded to midpoint)
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==2] <- 1750
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==3] <- 3750
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==4] <- 7500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==5] <- 17500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==6] <- 37500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==7] <- 50000 # "More than $50,000" coded to $50,000
  
  # YAST20 student debt showcard 2005 (recoded to midpoint)
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==1] <- 500
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==2] <- 1750
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==3] <- 3750
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==4] <- 7500
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==5] <- 17500
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==6] <- 37500
  nlsy97$`YAST-5018_2005`[nlsy97$`YAST-5018_2005`==7] <- 50000 # "More than $50,000" coded to $50,000

for (i in 1:nrow(nlsy97)) {
  if (!is.na(nlsy97$`YAST-5016_2003`[i])) {
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5016_2003`[i] 
  } else if (!is.na(nlsy97$`YAST-5016_2004`[i])) {
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5016_2004`[i] 
  } else if (!is.na(nlsy97$`YAST-5016_2005`[i])) {
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5016_2005`[i] 
  } else if (!is.na(nlsy97$`YAST-5016_2006`[i])) {
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5016_2006`[i] 
  } else if (!is.na(nlsy97$`YAST-5016_2007`[i])) {
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5016_2007`[i] 
  } else if (!is.na(nlsy97$`YAST-5017~000001_2003`[i])) { # respondents answered w/ range
    nlsy97$student_debt_20[i] <- (nlsy97$`YAST-5017~000001_2003`[i] + nlsy97$`YAST-5017~000002_2003`[i]) / 2 
  } else if (!is.na(nlsy97$`YAST-5017_000001_2004`[i])) { # respondents answered w/ range
    nlsy97$student_debt_20[i] <- (nlsy97$`YAST-5017_000001_2004`[i] + nlsy97$`YAST-5017_000002_2004`[i]) / 2 
  } else if (!is.na(nlsy97$`YAST-5017_000001_2005`[i])) { # respondents answered w/ range
    nlsy97$student_debt_20[i] <- (nlsy97$`YAST-5017_000001_2005`[i] + nlsy97$`YAST-5017_000002_2005`[i]) / 2 
  } else if (!is.na(nlsy97$`YAST-5018_2003`[i])) { # respondents answered with showcard
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5018_2003`[i]
  } else if (!is.na(nlsy97$`YAST-5018_2004`[i])) { # respondents answered with showcard
    nlsy97$student_debt_20[i] <- nlsy97$`YAST-5018_2004`[i]
  } else if (!is.na(nlsy97$`YAST-5018_2005`[i])) { # respondents answered with showcard
    nlsy97$student_debt[i] <- nlsy97$`YAST-5018_2005`[i]
  }
}

nlsy97 <- nlsy97 %>% # adjusting for inflation
  left_join(inflation, by=join_by(CVC_ASSETS_RND_20_XRND==Year)) %>%
  mutate(student_debt_20 = student_debt_20*Buying.Power) %>%
  select(-Buying.Power)

# student_debt_25
nlsy97$student_debt_25 <- 0
nlsy97$student_debt_25[is.na(nlsy97$CVC_ASSETS_RND_25_XRND)] <- NA

  # YAST25 student debt showcard (recoded to midpoint)
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==1] <- 500
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==2] <- 1750
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==3] <- 3750
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==4] <- 7500
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==5] <- 17500
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==6] <- 37500
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==7] <- 75000
  nlsy97$`YAST25-5018_COMB_XRND`[nlsy97$`YAST25-5018_COMB_XRND`==8] <- 100000 # Recoded "More than $100,000" to $100,000
  
for (i in 1:nrow(nlsy97)) {
  if (!is.na(nlsy97$`YAST25-5016_COMB_XRND`[i])) {
    nlsy97$student_debt_25[i] <- nlsy97$`YAST25-5016_COMB_XRND`[i]
  } else if (!is.na(nlsy97$`YAST25-5017_000001_COMB_XRND`[i])) { # respondents answered w/ range
    nlsy97$student_debt_25[i] <- (nlsy97$`YAST25-5017_000001_COMB_XRND`[i] + nlsy97$`YAST25-5017_000002_COMB_XRND`[i]) / 2 
  } else if (!is.na(nlsy97$`YAST25-5018_COMB_XRND`[i])) { # respondents answered with showcard
    nlsy97$student_debt_25[i] <- nlsy97$`YAST25-5018_COMB_XRND`[i]
  }
}

nlsy97 <- nlsy97 %>% # adjusting for inflation
  left_join(inflation, by=join_by(CVC_ASSETS_RND_25_XRND==Year)) %>%
  mutate(student_debt_25 = student_debt_25*Buying.Power) %>%
  select(-Buying.Power)

# student_debt_30
nlsy97$student_debt_30 <- 0
nlsy97$student_debt_30[is.na(nlsy97$CVC_ASSETS_RND_30_XRND)] <- NA

  # YAST30 student debt showcard (recoded to midpoint)
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==1] <- 500
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==2] <- 1750
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==3] <- 3750
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==4] <- 7500
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==5] <- 17500
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==6] <- 37500
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==7] <- 75000
  nlsy97$`YAST30-5018_COMB_XRND`[nlsy97$`YAST30-5018_COMB_XRND`==8] <- 100000 # Recoded "More than $100,000" to $100,000
  
for (i in 1:nrow(nlsy97)) {
  if (!is.na(nlsy97$`YAST30-5016_COMB_XRND`[i])) {
    nlsy97$student_debt_30[i] <- nlsy97$`YAST30-5016_COMB_XRND`[i]
  } else if (!is.na(nlsy97$`YAST30-5017_000001_COMB_XRND`[i])) { # respondents answered w/ range
    nlsy97$student_debt_30[i] <- (nlsy97$`YAST30-5017_000001_COMB_XRND`[i] + nlsy97$`YAST30-5017_000002_COMB_XRND`[i]) / 2
  } else if (!is.na(nlsy97$`YAST30-5018_COMB_XRND`[i])) { # respondents answered with showcard
    nlsy97$student_debt_30[i] <- nlsy97$`YAST30-5018_COMB_XRND`[i]
  }
}
  
nlsy97 <- nlsy97 %>% # adjusting for inflation
  left_join(inflation, by=join_by(CVC_ASSETS_RND_30_XRND==Year)) %>%
  mutate(student_debt_30 = student_debt_30*Buying.Power) %>%
  select(-Buying.Power)
  
# student_debt_35
nlsy97$student_debt_35 <- 0
nlsy97$student_debt_35[is.na(nlsy97$CVC_ASSETS_RND_35_XRND)] <- NA

  # YAST35 student debt showcard (recoded to midpoint)
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==1] <- 500
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==2] <- 1750
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==3] <- 3750
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==4] <- 7500
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==5] <- 17500
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==6] <- 37500
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==7] <- 75000
  nlsy97$`YAST35-5018_COMB_XRND`[nlsy97$`YAST35-5018_COMB_XRND`==8] <- 10000 # Coded "More than $100,000" as 100,000
  
for (i in 1:nrow(nlsy97)) {
  if (!is.na(nlsy97$`YAST35-5016_COMB_XRND`[i])) {
    nlsy97$student_debt_35[i] <- nlsy97$`YAST35-5016_COMB_XRND`[i]
  } else if (!is.na(nlsy97$`YAST35-5017_000001_COMB_XRND`[i])) { # respondents answered w/ range
    nlsy97$student_debt_35[i] <- (nlsy97$`YAST35-5017_000001_COMB_XRND`[i] + nlsy97$`YAST35-5017_000002_COMB_XRND`[i]) / 2
  } else if (!is.na(nlsy97$`YAST35-5018_COMB_XRND`[i])) { # respondents answered with showcard
    nlsy97$student_debt_35[i] <- nlsy97$`YAST35-5018_COMB_XRND`[i]
  }
}

nlsy97 <- nlsy97 %>% # adjusting for inflation
  left_join(inflation, by=join_by(CVC_ASSETS_RND_35_XRND==Year)) %>%
  mutate(student_debt_35 = student_debt_35*Buying.Power) %>%
  select(-Buying.Power)
  
# student_debt_40
nlsy97$student_debt_40 <- 0
nlsy97$student_debt_40[is.na(nlsy97$CVC_ASSETS_RND_40_XRND)] <- NA

  # Recoding YAST40 student debt showcard
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==1] <- 500
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==2] <- 1750
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==3] <- 3750
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==4] <- 7500
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==5] <- 17500
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==6] <- 37500
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==7] <- 75000
  nlsy97$`YAST40-5018_COMB_XRND`[nlsy97$`YAST40-5018_COMB_XRND`==8] <- 100000 # Recoding "More than $100,000" to $100,000
  
for (i in 1:nrow(nlsy97)) {
  if (!is.na(nlsy97$`YAST40-5016_COMB_XRND`[i])) {
    nlsy97$student_debt_40[i] <- nlsy97$`YAST40-5016_COMB_XRND`[i]
  } else if (!is.na(nlsy97$`YAST40-5017_000001_COMB_XRND`[i])) { # respondents answered w/ range
    nlsy97$student_debt_40[i] <- (nlsy97$`YAST40-5017_000001_COMB_XRND`[i] + nlsy97$`YAST40-5017_000002_COMB_XRND`[i]) / 2
  } else if (!is.na(nlsy97$`YAST40-5018_COMB_XRND`[i])) { # respondents answered with showcard
    nlsy97$student_debt_40[i] <- nlsy97$`YAST40-5018_COMB_XRND`[i]
  }
}
  
nlsy97 <- nlsy97 %>% # adjusting for inflation
  left_join(inflation, by=join_by(CVC_ASSETS_RND_40_XRND==Year)) %>%
  mutate(student_debt_40 = student_debt_40*Buying.Power) %>%
  select(-Buying.Power)

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

nlsy97$income25 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i] # select the round respondent was administered YAST25
  if (!is.na(round)) {
    nlsy97$income25[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST25
    nlsy97$income25[i] <- nlsy97$income25[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}

nlsy97$income30 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i] # select the round respondent was administered YAST30
  if (!is.na(round)) {
    nlsy97$income30[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST30
    nlsy97$income30[i] <- nlsy97$income30[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}

nlsy97$income35 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i] # select the round respondent was administered YAST35
  if (!is.na(round)) {
    nlsy97$income35[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST35
    nlsy97$income35[i] <- nlsy97$income35[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}

nlsy97$income40 <- NA
for (i in 1:nrow(nlsy97)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i] # select the round respondent was administered YAST40
  if (!is.na(round)) {
    nlsy97$income40[i] <- select(nlsy97, starts_with("CV_Income") & ends_with(as.character(round)))[i,] # income is the income reported in round respondent was
    # administered YAST40
    nlsy97$income40[i] <- nlsy97$income40[i]*filter(inflation, Year==round)$Buying.Power # adjusting for inflation (2024 $)
  }
}

# assets
nlsy97$assets20 <- nlsy97$CVC_ASSETS_FINANCIAL_20_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_20_XRND
for (i in 1:length(nlsy97$assets20)) {
  round <- nlsy97$CVC_ASSETS_RND_20_XRND[i]
  if (!is.na(round)) { nlsy97$assets20[i] <- nlsy97$assets20[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}

nlsy97$assets25 <- nlsy97$CVC_ASSETS_FINANCIAL_25_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_25_XRND
for (i in 1:length(nlsy97$assets25)) {
  round <- nlsy97$CVC_ASSETS_RND_25_XRND[i]
  if (!is.na(round)) { nlsy97$assets25[i] <- nlsy97$assets25[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}

nlsy97$assets30 <- nlsy97$CVC_ASSETS_FINANCIAL_30_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_30_XRND
for (i in 1:length(nlsy97$assets30)) {
  round <- nlsy97$CVC_ASSETS_RND_30_XRND[i]
  if (!is.na(round)) { nlsy97$assets30[i] <- nlsy97$assets30[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}

nlsy97$assets35 <- nlsy97$CVC_ASSETS_FINANCIAL_35_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_35_XRND
for (i in 1:length(nlsy97$assets35)) {
  round <- nlsy97$CVC_ASSETS_RND_35_XRND[i]
  if (!is.na(round)) { nlsy97$assets35[i] <- nlsy97$assets35[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}

nlsy97$assets40 <- nlsy97$CVC_ASSETS_FINANCIAL_40_XRND + nlsy97$CVC_ASSETS_NONFINANCIAL_40_XRND
for (i in 1:length(nlsy97$assets40)) {
  round <- nlsy97$CVC_ASSETS_RND_40_XRND[i]
  if (!is.na(round)) { nlsy97$assets40[i] <- nlsy97$assets40[i]*filter(inflation, Year==round)$Buying.Power } # adjusting for inflation
}

# marital_status

  # Creating 2-level factor ("Married", "Not Married")
  marstat_columns <- select(nlsy97, starts_with("CV_MARSTAT"))
  marstat_columns <- apply(marstat_columns, 2, as.character)
  marstat_columns <- ifelse(marstat_columns == "1", "Married", marstat_columns)
  marstat_columns <- ifelse(marstat_columns != "Married" & !is.na(marstat_columns), "Not Married", marstat_columns)
  marstat_columns <- apply(marstat_columns, 2, factor, levels=c("Married", "Not Married"))
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
  
# weight
weights <- read.table("Custom Weights.dat")
nlsy97 <- nlsy97 %>%
  inner_join(weights, by=c("id"="V1")) %>% # joining in weights
  rename(weight = V2) %>%
  mutate(weight = weight / sum(weight)) # standardizing weights

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
         student_debt_20,
         student_debt_25,
         student_debt_30,
         student_debt_35,
         student_debt_40,
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
         weight,
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

cols_impute <- c("sex", # columns to impute
                 "race",
                 "age_20",
                 "age_25",
                 "age_30",
                 "age_35",
                 "age_40",
                 "educ_20",
                 "educ_25",
                 "educ_30",
                 "educ_35",
                 "educ_40",
                 "income20",
                 "income25",
                 "income30",
                 "income35",
                 "income40",
                 "assets20",
                 "assets25",
                 "assets30",
                 "assets35",
                 "assets40",
                 "marital_status_20",
                 "marital_status_25",
                 "marital_status_30",
                 "marital_status_35",
                 "marital_status_40",
                 "num_children_20",
                 "num_children_25",
                 "num_children_30",
                 "num_children_35",
                 "num_children_40")
cols_impute_index <- which(colnames(nlsy97) %in% cols_impute)
impute <- preProcess(nlsy97[,cols_impute_index], method=c("bagImpute")) # imputation using bagging (decision tree)
predictors <- predict(impute, nlsy97[,cols_impute_index])

# If probability of being married >= 50% then "Married", if < 50% then "Not Married"
for (j in which(startsWith(colnames(predictors), "marital"))) {
  predictors[,j] <- ifelse(predictors[,j] >= 0.5, 1, 0)
  predictors[,j] <- as.character(predictors[,j])
  predictors[,j] <- ifelse(predictors[,j]=="1", "Married", "Not Married")
  predictors[,j] <- factor(predictors[,j], levels=c("Married", "Not Married"))
}

cols_not_imputed <- nlsy97[,which(!(colnames(nlsy97) %in% cols_impute))]
nlsy97 <- cols_not_imputed %>% # dropping non-imputed columns and joining in imputed columns
  bind_cols(predictors) 

# Pivoting longer

  # Time variant control variables need to be separated, pivoted, and then joined back in
  # Function to separate and pivot time variant controls
  # keyword = "age", "educ", "income", "assets", "marital_status", "num_children"
  pivot_tvc <- function(keyword) {
    tvc <- select(nlsy97, id, starts_with(keyword))
    tvc <- tvc %>%
      pivot_longer(!id,
                   values_to=keyword,
                   names_to="time")
    tvc$time[tvc$time==paste0(keyword, "_20")] <- 20
    tvc$time[tvc$time==paste0(keyword, "_25")] <- 25
    tvc$time[tvc$time==paste0(keyword, "_30")] <- 30
    tvc$time[tvc$time==paste0(keyword, "_35")] <- 35
    tvc$time[tvc$time==paste0(keyword, "_40")] <- 40
    tvc$time[tvc$time==paste0(keyword, "20")] <- 20
    tvc$time[tvc$time==paste0(keyword, "25")] <- 25
    tvc$time[tvc$time==paste0(keyword, "30")] <- 30
    tvc$time[tvc$time==paste0(keyword, "35")] <- 35
    tvc$time[tvc$time==paste0(keyword, "40")] <- 40
    return(tvc)
  }
  age <- pivot_tvc("age")
  educ <- pivot_tvc("educ")
  income <- pivot_tvc("income")
  assets <- pivot_tvc("assets")
  marital_status <- pivot_tvc("marital_status")
  num_children <- pivot_tvc("num_children")
  
  # total_debt
  total_debt <- nlsy97 %>% # respondents with at least one debt measurement
    filter(debt_20 > 0 |
             debt_25 > 0 |
             debt_30 > 0 |
             debt_35 > 0 |
             debt_40 > 0)
  total_debt <- total_debt %>%
    select(-starts_with("student_debt"), # only interested in total debt for RQ #1 and RQ #2
           # removing time variant controls
           -starts_with("age"),
           -starts_with("educ"),
           -starts_with("income"),
           -starts_with("assets"),
           -starts_with("marital_status"),
           -starts_with("num_children"))
  total_debt <- total_debt %>%
    pivot_longer(cols=starts_with("debt"),
                 values_to="debt",
                 names_to="time")
  total_debt$time[total_debt$time=="debt_20"] <- 20
  total_debt$time[total_debt$time=="debt_25"] <- 25
  total_debt$time[total_debt$time=="debt_30"] <- 30
  total_debt$time[total_debt$time=="debt_35"] <- 35
  total_debt$time[total_debt$time=="debt_40"] <- 40
  
  total_debt <- total_debt %>% # joining in time variant controls
    inner_join(age, by=c("id", "time")) %>%
    inner_join(educ, by=c("id", "time")) %>%
    inner_join(income, by=c("id", "time")) %>%
    inner_join(assets, by=c("id", "time")) %>%
    inner_join(marital_status, by=c("id", "time")) %>%
    inner_join(num_children, by=c("id", "time"))
  
  total_debt <- total_debt %>%
    relocate(id,
             debt)
  
  # student_debt
  student_debt <- nlsy97 %>% # respondents with at least one student debt measurement
    filter(student_debt_20 > 0 |
             student_debt_25 > 0 |
             student_debt_30 > 0 |
             student_debt_35 > 0 |
             student_debt_40 > 0)
  student_debt <- student_debt %>%
    select(-starts_with("total_debt"), # only interested in studnet debt for RQ #3
           # removing time variant controls
           -starts_with("age"),
           -starts_with("educ"),
           -starts_with("income"),
           -starts_with("assets"),
           -starts_with("marital_status"),
           -starts_with("num_children"))
  student_debt <- student_debt %>%
    pivot_longer(cols=starts_with("student_debt"),
                 values_to="debt",
                 names_to="time")
  student_debt$time[student_debt$time=="student_debt_20"] <- 20
  student_debt$time[student_debt$time=="student_debt_25"] <- 25
  student_debt$time[student_debt$time=="student_debt_30"] <- 30
  student_debt$time[student_debt$time=="student_debt_35"] <- 35
  student_debt$time[student_debt$time=="student_debt_40"] <- 40
  
  student_debt <- student_debt %>% # joining in time variant controls
    inner_join(age, by=c("id", "time")) %>%
    inner_join(educ, by=c("id", "time")) %>%
    inner_join(income, by=c("id", "time")) %>%
    inner_join(assets, by=c("id", "time")) %>%
    inner_join(marital_status, by=c("id", "time")) %>%
    inner_join(num_children, by=c("id", "time"))
  
  student_debt <- student_debt %>%
    relocate(id,
             debt)
  
save(total_debt, file="total_debt.RData")
save(student_debt, file="student_debt.RData")

