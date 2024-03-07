rm(list=ls())
library(tidyverse)
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

# yob
nlsy97 <- nlsy97 %>% rename(yob = KEY_BDATE_Y_1997)

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
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 3750
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 7500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 17500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 37500
  nlsy97$`YAST-5018_2004`[nlsy97$`YAST-5018_2004`==1] <- 50000 # "More than $50,000" coded to $50,000
  
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
  left_join(inflation, by=join_by(CVC_ASSETS_RND_25_XRND==Year)) %>%
  mutate(debt_25 = debt_25*Buying.Power) %>%
  select(-Buying.Power)

# debt_30
nlsy97 <- nlsy97 %>%
  rename(debt_30 = CVC_ASSETS_DEBTS_30_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_30_XRND==Year)) %>%
  mutate(debt_30 = debt_30*Buying.Power) %>%
  select(-Buying.Power)

# debt_35
nlsy97 <- nlsy97 %>%
  rename(debt_35 = CVC_ASSETS_DEBTS_35_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_35_XRND==Year)) %>%
  mutate(debt_35 = debt_35*Buying.Power) %>%
  select(-Buying.Power)

# debt_40
nlsy97 <- nlsy97 %>%
  rename(debt_40 = CVC_ASSETS_DEBTS_40_XRND) %>%
  left_join(inflation, by=join_by(CVC_ASSETS_RND_40_XRND==Year)) %>%
  mutate(debt_40 = debt_40*Buying.Power) %>%
  select(-Buying.Power)

# weight
weights <- read.table("Custom Weights.dat")
nlsy97 <- nlsy97 %>%
  inner_join(weights, by=join_by(id==V1)) %>%
  rename(weight = V2) %>%
  mutate(weight = weight/100) # two implied decimal places

# VSTRAT
nlsy97 <- nlsy97 %>%
  rename(VSTRAT = VSTRAT_1997)

# VPSU
nlsy97 <- nlsy97 %>%
  rename(VPSU = VPSU_1997)

# nlsy97_debt (for RQ #1 and RQ #2)
nlsy97_debt <- nlsy97 %>%
  select(-student_debt_20, # only interested in total debt
         -student_debt_25,
         -student_debt_30,
         -student_debt_35,
         -student_debt_40) %>%
  filter(debt_20 > 0 | # filtering for respondents with debt at at least one measurement interval
           debt_25 > 0 |
           debt_30 > 0 |
           debt_35 > 0 |
           debt_40 > 0)

nlsy97_debt <- nlsy97_debt %>% # pivoting longer (for longitudinal analysis)
  pivot_longer(cols=c(debt_20,
                      debt_25,
                      debt_30,
                      debt_35,
                      debt_40),
               values_to="debt",
               names_to="measurement")

nlsy97_debt$measurement[nlsy97_debt$measurement=="debt_20"] <- 20
nlsy97_debt$measurement[nlsy97_debt$measurement=="debt_25"] <- 25
nlsy97_debt$measurement[nlsy97_debt$measurement=="debt_30"] <- 30
nlsy97_debt$measurement[nlsy97_debt$measurement=="debt_35"] <- 35
nlsy97_debt$measurement[nlsy97_debt$measurement=="debt_40"] <- 40
nlsy97_debt$measurement <- factor(nlsy97_debt$measurement)

nlsy97_debt <- nlsy97_debt %>% # selecting variables of interest
  select(id,
         measurement,
         debt,
         sex,
         parent_income,
         parent_net_worth,
         parent_education,
         race,
         VSTRAT,
         VPSU,
         weight)

# nlsy97_student_debt (for RQ #3)
nlsy97_student_debt <- nlsy97 %>%
  select(-debt_20, # only interested in student debt
         -debt_25,
         -debt_30,
         -debt_35,
         -debt_40) %>%
  filter(student_debt_20 > 0 | # filtering for respondents with student debt at at least one measurement interval
           student_debt_25 > 0 |
           student_debt_30 > 0 |
           student_debt_35 > 0 |
           student_debt_40 > 0)

nlsy97_student_debt <- nlsy97_student_debt %>% 
  pivot_longer(cols=c(student_debt_20, # pivoting longer (for longitudinal analysis)
                      student_debt_25,
                      student_debt_30,
                      student_debt_35,
                      student_debt_40),
               values_to="debt",
               names_to="measurement")

nlsy97_student_debt$measurement[nlsy97_student_debt$measurement=="student_debt_20"] <- 20
nlsy97_student_debt$measurement[nlsy97_student_debt$measurement=="student_debt_25"] <- 25
nlsy97_student_debt$measurement[nlsy97_student_debt$measurement=="student_debt_30"] <- 30
nlsy97_student_debt$measurement[nlsy97_student_debt$measurement=="student_debt_35"] <- 35
nlsy97_student_debt$measurement[nlsy97_student_debt$measurement=="student_debt_40"] <- 40
nlsy97_student_debt$measurement <- factor(nlsy97_student_debt$measurement)

nlsy97_student_debt <- nlsy97_student_debt %>% # selecting variables of interest
  select(id,
         measurement,
         debt,
         parent_income,
         parent_net_worth,
         parent_education,
         sex,
         race,
         VSTRAT,
         VPSU,
         weight)

# Fixing sparse data problem (see https://www.nlsinfo.org/content/cohorts/nlsy97/using-and-understanding-the-data/sample-weights-design-effects/page/0/1)
nlsy97_student_debt$VPSU <- ifelse(nlsy97_student_debt$VSTRAT==74 & nlsy97_student_debt$VPSU==1, 2, nlsy97_student_debt$VPSU)
nlsy97_student_debt$VPSU <- ifelse(nlsy97_student_debt$VSTRAT==73 & nlsy97_student_debt$VPSU==2, 1, nlsy97_student_debt$VPSU)
nlsy97_student_debt$VSTRAT <- ifelse(nlsy97_student_debt$VSTRAT==73 & nlsy97_student_debt$VPSU==1, 74, nlsy97_student_debt$VSTRAT)

nlsy97_student_debt$VPSU <- ifelse(nlsy97_student_debt$VSTRAT == 111 & nlsy97_student_debt$VPSU == 2, 1, nlsy97_student_debt$VPSU)
nlsy97_student_debt$VSTRAT <- ifelse(nlsy97_student_debt$VSTRAT == 112 & nlsy97_student_debt$VPSU == 2, 111, nlsy97_student_debt$VSTRAT)

# save(nlsy97_debt, file="nlsy97_debt.RData")
# save(nlsy97_student_debt, file="nlsy97_student_debt.RData")
