# Analysis with DHS, MICS, and PMA

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

library(tidyverse)
library(haven)
library(sjlabelled)
library(questionr)
library(survey)
library(jtools)

setwd("C:/Users/KristinBietsch/files/R Code/2022 Training")

#####################################################################################################################################
##########################
### DHS                ###
##########################

# I am using the model dataset from DHS- it can be downloaded here:https://www.dhsprogram.com/data/Download-Model-Datasets.cfm
# I downloaded the IR (women's recode) Stata file
women <- read_dta("ZZIR62FL.DTA")


# Setup data
# I am creating a new dataset- because if I mess something up I don't want to have to reload my original
women_clean <- women %>% mutate(sampleweights=v005/100000,
                                married=case_when(v502==1 ~ 1, v502!=1 ~ 0),
                                unmarried=case_when(v502!=1 ~ 1, v502==1 ~ 0))

# mCP, tCP, Unmet Modern, and Demand Satisfied Modern
women_clean <- women_clean %>% mutate(mcp=case_when(v313==3 ~ 1, v313!=3 ~ 0),
                                      tcp=case_when(v313==1 | v313==2 ~ 1, v313==0 | v313==3 ~ 0),
                                      unmet_mod=case_when(v626a==1 | v626a==2 ~ 1,
                                                          v313==1 | v313==2 ~ 1,
                                                          TRUE ~ 0), 
                                      ds=case_when(v313==3 ~ 1, v626a==1 | v626a==2 | v626a==3 | v626a==4 ~ 0))


# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women$v312)
Var1=get_values(women$v312)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(v312==1 ~ "Pill",
                                                       v312==3 ~ "Injectable",
                                                       v312==2 ~ "IUD",
                                                       v312==11 ~ "Implant",
                                                       v312==5 ~ "MCondom",
                                                       v312==14 ~ "FCondom",
                                                       v312==13 ~ "LAM",
                                                       v312==6 ~ "FSter",
                                                       v312==7 ~ "MSter",
                                                       v312==18 ~ "SDM",
                                                       v312==16 | v312==17 ~ "OMM"))


################################################
# Calculate Results

married <- women_clean %>% filter(married==1)
unmarried <- women_clean %>% filter(unmarried==1)

# mCP All women
prop.table(wtd.table(x= women_clean$mcp, weights = women_clean$sampleweights))
# mCP Married women
prop.table(wtd.table(x= married$mcp, weights = married$sampleweights))
# mCP Unmarried women
prop.table(wtd.table(x= unmarried$mcp, weights = unmarried$sampleweights))

# Let's say I want to store results in a dataframe (for easy export)

mcpr_aw <- as.data.frame(prop.table(wtd.table(x= women_clean$mcp, weights = women_clean$sampleweights))) %>% mutate(Pop="AW")
mcpr_mw <- as.data.frame(prop.table(wtd.table(x= married$mcp, weights = married$sampleweights))) %>% mutate(Pop="MW")
mcpr_uw <- as.data.frame(prop.table(wtd.table(x= unmarried$mcp, weights = unmarried$sampleweights))) %>% mutate(Pop="UW")

mcpr <- bind_rows(mcpr_aw, mcpr_mw, mcpr_uw) %>% filter(Var1==1) %>% rename(mCPR=Freq) %>% select(Pop, mCPR)


# Method Mix All Women
prop.table(wtd.table(x= women_clean$method, weights = women_clean$sampleweights))

# If I want to run a regression or have standard errors, need to set up the survey design
design.aw <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women_clean, nest=TRUE)
options(survey.lonely.psu="adjust")

mcpr_aw_se <-  as.data.frame(svymean(women_clean$mcp, design.aw, na.rm=TRUE))
mcpr_mw_um_se <- as.data.frame(svyby(women_clean$mcp, by=women_clean$married, design=design.aw, data=women_clean, svymean, na.rm=TRUE))

# Running a regression
summ(svyglm( mcp ~ married ,  design.aw, family=quasibinomial() ), exp=TRUE, confint=TRUE)

#####################################################################################################################################
##########################
### MICS               ###
##########################

# Please note that variable names change between rounds of MICS surveys.  
# This code is written for MICS 6.  
# The dataframe "mics1" created below will show names and labels for variables and can be used to edit the code for other datasets. 

mics <- read_sav("DRCongo MICS6 SPSS Datafiles/wm.sav")

library(foreign)
mics1 <-  read.spss("DRCongo MICS6 SPSS Datafiles/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels")) %>% rownames_to_column()

attr(mics$MSTATUS,"labels")
mics <- mics %>% mutate(married=case_when(MSTATUS==1 ~ 1, MSTATUS!=1 ~ 0))


attr(mics$CP4A,"labels")
attr(mics$CP4B,"labels")
attr(mics$CP4C,"labels")
attr(mics$CP4D,"labels")
attr(mics$CP4E,"labels")
attr(mics$CP4F,"labels")
attr(mics$CP4G,"labels")
attr(mics$CP4H,"labels")
attr(mics$CP4I,"labels")
attr(mics$CP4J,"labels")
attr(mics$CP4K,"labels")
attr(mics$CP4L,"labels")
attr(mics$CP4M,"labels")
attr(mics$CP4X,"labels")


mics <- mics %>% mutate(method=case_when(CP4A=="A" ~ "F_Ster", 
                                         CP4B=="B" ~ "M_Ster", 
                                         CP4C=="C" ~ "IUD",
                                         CP4D=="D" ~ "Injectable",
                                         CP4E=="E" ~ "Implant",
                                         CP4F=="F" ~ "Pill",
                                         CP4G=="G" ~ "M_Condom",
                                         CP4H=="H" ~ "F_Condom",
                                         CP4I=="I" ~ "Diaphragm",
                                         CP4J=="J" ~ "Foam/Jelly",
                                         CP4K=="K" ~ "LAM",
                                         CP4L=="L" ~ "Periodic Abstinence/Rhythm",
                                         CP4M=="M" ~ "Withdrawal",
                                         CP4X=="X" ~ "Other", 
                                         TRUE ~  "None"))

mics$modern <- ifelse(mics$method=="F_Ster" | 
                        mics$method=="M_Ster" |
                        mics$method=="IUD" |
                        mics$method=="Injectable" |
                        mics$method=="Implant" |
                        mics$method=="Pill" |
                        mics$method=="M_Condom" |
                        mics$method=="F_Condom" |
                        mics$method=="Diaphragm" |
                        mics$method=="Foam/Jelly" |
                        mics$method=="LAM", 1, 
                      ifelse(mics$method=="Periodic Abstinence/Rhythm" |
                               mics$method=="Withdrawal" |
                               mics$method=="Other" |
                               mics$method=="Necklace"|
                               mics$method=="None" , 0, NA))


##############################################################################

married <- filter(mics, MSTATUS==1)
unmarried <- filter(mics, MSTATUS!=1)


# mCP All women
prop.table(wtd.table(x= mics$modern, weights = mics$wmweight))
# mCP Married women
prop.table(wtd.table(x= married$modern, weights = married$wmweight))
# mCP Unmarried women
prop.table(wtd.table(x= unmarried$modern, weights = unmarried$wmweight))

# If I want to run a regression or have standard errors, need to set up the survey design
design.all<- svydesign(ids=~HH1, strata=~HH6, weights=~wmweight, data=mics)
options(survey.lonely.psu="adjust")

svymean(mics$modern, design.all, na.rm=TRUE)
svyby(mics$modern, by=mics$married, design=design.all, svymean, na.rm=TRUE)

# Running a regression
summ(svyglm( modern ~ married ,  design.all, family=quasibinomial() ), exp=TRUE, confint=TRUE)

#####################################################################################################################################
##########################
### PMA                ###
##########################

women <- read_dta("PMA2020_UGP1_HQFQ_v2.0_21Jul2021.dta")

women_clean <- women %>% filter(FRS_result==1 & HHQ_result==1 &  usually_live==1 ) %>% filter(!is.na(FQweight))

# Setup data
women_clean <- women_clean %>% mutate(sampleweights= FQweight,
                                      married=case_when(FQmarital_status==1 | FQmarital_status==2 ~ 1, FQmarital_status!=1 & FQmarital_status!=2 ~ 0),
                                      unmarried=case_when(FQmarital_status==3 | FQmarital_status==4 | FQmarital_status==5 ~ 1, FQmarital_status!=3 & FQmarital_status!=4 & FQmarital_status!=5  ~ 0))

# mCP (do not need to recode), tCP (do not need to recode), Unmet Modern, and Demand Satisfied Modern
women_clean <- women_clean %>% mutate(unmet_mod=case_when(unmet==1 | unmet==2 ~ 1,
                                                          tcp==1  ~ 1,
                                                          TRUE ~ 0), 
                                      ds=case_when(mcp==1 ~ 1, unmet==1 | unmet==2 | unmet==3 | unmet==4 ~ 0))

# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women_clean$current_methodnum_rc)
Var1=get_values(women_clean$current_methodnum_rc)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(current_methodnum_rc==7 ~ "Pill",
                                                       current_methodnum_rc==5 |  current_methodnum_rc==16 ~ "Injectable",
                                                       current_methodnum_rc==4 ~ "IUD",
                                                       current_methodnum_rc==3 ~ "Implant",
                                                       current_methodnum_rc==9 ~ "MCondom",
                                                       current_methodnum_rc==10 ~ "FCondom",
                                                       current_methodnum_rc==14 ~ "LAM",
                                                       current_methodnum_rc==1 ~ "FSter",
                                                       current_methodnum_rc==2 ~ "MSter",
                                                       current_methodnum_rc==13 ~ "SDM",
                                                       current_methodnum_rc==8 | current_methodnum_rc==11 | current_methodnum_rc==12  ~ "OMM"))


################################################
# Calculate Results

married <- women_clean %>% filter(married==1)
unmarried <- women_clean %>% filter(unmarried==1)

# mCP All women
prop.table(wtd.table(x= as.factor(women_clean$mcp), weights = women_clean$sampleweights))
# mCP Married women
prop.table(wtd.table(x= as.factor(married$mcp), weights = married$sampleweights))
# mCP Unmarried women
prop.table(wtd.table(x= as.factor(unmarried$mcp), weights = unmarried$sampleweights))

# If I want to run a regression or have standard errors, need to set up the survey design
design.aw <- svydesign(ids=~EA_ID, strata=~strata, weights=~sampleweights, data=women_clean, nest=TRUE)
options(survey.lonely.psu="adjust")

mcpr_aw_se <-  as.data.frame(svymean(women_clean$mcp, design.aw, na.rm=TRUE))

# Running a regression
summ(svyglm( mcp ~ married ,  design.aw, family=quasibinomial() ), exp=TRUE, confint=TRUE)
