# THIS IS THE MASTER SCRIPT THAT REPRODUCES THE ANALYSIS FOR THE MAIN PAPER AND THE APPENDIX.
# PLEASE COPY THE "AttendanceNI" FOLDER TO THE WORKSPACE WHERE THE SCRIPT WILL BE EXECUTED. 
# THE WORKING DIRECTORY FOR THE CURRENT ANALYSIS SHOULD BE SET TO THE "AttendanceNI" FOLDER

# THE FOLLOWING DATASETS ARE ACCESSED WHILE EXECUTING THE SCRIPT:
## INDIVIDUAL-LEVEL DATA:
### WVS 7 DATA: WVS_Cross-National_Wave_7_sav_v2_0.sav; WVS_Cross-National_Wave_7_spss_v5_0.sav
### WVS 6 DATA: WV6_Data_Spss_v20180912.sav

## COUNTRY-LEVEL DATA:
### COUNTRY CODES, SURVEY WAVES/YEARS, CULTURAL ZONES, COMMUNIST GROUPS: CountryInfo.xlsx
### RELIGIOUS CHARACTERISTICS OF STATES DATASET PROJECT DATASET (ONLINE REPOSITORY)
### RELIGION AND STATE PROJECT: ROUND 3 DATASET (ONLINE REPOSITORY)
### YOU CAN ALSO UPLOAD THE TWO ONLINE REPOSITORY DATASETS FROM THE FOLDER "01_InputData"

# ONE DATASET WITH IMPUTED MISSING DATA ("imp_data_final.RData") 
## WILL BE SAVED ONTO YOUR COMPUTER ALONG WITH RUNNING THE CODE

# SOFTWARE REQUIREMENTS:
## R version 4.0.5 (2021-03-31) or later
## Package version used for the analyses:
### car_3.0.10
### dplyr_1.0.7
### gdata_2.18.0
### haven_2.4.3
### LittleHelpers_0.5.10
### mice_3.13.0
### readxl_1.3.1
### foreign_0.8.81
### kableExtra_1.3.4
### MplusAutomation_1.0.0
### reshape2_1.4.4


# LAST MODIFICATION: ALISA REMIZOVA, 05/04/2023

#===================================================================================================

# LIBRARIES AND FUNCTIONS

library(car)
library(foreign)
library(haven)
library(dplyr)
library(mice)
library(LittleHelpers)
library(MplusAutomation)
library(readxl)
library(gdata)
library(kableExtra) 
library(sna)
library(reshape2)

# Upload two functions written for the current analysis
source("./01_Scripts/02_AnalysisScripts/AddFunctions.R")

# DATA PREPARATION

# WVS 7 

# Religiosity indicators:
## Q6 - For each of the following, indicate how important it is in your life - Religion
## Q64 - For each one, could you tell me how much confidence you have in them. The Churches
## Q289 - Do you belong to a religion or religious denomination? 
## Q171 - Apart from weddings and funerals, about how often do you attend religious services these days?
## Q172 - Apart from weddings and funerals, about how often do you pray?
## Q173 - Would you say you are... religious / not a religious person / an atheist

## Q94 - For each organization, could you tell me whether you are….? 
### active / inactive member / do not belong. Church or religious organization
## Q165 - Which, if any, of the following do you believe in? God?
## Q167 - Hell
## Q164 - How important is God in your life?

# Missing data correlates:
## Q260 - Gender: Male Female 
## Q262 - You are ____ years old 
## Q288 - On this card is an income scale on which 1 indicates the lowest income group and 10 the highest income group
### in your country. We would like to know in what group your household is. 
### from Lower step to Tenth step 
## Q275 - What is the highest educational level that you have attained?
### from Early childhood education / no education
### to Doctoral or equivalent

# Other variables:
## B_COUNTRY - Country name
## N_REGION_ISO - Country-specific region (necessary for Germany)
## A_YEAR - Survey year

# Upload country codes
codes <- read_excel("./02_Data/01_InputData/CountryInfo.xlsx", sheet = "Country codes")

# Read data
WVS7 <- read.spss("./02_Data/01_InputData/WVS_Cross-National_Wave_7_sav_v2_0.sav", 
                  use.value.labels = T, to.data.frame = T, 
                  use.missings = T)
## Ignore the warnings

# Set the same country names in WVS7 as in WVS6:
levels(WVS7$B_COUNTRY)[levels(WVS7$B_COUNTRY)=="Taiwan ROC"] <- "Taiwan"
levels(WVS7$B_COUNTRY)[levels(WVS7$B_COUNTRY)=="Hong Kong SAR"] <- "Hong Kong"

# Merge with country codes
WVS7 <- merge(WVS7, codes, by.x = c("B_COUNTRY"), by.y = c("country"),  all.x = T)

# Select only necessary variables
rel_data <- select(WVS7, c(B_COUNTRY, N_REGION_ISO, Q6, Q94, Q64, Q289, Q171, Q172, Q173, Q165, Q167, 
                           Q164, Q260, Q262, Q288, Q275, A_YEAR, code)) 

# Set the following column names for religiosity indicators:
## Importance of religion = imprel
## Membership in a religious organisation = member
## Confidence in institutions = confidence
## Belonging to a denomination = belong
## Frequency of attendance = attend
## Frequency of praying = pray
## Identification as religious person = person
## Belief in God = bgod
## Belief in hell = bhell
## Importance of God = impgod
colnames(rel_data) <- c("country", "region", "imprel", "member", "confidence", "belong", "attend",
                        "pray", "person", "bgod", "bhell", "impgod", 
                        "gender", "age", "income", "education", "year", "code")

# Make two separate samples for Germany: East and West
Germany <- rel_data[rel_data$country == "Germany", ]
Germany$region <- droplevels(Germany$region)
Germany$country <- droplevels(Germany$country)

# Recode and drop Berlin
Germany$country <- Recode(
  Germany$region,
  recodes = "'DE-MV Mecklenburg-Western Pomerania' = 'Germany East';
  'DE-BB Brandenburg' = 'Germany East';
  'DE-SN Saxony' = 'Germany East';
  'DE-ST Saxony-Anhalt' = 'Germany East';
  'DE-TH Thuringia' = 'Germany East';
  'DE-BE Berlin' = NA;
  else = 'Germany West'",
  as.factor = T)

Germany <- Germany[!is.na(Germany$country), ]

# Add to the main dataset
rel_data <- rbind(rel_data, Germany)
rel_data <- subset(
  rel_data, subset = !(rel_data$country == "Germany"))
rel_data$region <- NULL
rm(Germany)

# Add country-specific data files for Kenya, Armenia, Mongolia, 
## Venezuela, Maldives, Northern Ireland, Great Britain, Czechia, and Slovakia from WVS7 last release
## (countries not included in the second WVS 7 release and WVS 6)
## with haven package so that the variables' values are trimmed

WVS7_05 <- read_spss("./02_Data/01_InputData/WVS_Cross-National_Wave_7_spss_v5_0.sav")

# Select columns
rel_data_05 <- select(WVS7_05, c(B_COUNTRY, Q6, Q94, Q64, Q289, Q171, Q172, Q173, Q165,
                                 Q167, Q164, Q260, Q262, Q288, Q275, A_YEAR))

colnames(rel_data_05) <- c("country", "imprel", "member", "confidence", "belong", "attend",
                           "pray", "person", "bgod", "bhell", "impgod", 
                           "gender", "age", "income", "education", "year")

# Recode to labelled factors
for (item in c("country", "imprel", "member", "confidence", "belong", "attend",
               "pray", "person", "bgod", "bhell", "impgod", 
               "gender", "age", "income", "education", "year")) {
  rel_data_05[, item] <- as_factor(rel_data_05[, item])
  rel_data_05[, item] <- droplevels(rel_data_05[, item])
  
}

# Subset countries
rel_data_05 <- subset(rel_data_05, subset = rel_data_05$country %in% 
                        c("Northern Ireland", "Czechia", "Slovakia", "Great Britain", "Kenya", 
                          "Armenia", "Mongolia", "Venezuela", "Maldives"))
rel_data_05$country <- droplevels(rel_data_05$country)

# Merge with country codes
rel_data_05 <- merge(rel_data_05, codes, by = c("country"),  all.x = T)

# Recode income because of different coding 
rel_data$income <- as.numeric(rel_data$income)
rel_data_05$income <- as.numeric(rel_data_05$income)

# Combine WVS 7 datasets
rel_data <- rbind(rel_data, rel_data_05)
levels(rel_data$country)
## 61 countries - do not count "Germany"

# -------------
  
# WVS 6

# Religiosity indicators:
## V9 - For each of the following, indicate how important it is in your life: Religion
## V108 - For each one, could you tell me how much confidence you have in them. The Churches
## V144 - Do you belong to a religion or religious denomination? 
## V145 - Apart from weddings and funerals, about how often do you attend religious services these days?
## V146 - Apart from weddings and funerals, about how often do you pray?
## V147 - Would you say you are... religious / not a religious person / an atheist

## V25 - For each organization, could you tell me whether you are….? 
### active / inactive member / do not belong. Church or religious organization
## V148 - Do you believe in God?
## V149 - Do you believe in hell? 
## V152 - How important is God in your life?

# Missing data correlates:
# V240 - Gender: Male Female 
# V242 - You are ____ years old 
# V239 - On this card is an income scale on which 1 indicates the lowest income group and 10 the highest
## income group in your country. We would like to know in what group your household is. 
## from Lower step to Tenth step
# V248 - What is the highest educational level that you have attained?
## from No formal education to University - level education, with degree 

# Other variables:
## V2 - Country name

# Read data
WVS6 <- read.spss("./02_Data/01_InputData/WV6_Data_Spss_v20180912.sav", use.value.labels = T, 
                  to.data.frame = T, use.missings = T)
## Ignore the warnings

# Upload survey year
WVS6_year <- read_excel("./02_Data/01_InputData/CountryInfo.xlsx", sheet = "Survey year")

# Merge with country codes and survey year
WVS6 <- merge(WVS6, WVS6_year, by.x = c("V2"), by.y = c("country"), all.x = T)
WVS6 <- merge(WVS6, codes, by.x = c("V2"), by.y = c("country"),  all.x = T)

# Select only necessary variables
rel_data_WVS6 <- select(WVS6, c(V2, V9, V25, V108, V144:V149, V152, 
                                V240, V242, V239, V248, year, code))

colnames(rel_data_WVS6) <- c("country", "imprel", "member", "confidence", "belong", "attend",
                             "pray", "person", "bgod", "bhell", "impgod", 
                             "gender", "age", "income", "education", "year", "code")

# Subset countries that did not participate in WVS 7
rel_data_WVS6 <- subset(rel_data_WVS6, subset = 
                          !(rel_data_WVS6$country %in% levels(rel_data$country)))
rel_data_WVS6$country <- droplevels(rel_data_WVS6$country)
levels(rel_data_WVS6$country)
# 24 countries

# -------------

# Harmonize "education" indicator for two waves
## WVS 6
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education) == "No formal education"|
                                  levels(rel_data_WVS6$education) =="Incomplete primary school"] <- 
  "Early childhood education (ISCED 0) / no education"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education) == "Complete primary school"] <- "Primary education (ISCED 1)"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education) == "Incomplete secondary school: technical/ vocational type"|
                                  levels(rel_data_WVS6$education) =="Incomplete secondary school: university-preparatory type"] <- 
  "Lower secondary education (ISCED 2)"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education) == "Complete secondary school: technical/ vocational type"|
                                  levels(rel_data_WVS6$education) == "Complete secondary school: university-preparatory type"|
                                  levels(rel_data_WVS6$education) == "Some university-level education, without degree"] <- 
  "Upper secondary / short tertiary education"

## WVS 7
levels(rel_data$education)[levels(rel_data$education) == "Upper secondary education (ISCED 3)"|
                                  levels(rel_data$education) =="Post-secondary non-tertiary education (ISCED 4)"|
                             levels(rel_data$education) =="Short-cycle tertiary education (ISCED 5)"] <- 
  "Upper secondary / short tertiary education"
levels(rel_data$education)[levels(rel_data$education) == "Bachelor or equivalent (ISCED 6)"|
                             levels(rel_data$education) == "Master or equivalent (ISCED 7)"|
                             levels(rel_data$education) == "Doctoral or equivalent (ISCED 8)"] <- 
  "University - level education, with degree"
rel_data$education <- droplevels(rel_data$education)

# Convert praying, belonging, and membership into numeric class 
## for each wave separately because of the different coding of levels
# Also convert year and age to numeric for WVS 7 and age for WVS 6 
# Also convert income to numeric for WVS 6 because of slightly different coding

for (item in c("member", "belong", "pray")) {
  rel_data[, item] <- as.numeric(rel_data[, item])
}

for (item in c("member", "belong", "pray", "income")) {
  rel_data_WVS6[, item] <- as.numeric(rel_data_WVS6[, item])
}

for (item in c("age", "year")) {
  rel_data[, item] <- as.numeric(as.character(rel_data[, item]))
}

rel_data_WVS6$age <- as.numeric(as.character(rel_data_WVS6$age))

# Combine two waves
rel_data <- rbind(rel_data, rel_data_WVS6)
rel_data$country <- droplevels(rel_data$country)
levels(rel_data$country)
# 85 countries in total

# -------------

# Recode the remaining variables into numeric
for (item in c("imprel", "confidence", "attend", "person", "bgod", "bhell", "impgod", 
               "education")) {
  rel_data[, item] <- as.numeric(rel_data[, item])
}

# Reverse recode all variables except impgod and member
## They do not need to be recoded - higher scores already correspond to higher religiosity
for (item in c("imprel", "confidence")) {
  rel_data[,item] <- Recode(rel_data[,item], rec= "1=4; 2=3; 3=2; 4=1; else=NA")
}

for (item in c("bgod", "bhell")) {
  rel_data[,item] <- Recode(rel_data[,item], rec= "1=2; 2=1; else=NA")
}

rel_data$attend <- Recode(rel_data$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
rel_data$pray <- Recode(rel_data$pray, rec = "1=8; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1; else=NA")

# Make person binary
rel_data$person <- Recode(rel_data$person, rec = "1=2; 2=1; 3=1; else=NA")

# Make belonging binary
rel_data$belong <- ifelse(rel_data$belong == "1", 1, 2)

#===================================================================================================

# SELECTING COUNTRIES

# There are empty categories / omitted questions in the following countries:
sapply(c("imprel", "confidence", "belong", "attend", "pray", "person", 
         "gender", "age", "income", "education"), function(x) {
           crosstab("country", x, rel_data,  margin = "row")
         }) 

# Omitted questions
## praying: Kuwait, Qatar
## attendance: Kuwait, Morocco, Qatar
## belonging: Kuwait, Qatar

# Empty response categories of categorical indicators:  
## belonging: Bangladesh, Indonesia, Iraq, Jordan, Lebanon, Pakistan, Thailand,
### Egypt, Palestine, Algeria, Haiti, Libya, Morocco, Yemen
## confidence: Bangladesh
## importance of religion: Ethiopia, Egypt

# Drop Bangladesh, Egypt, Ethiopia, Kuwait, Morocco, Qatar, Indonesia, Iraq, 
## Jordan, Lebanon, Pakistan, Thailand, Palestine, Algeria, Yemen, Haiti, and Libya
## from the whole further analysis 
## due to the absence of some core indicators
## or empty response categories of categorical indicators 
rel_data <- subset(
  rel_data, subset = !(rel_data$country %in% c("Kuwait", "Morocco", "Qatar", 
                                               "Bangladesh", "Ethiopia", "Egypt", 
                                               "Indonesia", "Iraq", "Jordan",
                                               "Lebanon", "Pakistan", "Thailand",
                                               "Palestine", "Algeria", "Libya", 
                                               "Yemen", "Haiti")))
rel_data$country <- droplevels(rel_data$country)
levels(rel_data$country)
# 68 countries for the further analysis

# Also drop Hong Kong, Macau SAR, and Puerto Rico because 
## there are no RRI and RLI country-level measures
rel_data <- subset(
  rel_data, subset = !(rel_data$country %in% c("Hong Kong", "Macau SAR", "Puerto Rico")))

rel_data$country <- droplevels(rel_data$country)
levels(rel_data$country)
# 65 countries for the further analysis

# --------------------------------------------------------------------------------

# IMPUTATION
## Imputation for all countries is time-consuming
## There is the code to load the imputed dataset at the end of the section

# Specify prediction method for each indicator
pred_matrix <- make.predictorMatrix(data = rel_data)
pred_matrix[, c("country", "code", "year")] <- 0 # exclude these variables
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = rel_data)
imp_method[c("person", "belong", "gender", "bgod", "bhell")] <- "logreg" # binary variables
imp_method[c("confidence", "member", "imprel")] <- "polr" # ordered categorical variables with > 2 categories
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm" # continuous variables

# Drop Russia, Tunisia, Turkey, Georgia, and Maldives
## The corresponding data will be imputed separately due to the convergence issues
imp_data <- subset(
  rel_data, subset = !(rel_data$country %in% 
                         c("Russia", "Tunisia", "Turkey", "Georgia", "Maldives")))
imp_data$country <- droplevels(imp_data$country)
imp_data <- split(imp_data, imp_data$country)

# There are no missing values in: 
## Rwanda, Ghana, Azerbaijan, Venezuela, Nicaragua, South Korea, 
## Colombia, Canada, and Myanmar
## Drop these countries from the imputation; they will be added later
## lapply(imp_data, function(x)
##  sum(is.na(x[, c("person", "belong", "confidence", "imprel", "attend", "pray")])))

imp_data <- subset(imp_data, subset = !(names(imp_data) %in% 
                                          c("Rwanda", "Ghana", "Azerbaijan", "Venezuela", "Nicaragua", 
                                            "South Korea", "Colombia", "Canada", "Myanmar")))


for (i in 1:length(imp_data)){
  for (item in c("person", "belong", "bgod", "bhell", "confidence", "member", "imprel")) {
    imp_data[[i]][, item] <- as.factor(imp_data[[i]][, item])
  }
    
    imp_data[[i]] <- mice(data = imp_data[[i]], m = 5, #5 imputed datasets
                          maxit = 100, #100 iterations
                          meth = imp_method, predictorMatrix = pred_matrix, 
                          seed = 12345, #to replicate the results
                          print = F)
}

## Ignore the warnings - logged events (for constant variables):
# Kyrgyzstan: bhell

# Diagnostics
plot(imp_data$Andorra)
# for continuous variables
densityplot(imp_data$Andorra)
# or 
bwplot(imp_data$Andorra)

# Make five imputed datasets for each country in a list
for (i in 1:length(imp_data)){
  imp_data[[i]] <- complete(imp_data[[i]], action = "all") 
}

# ----------

# Imputation for Russia and Georgia
## Impute without bgod due to its high correlation with person (>0.9)
## in Russia - bgod is collinear (mice warning)
## correlations computed with lavCor function, "pairwise" deletion of missings
## e.g., subset Georgia and compute
## lavCor(Georgia[, 2:10], ordered = c("person", "belong", "bgod", "bhell", 
## "confidence", "member", "imprel"), 
## missing = "pairwise")

imp_data_RG <- subset(rel_data, rel_data$country %in% c("Russia", "Georgia"))
imp_data_RG$country <- droplevels(imp_data_RG$country)
imp_data_RG$bgod <- NULL
imp_data_RG <- split(imp_data_RG, imp_data_RG$country)

pred_matrix <- make.predictorMatrix(data = imp_data_RG$Russia)
pred_matrix[, c("country", "code", "year")] <- 0
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = imp_data_RG$Russia)
imp_method[c("person", "belong", "gender", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"


for (i in 1:length(imp_data_RG)){
  for (item in c("person", "bhell", "belong", "confidence", "member", "imprel")) {
    imp_data_RG[[i]][, item] <- as.factor(imp_data_RG[[i]][, item])
  }
  
  imp_data_RG[[i]] <- mice(data = imp_data_RG[[i]], m = 5, #5 imputed datasets
                              maxit = 100, #100 iterations
                              meth = imp_method, predictorMatrix = pred_matrix, 
                              seed = 12345, #to replicate the results
                              print = F)

}


# Diagnostics
plot(imp_data_RG$Georgia)
# for continious variables
densityplot(imp_data_RG$Georgia)
# or 
bwplot(imp_data_RG$Georgia)

# Make five imputed datasets for each country in a list
for (i in 1:length(imp_data_RG)){
  imp_data_RG[[i]] <- complete(imp_data_RG[[i]], action = "all")
}

# Add to the list with the remaining countries
imp_data$Russia <- imp_data_RG$Russia
imp_data$Georgia <- imp_data_RG$Georgia


# Imputation for Tunisia
## Impute without bhell due to the convergence issues when predicting bgod (mice warning)
## high correlation with bgod (>0.9)
Tunisia <- rel_data[rel_data$country=="Tunisia", ]
Tunisia$bhell <- NULL
pred_matrix <- make.predictorMatrix(data = Tunisia)
pred_matrix[, c("country", "code", "year")] <- 0
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = Tunisia)
imp_method[c("person", "belong", "gender", "bgod")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "belong", "confidence", "member", "imprel", "bgod")) {
  Tunisia[, item] <- as.factor(Tunisia[, item])
}

Tunisia <- mice(data = Tunisia, m = 5, #5 imputed datasets
               maxit = 100, #100 iterations
               meth = imp_method, predictorMatrix = pred_matrix, 
               seed = 12345, #to replicate the results
               print = F)

# Diagnostics
plot(Tunisia)
# for continuous variables
densityplot(Tunisia)
# or 
bwplot(Tunisia)

# Make five imputed datasets and add to the list with the remaining countries
Tunisia <- complete(Tunisia, action = "all")
imp_data$Tunisia <- Tunisia


# Imputation for Turkey
## Impute without belong due to the convergence issues when predicting attendance (mice warning)
Turkey <- rel_data[rel_data$country=="Turkey",]
Turkey$belong <- NULL
pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year")] <- 0
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "gender", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "confidence", "member", "imprel", "bgod", "bhell")) {
  Turkey[, item] <- as.factor(Turkey[, item])
}

Turkey <- mice(data = Turkey, m = 5, #5 imputed datasets
                maxit = 100, #100 iterations
                meth = imp_method, predictorMatrix = pred_matrix, 
                seed = 12345, #to replicate the results
                print = F)

# Diagnostics
plot(Turkey)
# for continuous variables
densityplot(Turkey)
# or 
bwplot(Turkey)

# Make five imputed datasets and add to the list with the remaining countries
Turkey <- complete(Turkey, action = "all")
imp_data$Turkey <- Turkey

## Impute belong without attendance
Turkey <- rel_data[rel_data$country=="Turkey",]
Turkey$attend <- NULL
pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year")] <- 0
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "belong", "gender", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "belong", "confidence", "member", "imprel", "bgod", "bhell")) {
  Turkey[, item] <- as.factor(Turkey[, item])
}

Turkey <- mice(data = Turkey, m = 5, #5 imputed datasets
                maxit = 100, #100 iterations
                meth = imp_method, predictorMatrix = pred_matrix, 
                seed = 12345, #to replicate the results
                print = F)

# Diagnostics
plot(Turkey)
# for continuous variables
densityplot(Turkey)
# or 
bwplot(Turkey)

## Add belong to the imputed datasets
Turkey <- complete(Turkey, action = "all")
for (i in 1:length(imp_data$Turkey)){
  imp_data$Turkey[[i]]$belong <- Turkey[[i]]$belong
}

# Imputation for Maldives
## Impute without bgod due to its high correlation with confidence (>0.9)
Maldives <- rel_data[rel_data$country=="Maldives", ]
Maldives$bgod <- NULL
pred_matrix <- make.predictorMatrix(data = Maldives)
pred_matrix[, c("country", "code", "year")] <- 0
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = Maldives)
imp_method[c("person", "gender", "belong", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "confidence", "member", "imprel", "belong", "bhell")) {
  Maldives[, item] <- as.factor(Maldives[, item])
}


Maldives <- mice(data = Maldives, m = 5, #5 imputed datasets
                 maxit = 100, #100 iterations
                 meth = imp_method, predictorMatrix = pred_matrix, 
                 seed = 12345, #to replicate the results
                 print = F)

# Diagnostics
plot(Maldives)
# for continuous variables
densityplot(Maldives)
# or 
bwplot(Maldives)

# Make five imputed datasets and add to the list with the remaining countries
Maldives <- complete(Maldives, action = "all")
imp_data$Maldives <- Maldives

# -------------

# Make five datasets for each country that had no missing values
imp_data_nomis <- subset(rel_data, 
                         subset = (rel_data$country %in%
                                      c("Rwanda", "Ghana", "Azerbaijan", "Venezuela", "Nicaragua",
                                        "South Korea", "Colombia", "Canada", "Myanmar")))
imp_data_nomis$country <- droplevels(imp_data_nomis$country)
imp_data_nomis <- split(imp_data_nomis, imp_data_nomis$country)

imp_data_nomis <- lapply(imp_data_nomis, function(x)
  rep(list(x), 5))

# Combine datasets
imp_data <- append(imp_data, imp_data_nomis)

# -------------

# Make final imputed datasets
for (i in 1:length(imp_data)){
  
  for (a in 1:length(imp_data[[i]])){
    # Drop all variables used for imputation only
    # Specify the same order of columns for all datasets
    imp_data[[i]][[a]] <- imp_data[[i]][[a]][, c("country", "imprel", "confidence", "belong", "attend",
                                                 "pray", "person", "year", "code")]
    
    imp_data[[i]][[a]]$country <- droplevels(imp_data[[i]][[a]]$country)
    
    # Recode factor variables into numeric
    for (item in c("person", "belong", "confidence", "imprel")) {
      imp_data[[i]][[a]][, item] <- as.numeric(imp_data[[i]][[a]][, item])
    }
    
  }
  
}

imp_data <- imp_data[order(names(imp_data))]

# Save or download the imputed dataset 
## save(imp_data, file = "imp_data_final.RData")
## load("./02_Data/02_AnalysisData/imp_data_final.RData")

rm(Tunisia, Turkey, Maldives, imp_data_RG, imp_data_nomis)

#===================================================================================================

# CFA

# ordered indicators
ord_items <- c("person", "imprel", "confidence", "belong")

# One-factor model - Model 2 from 
## Remizova, Alisa, Maksim Rudnev, and Eldad Davidov. 2022. 
## In search of a comparable measure of generalized individual religiosity 
## in the World Values Survey. Sociological Methods and Research:1-33.

## Add the second residual covariance: person ~~ belong

model <- 'Religiosity =~ imprel + person + confidence + pray + attend + belong;
pray ~~ attend;
person ~~ belong;
'

# Save only the countries with acceptable model fit
cfa_model <- groupwiseCFA.mi.sep(model, data = imp_data, ordered = ord_items, 
                                  estimator = "WLSMV", out = c("goodfit"))

# Ignore the warnings, they do not affect the sample with acceptable model fit
## except Poland - it will be dropped later

## Verify: 
## imp_data2 <- subset(imp_data, (names(imp_data) %in% rownames(cfa_model)[-31]))
## cfa_model2 <- groupwiseCFA.mi.sep(model, data = imp_data2, ordered = ord_items, 
##                                   estimator = "WLSMV", out = c("goodfit"))
## There are no warnings

# Compute correlations between variables
## and drop countries with negative correlations or collinear variables 
## from the final samples of the CFA model (if necessary)
imp_data_cor <- imp_data
for (i in 1:length(imp_data_cor)) {
  
  for (a in 1:length(imp_data_cor[[i]])) { 
    # Compute correlations that are used in CFA analyses
    imp_data_cor[[i]][[a]] <- lavCor(as.data.frame(
      imp_data_cor[[i]][[a]][, c("person", "belong", "confidence", "imprel", 
                            "pray", "attend")]), ordered = ord_items, output = "cor")
    
    # Remove diagonal variances
    imp_data_cor[[i]][[a]] <- diag.remove(imp_data_cor[[i]][[a]], remove.val = NA)
    
    # Find negative correlations
    imp_data_cor[[i]][[a]] <- ifelse(imp_data_cor[[i]][[a]] < 0, imp_data_cor[[i]][[a]], NA)
    imp_data_cor[[i]][[a]] <- sum(!is.na(imp_data_cor[[i]][[a]]))
    
  }
  
}

lapply(imp_data_cor, function(x) 
  lapply(x, function(y)
    y == 0))

# There are negative correlations in:
## China, Maldives, Myanmar, Nigeria, the Philippines, Rwanda, Tunisia, and Zimbabwe

# Make five final imputed datasets
data1 <- lapply(imp_data, function(x) x[[1]]) %>% do.call("rbind", .) 
data2 <- lapply(imp_data, function(x) x[[2]]) %>% do.call("rbind", .) 
data3 <- lapply(imp_data, function(x) x[[3]]) %>% do.call("rbind", .) 
data4 <- lapply(imp_data, function(x) x[[4]]) %>% do.call("rbind", .) 
data5 <- lapply(imp_data, function(x) x[[5]]) %>% do.call("rbind", .) 

data_list <- list(data1, data2, data3, data4, data5)
rm(data1, data2, data3, data4, data5)

# Sample for model 2 - select 42 countries
## drop Poland
## lavaan WARNING: correlation between variables belong and person is (nearly) 1.0
mlsem_dat <- lapply(data_list, function(x) 
  subset(x, subset = x$country %in% rownames(cfa_model)[-31]))

for (i in 1:length(mlsem_dat)) {
  mlsem_dat[[i]]$country <- droplevels(mlsem_dat[[i]]$country)
}

# MGCFA to ensure the similarity of the factor structure
mgcfa_model <- globalMI.mi.sep(model, data = mlsem_dat, ordered = ord_items, 
                                estimator = "WLSMV", group = "country")


#===================================================================================================

# COUNTRY-LEVEL INDICATORS

# Religious composition
# Data: Brown, D., & James, P. Religious Characteristics of States Dataset Project - Demographics v. 2.0 (RCS-Dem 2.0), COUNTRIES ONLY.

# Read the data from online repository or download from the InputData folder
rcs_data <- read.spss("https://osf.io/xtvf5/download", use.value.labels = T, to.data.frame = T)

# WVS6: select observations corresponding to the country-specific year of WVS data collection
# WVS7: select observations for 2015 year as the most recent data

# The following variables are selected:
## NUMISO - Country Code assigned by International Standards Organization

## Christians:
### CATPC - Percentage of Catholics
### XPRTPC - Percentage of Extended Protestants (Combined Protestant, Anglican, Pentecostal)
### ORTPC - Percentage of Orthodox

## UPRTPC - Percentage of unspecified Protestants - to drop it for South Korea

## Other Christians:
### OCHRPC - Percentage of Other Non-Liminal Christians
### CSYNPC - Percentage of Christian Syncretics (Mostly African and New World African spiritist and spiritualist denominations)
### Percentage of Liminal Christians:
#### ULCHPC - Percentage of unspecified Liminal Christians
#### ECCPC - Percentage of Extra-Canonical Christians 
#### LDSPC - Percentage of Latter-Day Saints (Mormons) 
#### OECCPC - Percentage of Other Extra-Canonical Christians 
#### NTCPC - Percentage of Non-Trinitarian Christians 
#### JWTPC - Percentage of Jehovah’s Witnesses 
#### UNIPC - Percentage of Unitarians
#### ONTCPC - Percentage of Other Non-Trinitarian Christians
#### OLCHPC - Percentage of Other Liminal Christians 

## Muslims:
### MUSPC - Percentage of RCMUSLIMs
### MSYNPC - Percentage of RCMUSLIM Syncretics 

## Asian Religions:
## HINPC - Percentage of Hindus
## Buddhists:
### BUDPC - Percentage of Buddhists
### BSYNPC - Percentage of Buddhist Syncretics 
## Other religions:
### East Asian Complex:
#### JAIPC - Percentage of Jains
#### SHNPC - Percentage of Shintoists
#### CNFPC - Percentage of Confucianists
#### TAOPC - Percentage of Taoists
#### CHFPC - Percentage of Chinese Folk Religionists

## NREPC - Percentage of Not Religious
## UNKPC - Percentage of Unknown

## Other religions not included in the analysis:
### JEWPC - Percentage of Jews
### MANPC - Percentage of Mandaeans
### ZORPC - Percentage of Zoroastrians
### BAHPC - Percentage of Bahais
### SIKPC - Percentage of Sikhs
### INDPC - Percentage of Indigenous Religionists (Ethnoreligionists)
### NEWPC - Percentage of New Age Religionists
### OREPC - Percentage of Other Religionists

## UCHRPC - Percentage of unspecified Christians
### All known Christians of unknown classification;
### also used when sum of branches departs from best estimate of total Christians; 
### negative number denotes double-affiliates

# Change country code for Yugoslavia, as Serbia (345, YUG) - in codebook, but in reality it is 688
rcs_data$NUMISO[rcs_data$NUMISO =="688"] <- "891"

# Subset WVS 7 countries and the most recent available 2015 year
rel_compos_WVS7 <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% WVS7$code) & 
                                                rcs_data$YEAR == 2015))

# Subset added countries from the fifth release
rel_compos_WVS7_05 <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% 
                                                    rel_data_05$code) & 
                                                    rcs_data$YEAR == 2015))

rel_compos_WVS7 <- rbind(rel_compos_WVS7, rel_compos_WVS7_05)
rm(rel_compos_WVS7_05)

# Subset WVS 6 countries and the year corresponding to the data collection year
rel_compos_WVS6 <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% rel_data_WVS6$code) & 
                                                (rcs_data$YEAR %in% rel_data_WVS6$year))) 

rel_compos_WVS6 <- merge(rel_data_WVS6[,c("code", "year")], rel_compos_WVS6, by.x = c("code", "year"), 
                         by.y = c("NUMISO", "YEAR"))
rel_compos_WVS6 <- rel_compos_WVS6[!duplicated(rel_compos_WVS6$code), ]

# Select variables
rel_compos_WVS7 <- select(rel_compos_WVS7, c(NUMISO, YEAR, CATPC, XPRTPC, ORTPC, 
                                             OCHRPC, CSYNPC, UPRTPC, 
                                             ULCHPC, ECCPC, LDSPC, OECCPC, NTCPC, JWTPC, UNIPC, ONTCPC, OLCHPC,
                                             MUSPC, MSYNPC, 
                                             HINPC,  
                                             BUDPC, BSYNPC,
                                             SHNPC, CNFPC, TAOPC, CHFPC, 
                                             NREPC, UNKPC, 
                                             JEWPC, MANPC, ZORPC, BAHPC, JAIPC, SIKPC, INDPC, NEWPC, OREPC,
                                             UCHRPC))  
names(rel_compos_WVS7)[names(rel_compos_WVS7) == "NUMISO"] <- "code"
names(rel_compos_WVS7)[names(rel_compos_WVS7) == "YEAR"] <- "year"

rel_compos_WVS6 <- select(rel_compos_WVS6, c(code, year, CATPC, XPRTPC, ORTPC, 
                                             OCHRPC, CSYNPC, UPRTPC, 
                                             ULCHPC, ECCPC, LDSPC, OECCPC, NTCPC, JWTPC, UNIPC, ONTCPC, OLCHPC,
                                             MUSPC, MSYNPC, 
                                             HINPC,  
                                             BUDPC, BSYNPC,
                                             SHNPC, CNFPC, TAOPC, CHFPC, 
                                             NREPC, UNKPC, 
                                             JEWPC, MANPC, ZORPC, BAHPC, JAIPC, SIKPC, INDPC, NEWPC, OREPC,
                                             UCHRPC)) 

rel_compos <- rbind(rel_compos_WVS6, rel_compos_WVS7)
rm(rel_compos_WVS6, rel_compos_WVS7)

# Remove countries with empty response options of categorical indicators or omitted questions of religiosity
rel_compos <- subset(rel_compos, subset = rel_compos$code %in% rel_data$code)
# 63 countries. The Northern Ireland will be added later; Germany will be divided latter


# Calculate final country-level predictors
## Followers of Asian Religions
rel_compos$RCASIAN <- rowSums(
  rel_compos[, c("SHNPC", "CNFPC", "TAOPC", "CHFPC", "JAIPC", ## other Asian religions
                 "HINPC", 
                 "BUDPC", "BSYNPC" ## Buddhists
                 )], na.rm=T)


## Christians
## for South Korea add negative UPRTPC to XPRTPC
rel_compos$XPRTPC <- ifelse((rel_compos$code=="410"), 
                            (rel_compos$XPRTP + rel_compos$UPRTPC), 
                            rel_compos$XPRTPC)

rel_compos$RCCHR <- rowSums(
  rel_compos[,c("CATPC", "XPRTPC", "ORTPC", 
                "ULCHPC", "ECCPC", "LDSPC", "OECCPC", "NTCPC", 
                "JWTPC", "UNIPC", "ONTCPC", "OLCHPC", ## Liminal Christians
                "OCHRPC", "CSYNPC" ## other Christians
                )], na.rm=T)

## Muslims
rel_compos$RCMUSLIM <- rowSums(rel_compos[,c("MUSPC", "MSYNPC")], na.rm=T)

## Variable with all other categories
rel_compos$RCOTHER <- rowSums(
  rel_compos[,c("NREPC", "UNKPC", 
                "JEWPC", "MANPC", "ZORPC", "BAHPC", "SIKPC",
                "INDPC", "NEWPC", "OREPC" ## other religions
                )], na.rm=T)

#-----------

# Adjust variables
# Add percentage of unspecified Christians to Christians variable in:
## Malaysia, Mongolia, New Zealand, United States,
## India, Rwanda, and South Africa
## UCHRPC is positive + less than 100% of religious adherents in total for these countries 
## -> additional observations are needed

## Argentina, Bolivia, Brazil, Cyprus, 
## Guatemala, Iraq, Japan, Kenya, Lebanon, Mexico, Nicaragua, Nigeria, 
## the Philippines, Serbia, Singapore, Vietnam, Zimbabwe, the Netherlands, and the UK
## UCHRPC is negative + more than 100% of religious adherents in total 
## -> possibly double-affiliated Christians
rel_compos$RCCHR <- ifelse((rel_compos$code=="458"|rel_compos$code=="496"|
                              rel_compos$code=="554"| rel_compos$code=="826"|
                              rel_compos$code=="840"|rel_compos$code=="356"|
                              rel_compos$code=="646"|rel_compos$code=="710"|
                              rel_compos$code=="32"|rel_compos$code=="68"|
                              rel_compos$code=="76"|rel_compos$code=="320"|
                              rel_compos$code=="392"|rel_compos$code=="484"|
                              rel_compos$code=="558"|rel_compos$code=="608"|
                              rel_compos$code=="702"|rel_compos$code=="716"|
                              rel_compos$code=="891"|rel_compos$code=="422"|
                              rel_compos$code=="566"|rel_compos$code=="704"|
                              rel_compos$code=="196"|rel_compos$code=="368"|
                              rel_compos$code=="528"|rel_compos$code=="404"), 
                            rowSums(rel_compos[,c("RCCHR", "UCHRPC")], na.rm = T), rel_compos$RCCHR)

# If the total percentage of adherents of all religions is >100%, subtract the extra %
## from all categories (if a category is higher than extra %)
## proportionally to the number of variables in total: 
rel_compos[is.na(rel_compos)] <- 0
rel_compos$SUM <- rowSums(
  rel_compos[,c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")], na.rm = T)
rel_compos$DIF <- (100 - rel_compos$SUM)

# Aalculate mean deviation for the Appendix
## round(mean(rel_compos$DIF), 2)

# Number of non-zero elements (as we cannot subtract the difference from religions with zero %)
rel_compos$NMISS <- rowSums(
  rel_compos[,c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")] != 0)

# Calculate the new difference to further subtraction based on non-zero elements
## because the difference is negative - subtract from 0
rel_compos$DIF1 <- (0 - rel_compos$DIF/rel_compos$NMISS)

# We cannot subtract from cells with values lower than the potential subtraction itself
# -> count the number of cells where we can subtract 
rel_compos$NMISS1 <- rowSums(
  rel_compos[,c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")] > rel_compos$DIF1)

# Repeat the procedure - the subtraction value changes 
# -> the number of cells where we can subtract changes as well
rel_compos$DIF1  <- (0 - rel_compos$DIF/rel_compos$NMISS1)
rel_compos$NMISS1 <- rowSums(
  rel_compos[,c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")] > rel_compos$DIF1)

# Final differences to subtract
rel_compos$DIF1  <- (0 - rel_compos$DIF/rel_compos$NMISS1)

# Subtract 
for (item in c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")) {
  rel_compos[, item] <- ifelse((rel_compos[, item] > rel_compos$DIF1), (rel_compos[, item] - rel_compos$DIF1), 
                               rel_compos[, item])
}

# If you want to check
## table(rowSums(
##   rel_compos[,c("RCCHR", "RCASIAN", "RCMUSLIM", "RCOTHER")], na.rm=T))

# Add country names
rel_compos <- merge(rel_compos, rel_data[, c(1, 17)], by = c("code"))
rel_compos <- rel_compos[!duplicated(rel_compos$code), ]
rel_compos <- trim(rel_compos)
rel_compos <- rel_compos[order(rel_compos$country), ] 

# Duplicate Germany for two samples:
rel_compos[18, 48] <- "Germany West"
rel_compos <- rbind(rel_compos, rel_compos[rel_compos$country == "Germany West", ])
rel_compos[nrow(rel_compos), 48] <- "Germany East"

# Duplicate the Great Britain for the WVS Northern Ireland
rel_compos <- rbind(rel_compos, rel_compos[rel_compos$country == "Great Britain", ])
rel_compos[nrow(rel_compos), 48] <- "Northern Ireland"

rel_compos <- rel_compos[order(rel_compos$country), ]

# ---------------------------------------------------------------------------------------------------------

# Cultural zones
## ZAFRICA - sub-Saharan Africa
## ZINDIC	- Indic East
## ZLA - Latin America
## ZRCMUSLIM - Islamic East
## ZNWEST	- New West
## ZOLDWEST	- Old West
## ZORT	- Orthodox East
## ZREFWEST	- Reformed West
## ZSINIC	- Sinic East

# Read the data
zones <- read_excel("./02_Data/01_InputData/CountryInfo.xlsx", sheet = "Predictors")
zones$COMMALL <- NULL
zones[is.na(zones)] <- 0

# ---------------------------------------------------------------------------------------------------------

# Communist legacy
## COMMALL - all countries that experienced/experience the communist regime

# Read the data
communism <- read_excel("./02_Data/01_InputData/CountryInfo.xlsx", sheet = "Predictors")
communism <- select(communism, c(country, COMMALL)) 
communism[is.na(communism)] <- 0

# ---------------------------------------------------------------------------------------------------------

# Religious regulations

# Religion and State: Round 3
## NXX2010-2014 - Religious Regulation [annually; 1990-2014]
### Regulation of and Restrictions on the Majority Religion or All Religions: INDEX
## LXX2010-2014 - Religious Legislation [annually; 1990-2014]
### Specific Types of Religious Support: INDEX

# Read the data from online repository or download from the InputData folder
rands_data <- read.spss("https://osf.io/mq2kt/download", use.value.labels = T, to.data.frame=T)

rel_regulation <- select(rands_data, c(COUNTRY, NUMISO,
                                   NXX2010, NXX2011, NXX2012, NXX2013, NXX2014,
                                   LXX2010, LXX2011, LXX2012, LXX2013, LXX2014))

# Change country code for Yugoslavia, as Serbia (345, YUG) - in codebook, but in reality it is 688
rel_regulation$NUMISO[rel_regulation$NUMISO =="688"] <- "891"

rel_regulation <- merge(
  rel_regulation, rel_data[, c(1,17)], 
  by.x = c("NUMISO"), by.y = c("code"))
rel_regulation <- rel_regulation[!duplicated(rel_regulation$country), ]

# Reshape data to the long format
rel_regulation <- reshape(rel_regulation, direction = "long",
                          varying = list(NXX = c("NXX2010", "NXX2011", "NXX2012", "NXX2013", "NXX2014"),
                                         LXX = c("LXX2010", "LXX2011", "LXX2012", "LXX2013", "LXX2014")),
                          timevar = c("year"),
                          times = c("2010", "2011", "2012", "2013", "2014"),
                          idvar = "country",
                          sep = "",
                          v.names=c("NXX", "LXX"))

# Subset WVS 7 countries and the most recent available 2014 year
rel_regulation_WVS7 <- subset(rel_regulation, subset = ((rel_regulation$NUMISO %in% WVS7$code) & 
                                                          rel_regulation$year == 2014))

# Subset added countries from the fifth release
rel_regulation_WVS7_05 <- subset(rel_regulation, subset = ((rel_regulation$NUMISO %in% 
                                                          rel_data_05$code) & 
                                                         rel_regulation$year == 2014))
# Combine datasets
rel_regulation_WVS7 <- rbind(rel_regulation_WVS7, rel_regulation_WVS7_05)


# Select WVS 6 countries and the year corresponding to the data collection year
rel_regulation_WVS6 <- subset(rel_regulation, subset = ((rel_regulation$NUMISO %in% rel_data_WVS6$code) & 
                                                (rel_regulation$year %in% rel_data_WVS6$year))) 

rel_regulation_WVS6 <- merge(rel_data_WVS6[,c("code", "year")], rel_regulation_WVS6, by.x = c("code", "year"), 
                         by.y = c("NUMISO", "year"))
rel_regulation_WVS6 <- rel_regulation_WVS6[!duplicated(rel_regulation_WVS6$code), ]

rel_regulation_WVS7$COUNTRY <- NULL
rel_regulation_WVS6$COUNTRY <- NULL

colnames(rel_regulation_WVS7)[1] <- "code"

# Combine datasets
rel_regulation <- rbind(rel_regulation_WVS6, rel_regulation_WVS7)

# Set column names:
## RRI - Religious Regulation Index
## RLI - Religious Legislation Index
names(rel_regulation)[names(rel_regulation) == "NXX"] <- 'RRI'
names(rel_regulation)[names(rel_regulation) == "LXX"] <- 'RLI'

rm(rel_regulation_WVS6, rel_regulation_WVS7, rel_regulation_WVS7_05)

#-----------------------------------------------------------------------------------------------

# Merge imputed data with country-level predictors
mlsem_dat <- lapply(mlsem_dat, function(y) 
  Reduce(function(x, z) merge(x, z, by = c("country"), all.x = F),
         list(y, rel_compos[, c("country", "RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER")], 
              communism[, c("country", "COMMALL")], 
              rel_regulation[, c("country", "RRI", "RLI")],
              zones[, c("country", "ZAFRICA", "ZLA", "ZINDIC", "ZSINIC",
                        "ZNWEST", "ZISLAM", "ZORT", "ZEUWEST")])))

for (i in 1:length(mlsem_dat)) {
  mlsem_dat[[i]]$country <- droplevels(mlsem_dat[[i]]$country)  
}

# Save each of the five imputed datasets for the further analysis in Mplus 
for (i in 1:length(mlsem_dat)) {
  prepareMplusData(mlsem_dat[[i]], filename = paste0("mlsem_dat", i, ".dat"))
}

# -------------

# To test the differences within the European West zone
# Reshape one variable to the wide format and create a new detailed dataset
zones_new <- dcast(zones[, 1:2], formula = country ~ Zone, 
                   fun.aggregate = length)
# Ignore the message

# Create new variables that combines several categories
zones_new$ZMIX1 <- ifelse((zones_new$ZAFRICA == 1|zones_new$ZLA == 1|zones_new$ZNWEST == 1),
                          1, 0)
zones_new$ZMIX2 <- ifelse((zones_new$ZINDIC == 1|zones_new$ZSINIC == 1|zones_new$ZISLAM == 1),
                          1, 0)

# Save data
# Merge imputed data with country-level predictors
mlsem_dat_zones <- lapply(mlsem_dat, function(y) 
  Reduce(function(x, z) merge(x, z, by = c("country"), all.x = F),
         list(y, rel_compos[, c("country", "RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER")], 
              communism[, c("country", "COMMALL")], 
              rel_regulation[, c("country", "RRI", "RLI")],
              zones_new[, c("country", "ZAFRICA", "ZLA", "ZINDIC", "ZSINIC",
                        "ZNWEST", "ZISLAM", "ZORT", "ZOLDWEST", "ZREFWEST", "ZRETWEST", "ZMIX1", 
                        "ZMIX2")])))

for (i in 1:length(mlsem_dat_zones)) {
  mlsem_dat_zones[[i]]$country <- droplevels(mlsem_dat_zones[[i]]$country)  
}

for (i in 1:length(mlsem_dat_zones)) {
  prepareMplusData(mlsem_dat_zones[[i]], filename = paste0("mlsem_dat_zones", i, ".dat"))
}

# ===================================================================================================

# RESULTS

estimates <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Retrieve the estimates for the parameters of interest: random intercept and slope
for (i in 1:length(estimates)) {
  
  estimates[[i]] <- estimates[[i]]$bparameters$valid_draw #post-burn-in iterations
  
  estimates[[i]] <- as.data.frame(do.call("rbind", estimates[[i]]))
  
  estimates[[i]] <- 
    estimates[[i]][, -grep("BETWEEN%:.ATTEND.ON.REL_B", colnames(estimates[[i]]))]
  
  estimates[[i]] <- 
    estimates[[i]][, grepl("BETWEEN%:.ATTEND.ON|BETWEEN%:.SLOPE.ON", 
                           names(estimates[[i]]))]
  
}

# Specify names of hypotheses
mod_names <- lapply(names(estimates), function(x)
  strsplit(x, "[.]")[[1]])
for (i in 1:length(mod_names)) {
  mod_names[[i]] <- mod_names[[i]][length(mod_names[[i]]) - 1]
}

# Specify names of predictors
pred_names <- estimates
for (i in 1:length(pred_names)) {
  pred_names[[i]] <- lapply(names(pred_names[[i]]), function(x)
    strsplit(x, "[.]")[[1]])
  
  pred_names[[i]] <- lapply(pred_names[[i]], function(x)
    x[length(x)]) 
}

# Select every 5th element (i.e., one of the imputed datasets)
pred_names <- pred_names[seq(1, length(pred_names), 5)]

# Combine datasets for each hypotheses
estimates <- tapply(estimates, 
                    rep(1:(length(estimates)/5), each = 5), function(x)
                      bind_rows(x, .id = NULL))

names(estimates) <- mod_names[seq(1, length(mod_names), 5)]

for (i in 1:length(estimates)) {
  names(estimates[[i]]) <- pred_names[[i]]
}


# Calculate median and CI for each effect
for (i in 1:length(estimates)) {
  estimates[[i]] <- apply(estimates[[i]], 2, function(x)
    ifelse((quantile(x, 0.025) < 0 & quantile(x, 0.975) > 0),
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3),  ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]"), 
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3), ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]*")
           )
  )
  
  estimates[[i]] <- as.data.frame(estimates[[i]])
  
  estimates[[i]] <- cbind(rownames(estimates[[i]]), estimates[[i]])
  colnames(estimates[[i]]) <- c("Predictor", "Estimate [CI]")
  
  
}

estimates_tab <- Reduce(
  function(x, y) merge(x, y, by = c("Predictor"), all = TRUE),
  estimates
)

# Ignore the warnings, they are related to the arguments' format only
colnames(estimates_tab) <- c("Predictor", names(estimates))
estimates_tab$RI_SIMPLE_1 <- NULL
estimates_tab$RS_SIMPLE_1 <- NULL

# Add the line with the residual variance
res_var <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

names(res_var) <- mod_names

# Retrieve the estimates for the parameters of interest: random intercept and slope
for (i in 1:length(res_var)) {
  res_var[[i]] <- res_var[[i]]$bparameters$valid_draw #post-burn-in iterations
  
  res_var[[i]] <- as.data.frame(do.call("rbind", res_var[[i]]))
  
  res_var[[i]] <- 
    res_var[[i]][, -grep(".ON.", colnames(res_var[[i]]))]
  
  res_var[[i]] <- as.data.frame(res_var[[i]])
  
  res_var[[i]] <- 
    res_var[[i]][, grepl("BETWEEN%:.ATTEND|BETWEEN%:.SLOPE", names(res_var[[i]]))]
  
  
}

# Select only slope variance for the models with RS
for (i in 21:length(res_var)) {
  res_var[[i]] <- res_var[[i]][, 1]
}

# Combine datasets for each hypotheses
res_var <- tapply(res_var, rep(1:(length(res_var)/5), each = 5), function(x)
  bind_rows(x, .id = NULL))

names(res_var) <- mod_names[seq(1, length(mod_names), 5)]

## (residual) variance without predictors
## paste0(round(median(unlist(res_var$RI_SIMPLE_1)), 3), " [",
##        round(as.numeric(quantile(unlist(res_var$RI_SIMPLE_1), 0.025)), 3), ";", 
##        round(as.numeric(quantile(unlist(res_var$RI_SIMPLE_1), 0.975)), 3), "]*")

## paste0(round(median(unlist(res_var$RS_SIMPLE_1)), 3), " [",
##        round(as.numeric(quantile(unlist(res_var$RS_SIMPLE_1), 0.025)), 3), ";", 
##        round(as.numeric(quantile(unlist(res_var$RS_SIMPLE_1), 0.975)), 3), "]*")

res_var$RI_SIMPLE_1 <- NULL
res_var$RS_SIMPLE_1 <- NULL

for (i in 1:length(res_var)) {
  res_var[[i]] <- paste0(round(median(unlist(res_var[[i]])), 3), " [", 
                         round(as.numeric(quantile(unlist(res_var[[i]]), 0.025)), 3), ";", 
                         round(as.numeric(quantile(unlist(res_var[[i]]), 0.975)), 3), "]*")
  
}

res_var <- unlist(res_var)
res_var <- c("Residual variance", res_var)
estimates_tab <- rbind(estimates_tab, res_var)

# Recode to full names
estimates_tab$Predictor <- as.factor(estimates_tab$Predictor)
estimates_tab$Predictor <- car::Recode(estimates_tab$Predictor,
                                   as.factor = T,
                                   recodes = "'RCASIAN' = 'Percentage of the followers of Asian religions';
                                   'COMMALL' = 'Communist';
                                   'RCOTHER' = 'Percentage of Others'; 
                                   'RCMUSLIM' = 'Percentage of Muslims';
                                   'ZAFRICA' = 'Sub-Saharan Africa';
                                   'ZINDIC' = 'Indic East';
                                   'ZSINIC' = 'Sinic East';
                                   'ZISLAM' = 'Islamic East';
                                   'ZLA' = 'Latin America';
                                   'ZNWEST' = 'New West';
                                   'ZORT' = 'Orthodox East'")

# Put the slope first
estimates_tab <- estimates_tab[, c(1, 5:7, 2:4)]

# Put communism after religions
estimates_tab <- estimates_tab[c(2:4, 1, 5:14),]

# Rename the columns with the model numbers
colnames(estimates_tab) <- c("", paste0("M", 1:6))


kable(estimates_tab, row.names = FALSE) %>%
  add_header_above(c(`Predictor` = 1, `Loading` = 3, `Intercept` = 3)) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of Others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all 'other religions': Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 42 countries."
  )


# or make two separate tables for loading and intercept models
kable(estimates_tab[, 1:4], row.names = FALSE) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of Others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all 'other' religions: Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 42 countries."
  )
  

kable(estimates_tab[, c(1, 5:7)], row.names = FALSE) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all other religions: Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 42 countries."
  )



# ===================================================================================================

# DIAGNOSTICS

# PSR values
psr_values <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify names of hypotheses/models
names(psr_values) <- mod_names

for (i in 1:length(psr_values)) {
  psr_values[[i]] <- psr_values[[i]]$tech8$psr
  
  psr_values[[i]] <- subset(psr_values[[i]], psr_values[[i]]$psr > 1.1)
  psr_values[[i]] <- max(psr_values[[i]]$iteration)
  
}

# Ignore the warning - for models with no psr values > 1.1

# -------------

# Traceplots, autocorrelation plots

plots <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify directions
direct <- lapply(names(plots), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(direct)) {
  direct[[i]] <- direct[[i]][-1]
  direct[[i]] <- direct[[i]][1:(length(direct[[i]]) - 2)]
  
  direct[[i]] <- paste(direct[[i]], collapse = "/")
  
  direct[[i]] <- paste0("/", direct[[i]])
}

# Specify file names
nfile <- lapply(names(plots), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(nfile)) {
  nfile[[i]] <- paste0(nfile[[i]][length(nfile[[i]]) - 1],
                       ".gh5")
}

# Create pdf files with all plots
for (i in 1:length(direct)) {
  setwd(direct[[i]])
  traceplots_mplus(nfile[[i]], is.file = T)
}

# ===================================================================================================

# ADDITIONAL SAMPLE FOR RI MODELS
## with Hong Kong, Macau SAR, and Puerto Rico

# Select only necessary variables from the WVS 7 and subset the three countries
rel_data_add <- subset(WVS7, c(B_COUNTRY, Q6, Q94, Q64, Q289, Q171, Q172, Q173, Q165, Q167,
                               Q164, Q260, Q262, Q288, Q275, A_YEAR, code),
                       subset = WVS7$B_COUNTRY %in% c("Hong Kong", "Macau SAR", "Puerto Rico")) 

colnames(rel_data_add) <- c("country","imprel", "member", "confidence", "belong", "attend",
                            "pray", "person", "bgod", "bhell", "impgod", 
                            "gender", "age", "income", "education", "year", "code")
rel_data_add$country <- droplevels(rel_data_add$country)

# Harmonize education
levels(rel_data_add$education)[levels(rel_data_add$education) == "Upper secondary education (ISCED 3)"|
                             levels(rel_data_add$education) =="Post-secondary non-tertiary education (ISCED 4)"|
                             levels(rel_data_add$education) =="Short-cycle tertiary education (ISCED 5)"] <- 
  "Upper secondary / short tertiary education"
levels(rel_data_add$education)[levels(rel_data_add$education) == "Bachelor or equivalent (ISCED 6)"|
                             levels(rel_data_add$education) == "Master or equivalent (ISCED 7)"|
                             levels(rel_data_add$education) == "Doctoral or equivalent (ISCED 8)"] <- 
  "University - level education, with degree"
rel_data_add$education <- droplevels(rel_data_add$education)


# Make age and year numeric
rel_data_add$year <- droplevels(rel_data_add$year)
for (item in c("age", "year")) {
  rel_data_add[, item] <- as.numeric(as.character(rel_data_add[, item]))
}

# Recode the remaining variables into numeric
for (item in c("imprel", "confidence", "attend", "person", "bgod", "bhell", "impgod", 
               "education", "income", "member", "belong", "pray")) {
  rel_data_add[, item] <- as.numeric(rel_data_add[, item])
}

# Reverse recode all variables except impgod and member
## They do not need to be recoded - higher scores already correspond to higher religiosity
for (item in c("imprel", "confidence")) {
  rel_data_add[,item] <- Recode(rel_data_add[,item], rec= "1=4; 2=3; 3=2; 4=1; else=NA")
}

for (item in c("bgod", "bhell")) {
  rel_data_add[,item] <- Recode(rel_data_add[,item], rec= "1=2; 2=1; else=NA")
}

rel_data_add$attend <- Recode(rel_data_add$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
rel_data_add$pray <- Recode(rel_data_add$pray, rec = "1=8; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1; else=NA")

# Make person binary
rel_data_add$person <- Recode(rel_data_add$person, rec = "1=2; 2=1; 3=1; else=NA")

# Make belonging binary
rel_data_add$belong <- ifelse(rel_data_add$belong == "1", 1, 2)

# -------------

# IMPUTATION

pred_matrix <- make.predictorMatrix(data = rel_data_add)
pred_matrix[, c("country", "code", "year")] <- 0 
pred_matrix[c("country", "code", "year"), ] <- 0

imp_method <- make.method(data = rel_data_add)
imp_method[c("person", "belong", "gender", "bgod", "bhell")] <- "logreg" 
imp_method[c("confidence", "member", "imprel")] <- "polr" 
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

imp_data_add <- split(rel_data_add, rel_data_add$country)

for (i in 1:length(imp_data_add)){
  for (item in c("person", "belong", "bgod", "bhell", "confidence", "member", "imprel")) {
    imp_data_add[[i]][, item] <- as.factor(imp_data_add[[i]][, item])
  }
  
  imp_data_add[[i]] <- mice(data = imp_data_add[[i]], m = 5, #5 imputed datasets
                            maxit = 100, #100 iterations
                            meth = imp_method, predictorMatrix = pred_matrix, 
                            seed = 12345, #to replicate the results
                            print = F)
}

# Diagnostics
plot(imp_data_add$`Hong Kong`)
# for continuous variables
densityplot(imp_data_add$`Hong Kong`)
# or 
bwplot(imp_data_add$`Hong Kong`)

# Make five imputed datasets for each country in a list
for (i in 1:length(imp_data_add)){
  imp_data_add[[i]] <- complete(imp_data_add[[i]], action = "all") 
}

# Make final imputed datasets
for (i in 1:length(imp_data_add)){
  
  for (a in 1:length(imp_data_add[[i]])){
    # Drop all variables used for imputation only
    # Specify the same order of columns for all datasets
    imp_data_add[[i]][[a]] <- imp_data_add[[i]][[a]][, c("country", "imprel", "confidence", "belong", "attend",
                                                 "pray", "person", "year", "code")]
    
    imp_data_add[[i]][[a]]$country <- droplevels(imp_data_add[[i]][[a]]$country)
    
    # Recode factor variables into numeric
    for (item in c("person", "belong", "confidence", "imprel")) {
      imp_data_add[[i]][[a]][, item] <- as.numeric(imp_data_add[[i]][[a]][, item])
    }
    
  }
  
}

imp_data_add <- imp_data_add[order(names(imp_data_add))]

# -------------

# CFA

# Save only the countries with acceptable model fit
cfa_model_add <- groupwiseCFA.mi.sep(model, data = imp_data_add, ordered = ord_items, 
                                     estimator = "WLSMV", out = c("goodfit"))

# Compute correlations between variables
## and drop countries with negative correlations or collinear variables 
## from the final samples of the CFA model (if necessary)
imp_data_cor_add <- imp_data_add
for (i in 1:length(imp_data_cor_add)) {
  
  for (a in 1:length(imp_data_cor_add[[i]])) { 
    # Compute correlations that are used in CFA analyses
    imp_data_cor_add[[i]][[a]] <- lavCor(as.data.frame(
      imp_data_cor_add[[i]][[a]][, c("person", "belong", "confidence", "imprel", 
                                 "pray", "attend")]), ordered = ord_items, output = "cor")
    
    # Remove diagonal variances
    imp_data_cor_add[[i]][[a]] <- diag.remove(imp_data_cor_add[[i]][[a]], remove.val = NA)
    
    # Find negative correlations
    imp_data_cor_add[[i]][[a]] <- ifelse(imp_data_cor_add[[i]][[a]] < 0, 
                                         imp_data_cor_add[[i]][[a]], NA)
    imp_data_cor_add[[i]][[a]] <- sum(!is.na(imp_data_cor_add[[i]][[a]]))
    
  }
  
}

lapply(imp_data_cor_add, function(x) 
  lapply(x, function(y)
    y == 0))

# No negative correlations

# Make five final imputed datasets
data1 <- lapply(imp_data_add, function(x) x[[1]]) %>% do.call("rbind", .) 
data2 <- lapply(imp_data_add, function(x) x[[2]]) %>% do.call("rbind", .) 
data3 <- lapply(imp_data_add, function(x) x[[3]]) %>% do.call("rbind", .) 
data4 <- lapply(imp_data_add, function(x) x[[4]]) %>% do.call("rbind", .) 
data5 <- lapply(imp_data_add, function(x) x[[5]]) %>% do.call("rbind", .) 

data_list_add <- list(data1, data2, data3, data4, data5)
rm(data1, data2, data3, data4, data5)

# Additinonal sample for model 2 - select 2 countries with acceptable fit: Hong Kong and Puerto Rico
mlsem_dat_add <- lapply(data_list_add, function(x) 
  subset(x, subset = x$country %in% rownames(cfa_model_add)))

for (i in 1:length(mlsem_dat_add)) {
  mlsem_dat_add[[i]]$country <- droplevels(mlsem_dat_add[[i]]$country)
}

# -------------

# COUNTRY-LEVEL PREDICTORS

# Religious composition
# Subset countries and the most recent available 2015 year
rel_compos_add <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% rel_data_add$code) & 
                                              rcs_data$YEAR == 2015))

# Select variables
rel_compos_add <- select(rel_compos_add, c(NUMISO, YEAR, CATPC, XPRTPC, ORTPC, 
                                           OCHRPC, CSYNPC, UPRTPC, 
                                           ULCHPC, ECCPC, LDSPC, OECCPC, NTCPC, JWTPC, UNIPC, ONTCPC, OLCHPC,
                                           MUSPC, MSYNPC, 
                                           HINPC,  
                                           BUDPC, BSYNPC,
                                           SHNPC, CNFPC, TAOPC, CHFPC, 
                                           NREPC, UNKPC, 
                                           JEWPC, MANPC, ZORPC, BAHPC, JAIPC, SIKPC, INDPC, NEWPC, OREPC,
                                           UCHRPC))  
names(rel_compos_add)[names(rel_compos_add) == "NUMISO"] <- "code"
names(rel_compos_add)[names(rel_compos_add) == "YEAR"] <- "year"

# Calculate final country-level predictors
## Followers of Asian Religions
rel_compos_add$RCASIAN <- rowSums(
  rel_compos_add[, c("SHNPC", "CNFPC", "TAOPC", "CHFPC", "JAIPC", ## other Asian religions
                 "HINPC", 
                 "BUDPC", "BSYNPC" ## Buddhists
  )], na.rm=T)


## Christians
rel_compos_add$RCCHR <- rowSums(
  rel_compos_add[,c("CATPC", "XPRTPC", "ORTPC", 
                "ULCHPC", "ECCPC", "LDSPC", "OECCPC", "NTCPC", 
                "JWTPC", "UNIPC", "ONTCPC", "OLCHPC", ## Liminal Christians
                "OCHRPC", "CSYNPC" ## other Christians
  )], na.rm=T)

## Muslims
rel_compos_add$RCMUSLIM <- rowSums(rel_compos_add[,c("MUSPC", "MSYNPC")], na.rm=T)

## Variable with all other categories
rel_compos_add$RCOTHER <- rowSums(
  rel_compos_add[,c("NREPC", "UNKPC", 
                "JEWPC", "MANPC", "ZORPC", "BAHPC", "SIKPC",
                "INDPC", "NEWPC", "OREPC" ## other religions
  )], na.rm=T)

# -------------

# Adjust variables
rel_compos_add[is.na(rel_compos_add)] <- 0
rel_compos_add$SUM <- rowSums(
  rel_compos_add[,c("RCASIAN", "RCCHR", "RCMUSLIM", "RCOTHER")], na.rm=T)
rel_compos_add$DIF <- (100 - rel_compos_add$SUM)

rel_compos_add$NMISS <- rowSums(
  rel_compos_add[,c("RCASIAN", "RCCHR", "RCMUSLIM", "RCOTHER")] != 0)

rel_compos_add$DIF1 <- (0 - rel_compos_add$DIF/rel_compos_add$NMISS)
rel_compos_add$NMISS1 <- rowSums(
  rel_compos_add[,c("RCASIAN", "RCCHR", "RCMUSLIM", "RCOTHER")] > rel_compos_add$DIF1)

rel_compos_add$DIF1  <- (0 - rel_compos_add$DIF/rel_compos_add$NMISS1)
rel_compos_add$NMISS1 <- rowSums(
  rel_compos_add[,c("RCASIAN", "RCCHR", "RCMUSLIM", "RCOTHER")] > rel_compos_add$DIF1)

rel_compos_add$DIF1  <- (0 - rel_compos_add$DIF/rel_compos_add$NMISS1)

# Subtract 
for (item in c("RCASIAN", "RCCHR", "RCMUSLIM", "RCOTHER")) {
  rel_compos_add[, item] <- ifelse((rel_compos_add[, item] > rel_compos_add$DIF1),
                                   (rel_compos_add[, item] - rel_compos_add$DIF1), 
                                   rel_compos_add[, item])
}

rel_compos_add <- merge(rel_compos_add, rel_data_add[, c(1, 17)], by = c("code"))
rel_compos_add <- rel_compos_add[!duplicated(rel_compos_add$code), ]
rel_compos_add <- trim(rel_compos_add)


# Merge imputed data with country-level predictors
mlsem_dat_add <- lapply(mlsem_dat_add, function(y) 
  Reduce(function(x, z) merge(x, z, by = c("country"), all.x = F),
         list(y, rel_compos_add[, c("country", "RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER")], 
              communism[, c("country", "COMMALL")], 
              zones[, c("country", "ZAFRICA", "ZLA", "ZINDIC", "ZSINIC",
                        "ZNWEST", "ZISLAM", "ZORT", "ZEUWEST")])))


# Add the empty columns for RRI and RLI
## Set the same order of columns as in the mlsem_dat main datasets
## Combine the two datasets
for (i in 1:length(mlsem_dat_add)) {
  mlsem_dat_add[[i]]$RRI <- NA
  mlsem_dat_add[[i]]$RLI <- NA
  
  mlsem_dat_add[[i]] <- mlsem_dat_add[[i]][colnames(mlsem_dat[[i]])]
  
  mlsem_dat_add[[i]] <- rbind(mlsem_dat[[i]], mlsem_dat_add[[i]])
}


# MGCFA to ensure the similarity of the factor structure
mgcfa_model_add <- globalMI.mi.sep(model, data = mlsem_dat_add, ordered = ord_items, 
                                   estimator = "WLSMV", group = "country")
df_to_viewer(mgcfa_model_add, rownames = F)
# CHI.sq	df	CFI	RMSEA	SRMR
# [1464.24; 1534.33]	308	.993	[0.051; 0.053]	.027


# Save data for Mplus analyses
for (i in 1:length(mlsem_dat_add)) {
  prepareMplusData(mlsem_dat_add[[i]], filename = paste0("mlsem_dat_add", i, ".dat"))
}

# -------------

# Retrieve priors for thresholds of the two categorical indicators
priors_add <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/01_Priors/02_MLR_StV", 
  recursive = T, what = "parameters")

for (i in 1:length(priors_add)) {
  priors_add[[i]] <- priors_add[[i]]$parameters$unstandardized
  
  # Select estimates only for thresholds
  priors_add[[i]] <- subset(priors_add[[i]],
                        subset = (priors_add[[i]]$param %in%
                                    c("IMPREL$1", "IMPREL$2", "IMPREL$3",
                                      "CONFIDEN$1", "CONFIDEN$2", "CONFIDEN$3")),
                        select = c("param", "est"))
  priors_add[[i]] <- as.data.frame(priors_add[[i]])
  
}

names(priors_add) <- 1:5

# Merge datasets to a single table
priors_add <- Reduce(
  function(x, y) merge(x, y, by = "param", all = TRUE),
  priors_add
)
## Ignore the warnings, they are related to the arguments' format only

colnames(priors_add) <- c("Parameter", "Data 1", "Data 2", "Data 3", 
                      "Data 4", "Data 5")

# Recode the names of parameters
priors_add$Parameter <- as.factor(priors_add$Parameter)
priors_add$Parameter <- car::Recode(priors_add$Parameter,
                                as.factor = T,
                                recodes = 
                                  "'CONFIDEN$1' = 'Confidence in institutions - t1';
                                   'CONFIDEN$2' = 'Confidence in institutions - t2';
                                   'CONFIDEN$3' = 'Confidence in institutions - t3';
                                   'IMPREL$1' = 'Importance of religion - t1';
                                   'IMPREL$2' = 'Importance of religion - t2';
                                   'IMPREL$3' = 'Importance of religion - t3'")

df_to_viewer(priors_add, rownames = F)

# ----------------------

# DIAGNOSTICS

# PSR values
psr_values_add <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify names of hypotheses/models
names(psr_values_add) <- mod_names[1:15]

for (i in 1:length(psr_values_add)) {
  psr_values_add[[i]] <- psr_values_add[[i]]$tech8$psr
  
  psr_values_add[[i]] <- subset(psr_values_add[[i]], psr_values_add[[i]]$psr > 1.1)
  psr_values_add[[i]] <- max(psr_values_add[[i]]$iteration)
  
}

# --------------

# Traceplots, autocorrelation plots

plots_add <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify directions
direct_add <- lapply(names(plots_add), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(direct_add)) {
  direct_add[[i]] <- direct_add[[i]][-1]
  direct_add[[i]] <- direct_add[[i]][1:(length(direct_add[[i]]) - 2)]
  
  direct_add[[i]] <- paste(direct_add[[i]], collapse = "/")
  
  direct_add[[i]] <- paste0("/", direct_add[[i]])
}

# Specify file names
nfile_add <- lapply(names(plots_add), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(nfile_add)) {
  nfile_add[[i]] <- paste0(nfile_add[[i]][length(nfile_add[[i]]) - 1],
                       ".gh5")
}

# Create pdf files with all plots
for (i in 1:length(direct_add)) {
  setwd(direct_add[[i]])
  traceplots_mplus(nfile_add[[i]], is.file = T)
}

# ===================================================================================================
# APPENDIX

# Table 2. Sample size and survey wave, by country
n_tab <- data.frame(table(rel_data$country))
n_tab$Wave <- ifelse((n_tab$Var1 %in% levels(rel_data_WVS6$country)),
                     "WVS 6", "WVS 7")
colnames(n_tab) <- c("Country", "N", "Wave")

n_tab <- trim(n_tab)
n_tab <- n_tab[order(n_tab$Country), ]
df_to_viewer(n_tab, rownames = F)

# ----------------------------

# Table 4. Denominational composition, by country (%)
df_to_viewer(
  rel_compos[, c("country", "year", "RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER")], 
  rownames = F, 
  digits = 0)

# ----------------------------

# Table 6. Country-level estimates, by country
tab_predict <- Reduce(function(x, y) 
  merge(x, y, all = F), 
  list(zones[, c("country", "Zone")], 
       communism[, c("country", "COMMALL")],
       rel_regulation[, c("country", "RRI", "RLI", "year")]))

colnames(tab_predict) <- c("Country", "Zone", "Communist", "RRI", "RLI", "Year")

# Recode the name of predictors to full and more intuitive ones
tab_predict$Zone[tab_predict$Zone == "ZAFRICA"] <- "Sub-Saharan Africa"
tab_predict$Zone[tab_predict$Zone == "ZLA"] <- "Latin America"
tab_predict$Zone[tab_predict$Zone == "ZINDIC"] <- "Indic East"
tab_predict$Zone[tab_predict$Zone == "ZSINIC"] <- "Sinic East"
tab_predict$Zone[tab_predict$Zone == "ZISLAM"] <- "Islamic East"
tab_predict$Zone[tab_predict$Zone == "ZNWEST"] <- "New West"
tab_predict$Zone[tab_predict$Zone == "ZOLDWEST"] <- "European West"
tab_predict$Zone[tab_predict$Zone == "ZRETWEST"] <- "European West"
tab_predict$Zone[tab_predict$Zone == "ZREFWEST"] <- "European West"
tab_predict$Zone[tab_predict$Zone == "ZORT"] <- "Orthodox East"

# Set "+" for (ex-)communist countries
tab_predict$Communist <- ifelse(
  tab_predict$Communist == 1, "+", 
  ""
)

df_to_viewer(tab_predict, rownames = F)

# ----------------------------

# Table 7. Percentage of missing observations for each religiosity indicator, by country
mis_tab <- aggregate(.~country,
                     rel_data[, c("country", "imprel", "confidence", "belong", "attend",
                                  "pray", "person")], 
                     FUN = function(x) 
                       mean(is.na(x))*100, na.action=NULL)

# For countries with omitted questions
mis_tab[mis_tab==100] <- NA

# Calculate means for countries
mis_tab$Mean <- apply(mis_tab[, 2:7], 1, function(x)
  mean(x, na.rm = T))

# Calculate means for variables
mis_tab[nrow(mis_tab) + 1, ] <- NA
mis_tab[nrow(mis_tab), 2:8] <- apply(mis_tab[, 2:8], 2, function(x)
  mean(x, na.rm = T))

mis_tab[, 2:8] <- round(mis_tab[, 2:8], 0)

mis_tab <- trim(mis_tab)
mis_tab <- mis_tab[order(mis_tab$country), ]
mis_tab[, 2:8][is.na(mis_tab[, 2:8])] <- "--"

colnames(mis_tab) <- c("Country", "Importance of religion", 
                       "Confidence in institutions", "Belonging to a denomination",
                       "Frequency of religious attendance", "Frequency of praying",
                       "Identification as religious person", "Mean")
df_to_viewer(mis_tab, rownames = F)

# ----------------------------

# Table 8. Fit measures of the factor model across five imputed datasets, by country
cfa_model_all <- groupwiseCFA.mi.sep(model, data = imp_data, ordered = ord_items, 
                                     estimator = "WLSMV", out = c("fit"))

# ----------------------------

# Table 10. Fit measures for configural model across five imputed datasets
df_to_viewer(mgcfa_model, rownames = F)

# ----------------------------

# Table 11. Mean (sd) for continuous country-level predictors
cont_tab <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER", "RRI", "RLI")) %>%
  sapply(., function(x)
    paste0(
      round(
        mean(x, na.rm = T), 2), " (", round(
          sd(x, na.rm = T), 2), ")"
    ))

cont_tab <- as.data.frame(cont_tab)
rownames(cont_tab) <- c("Christians", "Muslims", "Followers of Asian religions", "Others", 
                        "RRI", "RLI")
cont_tab <- cbind(rownames(cont_tab), cont_tab)

colnames(cont_tab) <- c("Predictor", "Mean (SD)")

kable(cont_tab, row.names = FALSE) %>%
  group_rows("Religious composition", 1, 4) %>%
  group_rows("Regulation of religion", 5, 6) %>%
  footnote(
    general = "Others = the sum share of not religious, individuals with unknown classification, 
    and the followers of all 'other' religions: Jews, Mandaeans, Zoroastrians, Bahais, Sikhs, 
    indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    The descriptive statistics were computed for the sample of 42 countries."
  )

# ----------------------------

# Table 12. Number (share) of countries for binary country-level predictors
bin_tab <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("ZAFRICA", "ZLA", "ZINDIC", "ZSINIC", "ZISLAM", "ZNWEST", 
              "ZORT", "ZEUWEST", 
              "COMMALL")) %>%
  sapply(., function(x)
    paste0(length(which(x==1)), " (", 
           round(
             length(which(x==1))/length(x)*100, 0), 
           "%)"))

bin_tab <- as.data.frame(bin_tab)

rownames(bin_tab) <- c("Sub-Saharan Africa", "Latin America", "Indic East",
                       "Sinic East", "Islamic East", "New West", 
                       "Orthodox East", "European West",
                       "Communist")
bin_tab <- cbind(rownames(bin_tab), bin_tab)

colnames(bin_tab) <- c("Predictor", "Number (share)")

kable(bin_tab, row.names = FALSE) %>%
  footnote(
    general = "The descriptive statistics were computed for the sample of 42 countries."
  )


# ----------------------------

# Table 13. Correlations of country-level predictors

# Correlations of continuous predictors - Pearson
cont_tab_cor <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER", "RRI", "RLI")) %>%
  cor(., method = "pearson", use = "pairwise.complete.obs")

cont_tab_cor <- as.data.frame(as.table(cont_tab_cor))
cont_tab_cor <- cont_tab_cor[cont_tab_cor$Freq != 1, ]
cont_tab_cor <- cont_tab_cor[!duplicated(cont_tab_cor$Freq), ]

cont_tab_cor$Freq <- round(cont_tab_cor$Freq, 2)

colnames(cont_tab_cor) <- c("Predictor1", "Predictor2", "Correlation")

# Correlations of continuous predictors with Communism - Spearman
bin_tab_cor <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("COMMALL",
              "RCCHR", "RCMUSLIM", "RCASIAN", "RCOTHER", "RRI", "RLI"))

bin_tab_cor <- round(
  cor(bin_tab_cor[, 1], bin_tab_cor[, -1], 
      method = "spearman", use = "pairwise.complete.obs"), 2)

bin_tab_cor <-  as.data.frame(as.table(bin_tab_cor))
bin_tab_cor <- bin_tab_cor[bin_tab_cor$Freq != 1, ]
colnames(bin_tab_cor) <- c("Predictor1", "Predictor2", "Correlation")
bin_tab_cor$Predictor1 <- "COMMALL"

corr_tab <- rbind(cont_tab_cor, bin_tab_cor)
corr_tab <- trim(corr_tab)

## Exclude non-valid pairs 
corr_tab <- dplyr::filter(corr_tab,
                          !((Predictor1 == "RCOTHER" & 
                               Predictor2 == "RCCHR")|
                              (Predictor1 == "RCMUSLIM" & 
                                 Predictor2 == "RCCHR")|
                              (Predictor1 == "RCASIAN" & Predictor2 == "RCCHR")
                          )
)

# Recode names of predictors
corr_tab$Predictor1 <- car::Recode(corr_tab$Predictor1,
                                   as.factor = T,
                                   recodes = "'RCASIAN' = 'Followers of Asian religions';
                                   'COMMALL' = 'Communist';
                                   'RCOTHER' = 'Others'")

corr_tab$Predictor2 <- car::Recode(corr_tab$Predictor2,
                                   as.factor = T,
                                   recodes = "'RCASIAN' = 'Followers of Asian religions';
                                   'RCCHR' = 'Christians';
                                   'RCOTHER' = 'Others';
                                   'RCMUSLIM' = 'Muslims'")

corr_tab <- corr_tab[order(corr_tab[, 1]), ] 
df_to_viewer(corr_tab, rownames = F, digits = 2)

#----------------------------

# Specification of priors for MLSEM: 
## Table 1. Means of parameters in normal priors for Bayesian MLSEM

# Retrieve priors for thresholds of the two categorical indicators
priors <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/01_Priors/02_MLR_StV", 
  recursive = T, what = "parameters")

for (i in 1:length(priors)) {
  priors[[i]] <- priors[[i]]$parameters$unstandardized
  
  # select estimates only for thresholds
  priors[[i]] <- subset(priors[[i]],
                        subset = (priors[[i]]$param %in%
                                    c("IMPREL$1", "IMPREL$2", "IMPREL$3",
                                      "CONFIDEN$1", "CONFIDEN$2", "CONFIDEN$3")),
                        select = c("param", "est"))
  priors[[i]] <- as.data.frame(priors[[i]])
  
}

names(priors) <- 1:5

# Merge datasets to a single table
priors <- Reduce(
  function(x, y) merge(x, y, by = "param", all = TRUE),
  priors
)
## Ignore the warnings, they are related to the arguments' format only

colnames(priors) <- c("Parameter", "Data 1", "Data 2", "Data 3", 
                      "Data 4", "Data 5")

# Recode the names of parameters
priors$Parameter <- as.factor(priors$Parameter)
priors$Parameter <- car::Recode(priors$Parameter,
                                as.factor = T,
                                recodes = 
                                  "'CONFIDEN$1' = 'Confidence in institutions - t1';
                                   'CONFIDEN$2' = 'Confidence in institutions - t2';
                                   'CONFIDEN$3' = 'Confidence in institutions - t3';
                                   'IMPREL$1' = 'Importance of religion - t1';
                                   'IMPREL$2' = 'Importance of religion - t2';
                                   'IMPREL$3' = 'Importance of religion - t3'")

df_to_viewer(priors, rownames = F)

# ----------------------------

# Sensitivity analysis: 
## Table 1. Sensitivity analysis for parameters of the random loading models
## Table 2. Sensitivity analysis for parameters of the random intercept models

# Retrieve the estimates with defaul priors for thresholds
estimates_def <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/03_DefaultPriors",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

for (i in 1:length(estimates_def)) {
  
  estimates_def[[i]] <- estimates_def[[i]]$bparameters$valid_draw
  
  estimates_def[[i]] <- as.data.frame(do.call("rbind", estimates_def[[i]]))
  
  # Delete chain and iteration numbers
  estimates_def[[i]] <- estimates_def[[i]][-c(1, 2)]
  
}

# Specify names of parameters
par_names <- estimates_def
for (i in 1:length(par_names)) {
  par_names[[i]] <- lapply(names(par_names[[i]]), function(x)
    strsplit(x, "[.]")[[1]])
  
  # Delete "Parameter" word
  par_names[[i]] <- lapply(par_names[[i]], function(x) x[-1]) 
  
  for (a in 1:length(par_names[[i]])) {
    
    # Delete number of each parameter
    par_names[[i]][[a]][1] <- gsub("[[:digit:]]", "", par_names[[i]][[a]][1])
    
    # Delete "_" symbol of each parameter
    par_names[[i]][[a]][1] <- gsub("_", "", par_names[[i]][[a]][1])
    
    # Delete "%" symbol of each parameter
    par_names[[i]][[a]][1] <- gsub("%", "", par_names[[i]][[a]][1])
    
    # Delete "(equality/label)" in the names of thresholds
    par_names[[i]][[a]][length(par_names[[i]][[a]])] <- 
      gsub("[(equality/label)]", "", par_names[[i]][[a]][length(par_names[[i]][[a]])])
    
   

  }
  
  par_names[[i]] <- lapply(par_names[[i]], function(x) paste(x, collapse = "."))
  
}

for (i in 1:length(estimates_def)) {
  names(estimates_def[[i]]) <- par_names[[i]]
}

# Combine datasets for each hypotheses
estimates_def <- tapply(estimates_def, 
                     rep(1:(length(estimates_def)/5), each = 5), function(x)
                       bind_rows(x, .id = NULL))

names(estimates_def) <- mod_names[seq(1, length(mod_names), 5)]

# Estimates with the final informative prior
estimates_fin <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus//02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Retrieve the estimates for all parameters
for (i in 1:length(estimates_fin)) {
  
  estimates_fin[[i]] <- estimates_fin[[i]]$bparameters$valid_draw
  
  estimates_fin[[i]] <- as.data.frame(do.call("rbind", estimates_fin[[i]]))
  
  # Delete chain and iteration numbers
  estimates_fin[[i]] <- estimates_fin[[i]][-c(1, 2)]
  
}

# Set the names for parameters
for (i in 1:length(estimates_fin)) {
  names(estimates_fin[[i]]) <- par_names[[i]]
}


# Combine datasets for each hypotheses
estimates_fin <- tapply(estimates_fin, 
                     rep(1:(length(estimates_fin)/5), each = 5), function(x)
                       bind_rows(x, .id = NULL))

names(estimates_fin) <- mod_names[seq(1, length(mod_names), 5)]


# Calculate the discrepancy between the estimates
discr <- estimates_def
for (i in 1:length(discr)) {
  # The formula from the Appendix
  discr[[i]] <- (estimates_def[[i]] - estimates_fin[[i]])/estimates_def[[i]]*100
  
  # Median values for discrepancy
  discr[[i]] <- apply(discr[[i]], 2, function(x)
    round(median(x, na.rm = T), 3))
  
  discr[[i]] <- as.data.frame(discr[[i]])
  
  # Create column for parameter names
  par_names_empt <- as.data.frame(rownames(discr[[i]]))
  
  discr[[i]] <- cbind(par_names_empt, discr[[i]])
  colnames(discr[[i]]) <- c("Parameter", "Discr")
  
  
}

# Calculate median and sd for main estimates
## Default priors
for (i in 1:length(estimates_def)) {
  
  estimates_def[[i]] <- apply(estimates_def[[i]], 2, function(x)
    paste0(round(median(x), 3), " (", round(sd(x), 3), ")"))
  
  estimates_def[[i]] <- as.data.frame(estimates_def[[i]])
  
  # Create column for parameter names
  par_names_empt <- as.data.frame(rownames(estimates_def[[i]]))
  
  estimates_def[[i]] <- cbind(par_names_empt, estimates_def[[i]])
  colnames(estimates_def[[i]]) <- c("Parameter", "Default")
}

## Final priors
for (i in 1:length(estimates_fin)) {
  
  estimates_fin[[i]] <- apply(estimates_fin[[i]], 2, function(x)
    paste0(round(median(x), 3), " (", round(sd(x), 3), ")"))
  
  estimates_fin[[i]] <- as.data.frame(estimates_fin[[i]])
  
  # Create column for parameter names
  par_names_empt <- as.data.frame(rownames(estimates_fin[[i]]))
  
  estimates_fin[[i]] <- cbind(par_names_empt, estimates_fin[[i]])
  colnames(estimates_fin[[i]]) <- c("Parameter", "Inform")
}

# Combine estimastes into one table
sens_tab <- estimates_fin
for (i in 1:length(sens_tab)) {
  sens_tab[[i]] <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                          list(estimates_def[[i]], estimates_fin[[i]], discr[[i]]))
  
  colnames(sens_tab[[i]]) <- c("Parameter",
                               paste(colnames(sens_tab[[i]][2:4]), 
                                     names(sens_tab[i]), sep = "_"))
  
}


# Substitute "WITHIN:.ATTEND.ON.REL_W" by "WITHIN:.REL_W.BY.ATTEND" 
# as it is for other indicators for the all RI datasets except RI_SIMPLE

# Also substitute "BETWEEN:.ATTEND.ON.REL_B" by "BETWEEN:.REL_B.BY.ATTEND" 
# as it is for other indicators in all datasets

for (i in 1:length(sens_tab)) {
  sens_tab[[i]]$Parameter[
    sens_tab[[i]]$Parameter == "WITHIN:.ATTEND.ON.REL_W"] <-
    "WITHIN:.REL_W.BY.ATTEND"
  
  sens_tab[[i]]$Parameter[
    sens_tab[[i]]$Parameter == "BETWEEN:.ATTEND.ON.REL_B"] <-
    "BETWEEN:.REL_B.BY.ATTEND"
}

# -------------

# Make two separate tables for loading and intercept models
## RS
sens_tab_RS <- Reduce(
  function(x, y) merge(x, y, by = c("Parameter"), all = TRUE),
  sens_tab[5:8])

## from 1 to 39 row - between level parameters
## from 40 to 48 row - within level parameters

## Delete the level of each parameter
sens_tab_RS$Parameter <- gsub("BETWEEN:.", "", sens_tab_RS$Parameter)
sens_tab_RS$Parameter <- gsub("WITHIN:.", "", sens_tab_RS$Parameter)
sens_tab_RS$Parameter <- gsub("_B", "", sens_tab_RS$Parameter)
sens_tab_RS$Parameter <- gsub("_W", "", sens_tab_RS$Parameter)

## Substitute "MEAN" for intercepts  and random loading
sens_tab_RS$Parameter <- gsub("MEAN.ATTEND", "INT.ATTEND", sens_tab_RS$Parameter)
sens_tab_RS$Parameter <- gsub("MEAN.PRAY", "INT.PRAY", sens_tab_RS$Parameter)
sens_tab_RS$Parameter <- gsub("MEAN.SLOPE", "EST.SLOPE", sens_tab_RS$Parameter)

## Delete "MEAN" for thresholds 
sens_tab_RS$Parameter <- gsub("MEAN.", "", sens_tab_RS$Parameter)

# Format the name of models - detele the number of imputed dataset
colnames(sens_tab_RS) <- gsub("_1", "", colnames(sens_tab_RS))

kable(sens_tab_RS, 
      row.names = FALSE) %>%
  add_header_above(c(`Parameter` = 1, `M1` = 3, `M2` = 3, `M3` = 3, `Simple` = 3)) %>%
  group_rows("Between-country level", 1, 39) %>%
  group_rows("Within-country level", 40, 48) %>%
  footnote(
    general = "Discr = Discrepancy, in %, computed as
    [(initial prior specification – subsequent prior specification)/initial prior specification] *100;
    Default = Defaul prior N(0, 5); Inform = Informative prior N(MLR estimates, 0.001), 
    Simple = Model with no country-level predictors to retrieve initial variance of random parameter.
    For the list of models, see Table 14 in the Appendix. 
    ATTEND = Frequency of religious attendance indicator;
    BELONG = Belonging to a denomination indicator;
    CONFIDENCE = Confidence in institutions indicator;
    IMPREL = Importance of religion indicator;
    PERSON = Identification as a religious person indicator;
    PRAY = Frequency of praying indicator; 
    SLOPE = Random loading;
    REL = Religiosity factor;
    RES1 = Residual covariance factor for frequency of attendance with frequency of praying;
    RES2 = Residual covariance factor for belonging to a denomination with identification as a religious person;
    Stand alone indicator name/SLOPE = (Residual) Variance of indicator/slope;
    Stand alone factor name = Factor variance;
    REL.BY = Factor loading of indicator;
    SLOPE.ON.COMMALL = Random loading regressed on the Communist background predictor; 
    SLOPE.ON.RCASIAN = Random loading regressed on the percept of the followers of Asian religions predictor; 
    SLOPE.ON.RCMUSLIM = Random loading regressed on the percept of Muslims predictor;
    SLOPE.ON.RCOTHER = Random loading regressed on the percept of non-affiliated and followers of 'other' religions predictor; 
    SLOPE.ON.RLI = Random loading regressed on RLI predictor;
    SLOPE.ON.RRI = Random loading regressed on RRI predictor;
    SLOPE.ON.ZAFRICA = Random loading regressed on Sub-Saharan African zone predictor; 
    SLOPE.ON.ZINDIC = Random loading regressed on Indic East zone predictor; 
    SLOPE.ON.ZISLAM = Random loading regressed on Islamic East zone predictor; 
    SLOPE.ON.ZLA = Random loading regressed on Latin American zone predictor; 
    SLOPE.ON.ZNWEST = Random loading regressed on New West zone predictor; 
    SLOPE.ON.ZORT = Random loading regressed on Orthodox East zone predictor; 
    SLOPE.ON.ZSINIC = Random loading regressed on Sinic East zone predictor; 
    INT = Intercept of indicator;
    EST.SLOPE = Estimate of the random loading;
    $1:3 = First-third threshold of indicator."
  )


# RI
sens_tab_RI <- Reduce(
  function(x, y) merge(x, y, by = c("Parameter"), all = TRUE),
  sens_tab[1:4])

## from 1 to 35 row - between level parameters
## from 36 to 45 row - within level parameters

## Delete the level of each parameter
sens_tab_RI$Parameter <- gsub("BETWEEN:.", "", sens_tab_RI$Parameter)
sens_tab_RI$Parameter <- gsub("WITHIN:.", "", sens_tab_RI$Parameter)
sens_tab_RI$Parameter <- gsub("_B", "", sens_tab_RI$Parameter)
sens_tab_RI$Parameter <- gsub("_W", "", sens_tab_RI$Parameter)

## Substitute "MEAN" for intercepts
sens_tab_RI$Parameter <- gsub("MEAN.ATTEND", "INT.ATTEND", sens_tab_RI$Parameter)
sens_tab_RI$Parameter <- gsub("MEAN.PRAY", "INT.PRAY", sens_tab_RI$Parameter)

## Delete "MEAN" for thresholds
sens_tab_RI$Parameter <- gsub("MEAN.", "", sens_tab_RI$Parameter)

# Format the name of models - detele the number of imputed dataset
colnames(sens_tab_RI) <- gsub("_1", "", colnames(sens_tab_RI))

kable(sens_tab_RI, 
      row.names = FALSE) %>%
  add_header_above(c(`Parameter` = 1, `M4` = 3, `M5` = 3, `M6` = 3, `Simple` = 3)) %>%
  group_rows("Between-country level", 1, 35) %>%
  group_rows("Within-country level", 35, 45) %>%
  footnote(
    general = "Discr = Discrepancy, in %, computed as
    [(initial prior specification – subsequent prior specification)/initial prior specification] *100;
    Default = Defaul prior N(0, 5); Inform = Informative prior N(MLR estimates, 0.001), 
    Simple = Model with no country-level predictors to retrieve initial residual variance of random parameter.
    For the list of models, see Table 14 in the Appendix. 
    ATTEND = Frequency of religious attendance indicator;
    BELONG = Belonging to a denomination indicator;
    CONFIDENCE = Confidence in institutions indicator;
    IMPREL = Importance of religion indicator;
    PERSON = Identification as a religious person indicator;
    PRAY = Frequency of praying indicator;
    REL = Religiosity factor;
    RES1 = Residual covariance factor for frequency of attendance with frequency of praying;
    RES2 = Residual covariance factor for belonging to a denomination with identification as a religious person;
    Stand alone indicator name = Residual variance of indicator;
    Stand alone factor name = Factor variance;
    REL.BY = Factor loading of indicator;
    ATTEND.ON.COMMALL = Random intercept regressed on the Communist background predictor; 
    ATTEND.ON.RCASIAN = Random intercept regressed on the percept of the followers of Asian religions predictor; 
    ATTEND.ON.RCMUSLIM = Random intercept regressed on the percept of Muslims predictor;
    ATTEND.ON.ZAFRICA = Random intercept regressed on Sub-Saharan African zone predictor; 
    ATTEND.ON.ZINDIC = Random intercept regressed on Indic East zone predictor; 
    ATTEND.ON.ZISLAM = Random intercept regressed on Islamic East zone predictor; 
    ATTEND.ON.ZLA = Random intercept regressed on Latin American zone predictor; 
    ATTEND.ON.ZNWEST = Random intercept regressed on New West zone predictor; 
    ATTEND.ON.ZORT = Random intercept regressed on Orthodox East zone predictor; 
    ATTEND.ON.ZSINIC = Random intercept regressed on Sinic East zone predictor; 
    INT = Intercept of indicator;
    $1:3 = First-third threshold of indicator."
  )


# ----------------------------

# Doubled number of iterations

# Retrieve the estimates with 20000 iterations
estimates_doubitr <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/04_DoubleInter",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Retrieve the estimates for the parameters of interest: random intercept and slope
for (i in 1:length(estimates_doubitr)) {
  
  estimates_doubitr[[i]] <- estimates_doubitr[[i]]$bparameters$valid_draw #post-burn-in iterations
  
  estimates_doubitr[[i]] <- as.data.frame(do.call("rbind", estimates_doubitr[[i]]))
  
  estimates_doubitr[[i]] <- 
    estimates_doubitr[[i]][, -grep("BETWEEN%:.ATTEND.ON.REL_B", colnames(estimates_doubitr[[i]]))]
  
  estimates_doubitr[[i]] <- 
    estimates_doubitr[[i]][, grepl("BETWEEN%:.ATTEND.ON|BETWEEN%:.SLOPE.ON", 
                                   names(estimates_doubitr[[i]]))]
  
}

# Set the names for models
names(estimates_doubitr) <- mod_names[seq(1, length(mod_names), 5)]

# Set the names for parameters
for (i in 1:length(estimates_doubitr)) {
  names(estimates_doubitr[[i]]) <- pred_names[[i]]
}

# Calculate median and CI for each effect
for (i in 1:length(estimates_doubitr)) {
  estimates_doubitr[[i]] <- apply(estimates_doubitr[[i]], 2, function(x)
    ifelse((quantile(x, 0.025) < 0 & quantile(x, 0.975) > 0),
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3),  ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]"), 
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3), ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]*")
    )
  )
  
  estimates_doubitr[[i]] <- as.data.frame(estimates_doubitr[[i]])
  
  estimates_doubitr[[i]] <- cbind(rownames(estimates_doubitr[[i]]), estimates_doubitr[[i]])
  colnames(estimates_doubitr[[i]]) <- c("Predictor", "Estimate [CI]")
  
  
}

estimates_doubitr_tab <- Reduce(
  function(x, y) merge(x, y, by = c("Predictor"), all = TRUE),
  estimates_doubitr
)

# Ignore the warnings, they are related to the arguments' format only
colnames(estimates_doubitr_tab) <- c("Predictor", names(estimates_doubitr))
estimates_doubitr_tab$RI_SIMPLE_1 <- NULL
estimates_doubitr_tab$RS_SIMPLE_1 <- NULL

# Add the line with the residual variance
res_var_doubitr <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/04_DoubleInter",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Set the names for models
names(res_var_doubitr) <- mod_names[seq(1, length(mod_names), 5)]

# Retrieve the estimates for the parameters of interest: random intercept and slope
for (i in 1:length(res_var_doubitr)) {
  res_var_doubitr[[i]] <- res_var_doubitr[[i]]$bparameters$valid_draw #post-burn-in iterations
  
  res_var_doubitr[[i]] <- as.data.frame(do.call("rbind", res_var_doubitr[[i]]))
  
  res_var_doubitr[[i]] <- 
    res_var_doubitr[[i]][, -grep(".ON.", colnames(res_var_doubitr[[i]]))]
  
  res_var_doubitr[[i]] <- as.data.frame(res_var_doubitr[[i]])
  
  res_var_doubitr[[i]] <- 
    res_var_doubitr[[i]][, grepl("BETWEEN%:.ATTEND|BETWEEN%:.SLOPE", names(res_var_doubitr[[i]]))]
  
}

# Select only slope variance for the models with RS
for (i in 5:length(res_var_doubitr)) {
  res_var_doubitr[[i]] <- res_var_doubitr[[i]][, 1]
}

res_var_doubitr$RI_SIMPLE_1 <- NULL
res_var_doubitr$RS_SIMPLE_1 <- NULL

for (i in 1:length(res_var_doubitr)) {
  res_var_doubitr[[i]] <- paste0(round(median(unlist(res_var_doubitr[[i]])), 3), " [", 
                                 round(as.numeric(quantile(unlist(res_var_doubitr[[i]]), 0.025)), 3), ";", 
                                 round(as.numeric(quantile(unlist(res_var_doubitr[[i]]), 0.975)), 3), "]*")
  
}

res_var_doubitr <- unlist(res_var_doubitr)
res_var_doubitr <- c("Residual variance", res_var_doubitr)
estimates_doubitr_tab <- rbind(estimates_doubitr_tab, res_var_doubitr)

# Recode to full names
estimates_doubitr_tab$Predictor <- as.factor(estimates_doubitr_tab$Predictor)
estimates_doubitr_tab$Predictor <- car::Recode(estimates_doubitr_tab$Predictor,
                                               as.factor = T,
                                               recodes = "'RCASIAN' = 'Percentage of the followers of Asian religions';
                                   'COMMALL' = 'Communist';
                                   'RCOTHER' = 'Percentage of Others'; 
                                   'RCMUSLIM' = 'Percentage of Muslims';
                                   'ZAFRICA' = 'Sub-Saharan Africa';
                                   'ZINDIC' = 'Indic East';
                                   'ZSINIC' = 'Sinic East';
                                   'ZISLAM' = 'Islamic East';
                                   'ZLA' = 'Latin America';
                                   'ZNWEST' = 'New West';
                                   'ZORT' = 'Orthodox East'")

# Put the slope first
estimates_doubitr_tab <- estimates_doubitr_tab[, c(1, 5:7, 2:4)]

# Put communism after religions
estimates_doubitr_tab <- estimates_doubitr_tab[c(2:4, 1, 5:14),]

# Rename the columns with the model numbers
colnames(estimates_doubitr_tab) <- c("Predictor", paste0("M", 1:6))

# Table 15. The effect of doubled number of iterations 
## for parameters of the random loading models
kable(estimates_doubitr_tab[, 1:4], row.names = FALSE) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of Others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all 'other' religions: Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 42 countries."
  )

# Table 16. The effect of doubled number of iterations 
## for parameters of the random intercept models
kable(estimates_doubitr_tab[, c(1, 5:7)], row.names = FALSE) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all other religions: Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 42 countries."
  )


# ----------------------------

# Table 17. MLSEM results: 
## the effects of country-level predictors on random intercept of the indicator 
## on the extended sample

estimates_RI <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Retrieve the estimates_RI for the parameters of interest: random intercept
for (i in 1:length(estimates_RI)) {
  
  estimates_RI[[i]] <- estimates_RI[[i]]$bparameters$valid_draw #post-burn-in iterations
  
  estimates_RI[[i]] <- as.data.frame(do.call("rbind", estimates_RI[[i]]))
  
  estimates_RI[[i]] <- 
    estimates_RI[[i]][, -grep("BETWEEN%:.ATTEND.ON.REL_B", colnames(estimates_RI[[i]]))]
  
  estimates_RI[[i]] <- 
    estimates_RI[[i]][, grepl("BETWEEN%:.ATTEND.ON", names(estimates_RI[[i]]))]
  
}

# Combine datasets for each hypotheses
estimates_RI <- tapply(estimates_RI, 
                    rep(1:(length(estimates_RI)/5), each = 5), function(x)
                      bind_rows(x, .id = NULL))

# Specify names of hypotheses
names(estimates_RI) <- mod_names[seq(1, length(mod_names[1:15]), 5)]

# Specify names of predictors
for (i in 1:length(estimates_RI)) {
  names(estimates_RI[[i]]) <- pred_names[[i]]
}


# Calculate median and CI for each effect
for (i in 1:length(estimates_RI)) {
  estimates_RI[[i]] <- apply(estimates_RI[[i]], 2, function(x)
    ifelse((quantile(x, 0.025) < 0 & quantile(x, 0.975) > 0),
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3),  ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]"), 
           paste0(round(median(x), 3), " [", round(as.numeric(quantile(x, 0.025)), 3), ";", 
                  round(as.numeric(quantile(x, 0.975)), 3), "]*")
    )
  )
  
  estimates_RI[[i]] <- as.data.frame(estimates_RI[[i]])
  
  estimates_RI[[i]] <- cbind(rownames(estimates_RI[[i]]), estimates_RI[[i]])
  colnames(estimates_RI[[i]]) <- c("Predictor", "Estimate [CI]")
  
  
}

estimates_tab_RI <- Reduce(
  function(x, y) merge(x, y, by = c("Predictor"), all = TRUE),
  estimates_RI
)

# Recode to full names
estimates_tab_RI$Predictor <- as.factor(estimates_tab_RI$Predictor)
estimates_tab_RI$Predictor <- car::Recode(estimates_tab_RI$Predictor,
                                       as.factor = T,
                                       recodes = "'RCASIAN' = 'Percentage of the followers of Asian religions';
                                       'COMMALL' = 'Communist';
                                       'RCOTHER' = 'Percentage of Others'; 
                                       'RCMUSLIM' = 'Percentage of Muslims';
                                       'ZAFRICA' = 'Sub-Saharan Africa';
                                       'ZINDIC' = 'Indic East';
                                       'ZSINIC' = 'Sinic East';
                                       'ZISLAM' = 'Islamic East';
                                       'ZLA' = 'Latin America';
                                       'ZNWEST' = 'New West';
                                       'ZORT' = 'Orthodox East'")

# Put communism after religions
estimates_tab_RI <- estimates_tab_RI[c(2:4, 1, 5:11),]

# Rename the columns with the model numbers
colnames(estimates_tab_RI) <- c("Predictor", paste0("M", 4:6))

kable(estimates_tab_RI, row.names = FALSE) %>%
  footnote(
    general = "* - the effect or residual variance is significant: 95 CI does not include 0; 
    Percentage of Others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all 'other religions': Jews, Mandaeans, Zoroastrians, Bahais,  
    Sikhs, indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    For the list of models and reference categories, see Table 14 in the Appendix. 
    N = 44 countries."
  )


# -------------

# DIAGNOSTICS

# PSR values
psr_values_RI <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify names of models
names(psr_values_RI) <- mod_names[seq(1, length(mod_names), 5)]

for (i in 1:length(psr_values_RI)) {
  psr_values_RI[[i]] <- psr_values_RI[[i]]$tech8$psr
  
  psr_values_RI[[i]] <- subset(psr_values_RI[[i]], psr_values_RI[[i]]$psr > 1.1)
  psr_values_RI[[i]] <- max(psr_values_RI[[i]]$iteration)
  
}

# -------------

# Traceplots, autocorrelation plots

plots_RI <- readModels(
  target = "./01_Scripts/02_AnalysisScripts/Mplus/05_RIFullSample/02_Results",
  recursive = TRUE,
  what = "all",
  quiet = TRUE)

# Specify directions
direct_RI <- lapply(names(plots_RI), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(direct_RI)) {
  direct_RI[[i]] <- direct_RI[[i]][-1]
  direct_RI[[i]] <- direct_RI[[i]][1:(length(direct_RI[[i]]) - 2)]
  
  direct_RI[[i]] <- paste(direct_RI[[i]], collapse = "/")
  
  direct_RI[[i]] <- paste0("/", direct_RI[[i]])
}

# Specify file names
nfile_RI <- lapply(names(plots_RI), function(x)
  strsplit(x, "[.]")[[1]])

for (i in 1:length(nfile_RI)) {
  nfile_RI[[i]] <- paste0(nfile_RI[[i]][length(nfile_RI[[i]]) - 1],
                           ".gh5")
}

# Create pdf files with all plots
for (i in 1:length(direct_RI)) {
  setwd(direct_RI[[i]])
  traceplots_mplus(nfile_RI[[i]], is.file = T)
}

