# Project

install.packages("readxl")
 
#download the three files for each year
library(readxl)
data1 <- read.csv("2022VAERSDATA.csv")
VAX <- read.csv("2022VAERSVAX.csv")
symptoms <- read.csv("2022VAERSSYMPTOMS.csv")
 
#remove unnecessary columns from the data file
cleandata1 <- data1[,-c(9,22,26,27)]
 
#merge the files for each year and view
VAERS2022 <- merge(cleandata1, VAX, by = "VAERS_ID")
VAERS2022 <- merge(VAERS2022, symptoms, by = "VAERS_ID")
VAERS2022
 
#look for covid vaccines
VAERS2022new <- VAERS2022[VAERS2022$VAX_TYPE == "COVID19", ]
VAERS2022new
 
table(VAERS2022new$SYMPTOM1)

#now year 2021
data2 <- read.csv("2021VAERSDATA.csv")
VAX2 <- read.csv("2021VAERSVAX.csv")
symptoms2 <- read.csv("2021VAERSSYMPTOMS.csv")
 
#remove unnecessary columns from the data file
cleandata2 <- data2[,-c(9,22,26,27)]
 
#merge the files for each year and view
VAERS2021 <- merge(cleandata2, VAX2, by = "VAERS_ID")
VAERS2021 <- merge(VAERS2021, symptoms2, by = "VAERS_ID")
VAERS2021
 
#look for covid vaccines
VAERS2021new <- VAERS2021[VAERS2021$VAX_TYPE == "COVID19", ]
VAERS2021new

#now year 2020
data3 <- read.csv("2020VAERSDATA.csv")
VAX3 <- read.csv("2020VAERSVAX.csv")
symptoms3 <- read.csv("2020VAERSSYMPTOMS.csv")
 
#remove unnecessary columns from the data file
cleandata3 <- data3[,-c(9,22,26,27)]
 
#merge the files for each year and view
VAERS2020 <- merge(cleandata3, VAX, by = "VAERS_ID")
VAERS2020 <- merge(VAERS2020, symptoms, by = "VAERS_ID")
VAERS2020
 
#look for covid vaccines
VAERS2020new <- VAERS2020[VAERS2020$VAX_TYPE == "COVID19", ]
VAERS2020new

#merge all three years together
covid_data<-rbind(VAERS2022, VAERS2021, VAERS2020)

#look at age group distributions
age_covid<- table (covid_data$AGE_YRS)
age_covid
barplot(age_covid, main="side effects by age",
        xlab="age")
 
#make age groups
covid_data$AGE_GROUP<-cut(covid_data$AGE_YRS,
                        breaks = c(17 ,25 ,30 ,35 ,40 ,45 ,50 ,55 ,60 ,65 ,70 ,75 ,80 ,85
                                  , Inf),
                        labels = c( "0-17 years", "18-25 years", "26-30 years"
                                   , "31-35 years", "36-40 years", "41-45 years"
                                   , "46-50 years", "51-55 years", "56-60 years"
                                   ,"61-65 years", "66-70 years", "71-75 years"
                                   , "81-85 years", "86+ years"),
                        right = FALSE)

#export Covid19 data file
write.csv(covid_data, "/Users/Kellie/Documents/covid_data.csv")

#Create control groups and write to csv file. 
FLU <- covid_data[str_detect(covid_data$VAX_TYPE, "FLU"),]
write.csv(FLU, "FLUVAERS.csv")
SHINGLES <- covid_data[str_detect(covid_data$VAX_TYPE, "VARZOS"),]
write.csv(SHINGLES,"SHINGLES.csv")

#Attempt to run MCA with a small subset of the database, still froze during analysis.
results <- Factoshiny(resultsfilter)
resultsfilter <- sample_n(SHINGLES,100)

#Determing deaths for vaccine gorups and men vs women. 
pfizersympdeath <- ifelse(pfizersymp$DIED == "Y", 1,0)
pfizersympdeath
sum(pfizersympdeath)/nrow(pfizersymp)
dead = subset(pfizersymp, pfizersympdeath == 1)
alive = subset(pfizersymp, pfizersympdeath == 0)
mean(dead$AGE_YRS, na.rm = TRUE)
mean(alive$AGE_YRS, na.rm = TRUE)
t.test(alive$AGE_YRS, dead$AGE_YRS, alternative = "two.sided", conf.level = 0.95)
men = subset(pfizersymp, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
women= subset(pfizersymp, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
pfizermenwomen <- rbind(men,women)
t.test(mendeath, womendeath, alternative = "two.sided", conf.level = 0.95)

#Determin deaths by age group for pfizer. 
pfizersymp$AGE_GROUP<-cut(pfizersymp$AGE_YRS,
                          breaks = c(-Inf,1,4,14,24,34,44,54,64,74,84,Inf),
                          labels = c( "0-1 years", "1-4 years", "5-14 years", "15-24 years"
                                      , "25-34 years", "35-44 years", "45-54 years"
                                      , "55-64 years", "65-74 years", "75-84 years",
                                      "85+ years"),
                          right = FALSE)
age = subset(pfizersymp, AGE_GROUP == "0-1 years")
agedeath <- ifelse(age$DIED == "Y", 1, 0)
mean(agedeath)
ageall <- ifelse(pfizersymp$DIED =="Y", 1,0)
mean(ageall)
t.test(agedeath, ageall, alternative = "two.sided", conf.level = 0.95)
sum(agedeath)
nrow(age)
sum(agedeath)/nrow(age)
TAB <- table(pfizersymp$AGE_GROUP, pfizersymp$DIED)
TAB


#Determine death by age group janssen
janssensymp$AGE_GROUP<-cut(janssensymp$AGE_YRS,
                          breaks = c(-Inf,1,4,14,24,34,44,54,64,74,84,Inf),
                          labels = c( "0-1 years", "1-4 years", "5-14 years", "15-24 years"
                                      , "25-34 years", "35-44 years", "45-54 years"
                                      , "55-64 years", "65-74 years", "75-84 years",
                                      "85+ years"),
                          right = FALSE)




age = subset(janssensymp, AGE_GROUP == "85+ years")
agedeath <- ifelse(age$DIED == "Y", 1, 0)
mean(agedeath)
ageall <- ifelse(janssensymp$DIED =="Y", 1,0)
mean(ageall)
t.test(agedeath, ageall, alternative = "two.sided", conf.level = 0.95)
sum(agedeath)
nrow(age)
sum(agedeath)/nrow(age)
TAB <- table(janssensymp$AGE_GROUP, janssensymp$DIED)
TAB

#Determine death by age group Janssen.
janssensymp$AGE_GROUP<-cut(janssensymp$AGE_YRS,
                           breaks = c(-Inf,1,4,14,24,34,44,54,64,74,84,Inf),
                           labels = c( "0-1 years", "1-4 years", "5-14 years", "15-24 years"
                                       , "25-34 years", "35-44 years", "45-54 years"
                                       , "55-64 years", "65-74 years", "75-84 years",
                                       "85+ years"),
                           right = FALSE)




age = subset(janssensymp, AGE_GROUP == "85+ years")
agedeath <- ifelse(age$DIED == "Y", 1, 0)
mean(agedeath)
ageall <- ifelse(janssensymp$DIED =="Y", 1,0)
mean(ageall)
t.test(agedeath, ageall, alternative = "two.sided", conf.level = 0.95)
sum(agedeath)
nrow(age)
sum(agedeath)/nrow(age)
TAB <- table(janssensymp$AGE_GROUP, janssensymp$DIED)
TAB

#Determine death by age group Flu
FLU$AGE_GROUP<-cut(FLU$AGE_YRS,
                           breaks = c(-Inf,1,4,14,24,34,44,54,64,74,84,Inf),
                           labels = c( "0-1 years", "1-4 years", "5-14 years", "15-24 years"
                                       , "25-34 years", "35-44 years", "45-54 years"
                                       , "55-64 years", "65-74 years", "75-84 years",
                                       "85+ years"),
                           right = FALSE)




age = subset(FLU, AGE_GROUP == "85+ years")
agedeath <- ifelse(age$DIED == "Y", 1, 0)
mean(agedeath)
ageall <- ifelse(FLU$DIED =="Y", 1,0)
mean(ageall)
t.test(agedeath, ageall, alternative = "two.sided", conf.level = 0.95)
sum(agedeath)
nrow(age)
sum(agedeath)/nrow(age)
TAB <- table(FLU$AGE_GROUP, FLU$DIED)
TAB

#Determine death by age group Shingles. 

SHINGLES$AGE_GROUP<-cut(SHINGLES$AGE_YRS,
                   breaks = c(-Inf,1,4,14,24,34,44,54,64,74,84,Inf),
                   labels = c( "0-1 years", "1-4 years", "5-14 years", "15-24 years"
                               , "25-34 years", "35-44 years", "45-54 years"
                               , "55-64 years", "65-74 years", "75-84 years",
                               "85+ years"),
                   right = FALSE)




age = subset(SHINGLES, AGE_GROUP == "85+ years")
agedeath <- ifelse(age$DIED == "Y", 1, 0)
mean(agedeath)
ageall <- ifelse(SHINGLES$DIED =="Y", 1,0)
mean(ageall)
t.test(agedeath, ageall, alternative = "two.sided", conf.level = 0.95)
sum(agedeath)
nrow(age)
sum(agedeath)/nrow(age)
TAB <- table(SHINGLES$AGE_GROUP, SHINGLES$DIED)
TAB

#death by sex pfizer.
men = subset(pfizersymp, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
alldeath <- ifelse(pfizersymp$DIED == "Y", 1,0)
alldeath
women= subset(pfizersymp, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
t.test(mendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
t.test(womendeath, alldeath, alternative = "two.sided", conf.level = 0.95)

#death by sex janssen
men = subset(janssensymp, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
alldeath <- ifelse(janssensymp$DIED == "Y", 1,0)
alldeath
women= subset(janssensymp, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
t.test(mendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
t.test(womendeath, alldeath, alternative = "two.sided", conf.level = 0.95)

#death by sex janssen
men = subset(janssensymp, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
alldeath <- ifelse(janssensymp$DIED == "Y", 1,0)
alldeath
women= subset(janssensymp, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
t.test(mendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
t.test(womendeath, alldeath, alternative = "two.sided", conf.level = 0.95)

#death by sex flu
men = subset(FLU, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
alldeath <- ifelse(FLU$DIED == "Y", 1,0)
alldeath
women= subset(FLU, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
t.test(mendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
t.test(womendeath, alldeath, alternative = "two.sided", conf.level = 0.95)

#death by sex shingles
men = subset(SHINGLES, SEX == "M")
mendeath <- ifelse(men$DIED == "Y", 1,0)
mean(mendeath)
alldeath <- ifelse(SHINGLES$DIED == "Y", 1,0)
alldeath
women= subset(SHINGLES, SEX == "F")
womendeath <- ifelse(women$DIED == "Y",1,0)
mean(womendeath)
t.test(mendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
t.test(womendeath, alldeath, alternative = "two.sided", conf.level = 0.95)
TAB <- table(SHINGLES$AGE_GROUP, SHINGLES$DIED)

#Count allergies check results.
count(pfizersymp$VAX_LOT)
allergies = subset(pfizersymp, ALLERGIES == "latex")
allergies
totalallergies <- pfizersymp[!(pfizersymp$ALLERGIES == "None")| (pfizersymp$ALLERGIES == " ")|(pfizersymp$ALLERGIES == "NA"),]
totalallergies


#Count allergies check results. 
count(pfizersymp$VAX_LOT)
allergies = subset(pfizersymp, ALLERGIES == "penicillin")
allergies
totalallergies <- pfizersymp[!(pfizersymp$ALLERGIES == "None")| (pfizersymp$ALLERGIES == " ")|(pfizersymp$ALLERGIES == "NA"),]
totalallergies

# used to get citations for each citation used. 
citation("Factoshiny")
citation("readxl")
citation("stringr")
citation("gtsummary")
citation("ggplot2")
citation("FactoMineR")
citation("Factoshiny")
citation("Hmisc")
citation("epiR")
