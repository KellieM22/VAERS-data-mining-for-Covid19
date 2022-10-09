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
