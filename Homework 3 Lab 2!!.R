#Valery Delgado 
#Homework 3 based on Lab2
#Intro to Econometrics and Statistics B2000
#Group members:Cassidy Drummond and Sule Zakaria

load("~/Downloads/Stats and Econometrics/Household_Pulse_data.RData")
install.packages("gmodels")
install.packages("descr")
library(descr)
install.packages("margins")

#This is to restrict the household data in terms of gender at birth, recieved vaccination, and covid status.
restrict2 <- (Household_Pulse_data$EGENID_BIRTH == "male") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
restrict1 <- (Household_Pulse_data$EGENID_BIRTH == "female") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
data_newf <- subset(Household_Pulse_data,restrict1)
data_newm <- subset(Household_Pulse_data, restrict2)

#summary for the data_new for females and males regarding their vaccination status and covid status.
summary(data_newf$RECVDVACC)
summary(data_newm$RECVDVACC)
summary(data_newf$HADCOVID)
summary(data_newm$HADCOVID)


#Thia is a cross table for females and males for vaccination status.
RECVDVACC = sample(c("yes got vaxx","no did not get vaxx","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(RECVDVACC,EGENID_BIRTH)
CrossTable(RECVDVACC,EGENID_BIRTH)


#This is a cross table for females and males for covid status>
HADCOVID = sample(c("yes doctor told had covid","no did not","NA"), 68962, replace = TRUE)
EGENID_BIRTH = sample(c("female", "male"), 68962, replace = TRUE)
x = data.frame(HADCOVID,EGENID_BIRTH)
CrossTable(HADCOVID,EGENID_BIRTH)

# All of these four sections of code below are used to determine sd(standard deviation) and t-test for the standard error formula.
#Standard error formula is the sample standard deviation/ number of samples(n=68962)
sd(summary(data_newm$RECVDVACC))
t.test(summary(data_newm$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$RECVDVACC))
t.test(summary(data_newf$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$HADCOVID))
t.test(summary(data_newf$HADCOVID),var.equal = TRUE)

sd(summary(data_newm$HADCOVID))
t.test(summary(data_newm$HADCOVID),var.equal = TRUE)
#The prop table is used to determine the marginal probabilites
prop.table(summary(data_newf$RECVDVACC), margin = NULL)
prop.table(summary(data_newm$RECVDVACC), margin = NULL)
prop.table(summary(data_newf$HADCOVID), margin = NULL)
prop.table(summary(data_newm$HADCOVID), margin = NULL)

