#Load the Tiudyverse
library(tidyverse)

#Read in the data
sept <- read.csv("Sept_Dials.csv", header = T, stringsAsFactors = T)
july <- read.csv("Jul_Dials.csv", header = T, stringsAsFactors = T)

#Clean the data
sept$Created.Date <- as.Date(sept$Created.Date, "%m/%d/%Y")
sept$Contacted.Date <- as.Date(sept$Contacted.Date, "%m/%d/%Y")
sept$Scheduled.Date <- as.Date(sept$Scheduled.Date, "%m/%d/%Y")
sept$Showed.Date <- as.Date(sept$Showed.Date, "%m/%d/%Y")
sept$Enrollment.Date <- as.Date(sept$Enrollment.Date, "%m/%d/%Y")
sept$contacted[sept$Contacted.Date > 0] <- 1
sept$contacted[sept$Contacted.Date < 0 | is.na(sept$Contacted.Date)] <- 0
sept$contacted <- as.factor(sept$contacted)

sept$create <- as.factor(sept$Created.Date)
july$create <- as.factor(july$Created.Date)

july$Created.Date <- as.Date(july$Created.Date, "%m/%d/%Y")
july$Contacted.Date <- as.Date(july$Contacted.Date, "%m/%d/%Y")
july$Scheduled.Date <- as.Date(july$Scheduled.Date, "%m/%d/%Y")
july$Showed.Date <- as.Date(july$Showed.Date, "%m/%d/%Y")
july$Enrollment.Date <- as.Date(july$Enrollment.Date, "%m/%d/%Y")
july$contacted[july$Contacted.Date > 0] <- 1
july$contacted[july$Contacted.Date < 0 | is.na(july$Contacted.Date)] <- 0
july$contacted <- as.factor(july$contacted)

#Initial exploration of the variable
summary(sept$Dials)
summary(july$Dials)

#Further wrangling the data
sep_contact <- filter(sept, Contacted.Date > 0 & Dials > 0)
jul_contact <- filter(july, Contacted.Date > 0 & Dials > 0)

sep_sched <- filter(sept, Scheduled.Date > 0 & Dials > 0)
jul_sched <- filter(july, Scheduled.Date > 0 & Dials > 0)

sep_no <- filter(sept, is.na(Contacted.Date))
jul_no <- filter(july, is.na(Contacted.Date))


#More data exploration by outcome
summary(sept$Dials)
summary(sep_no$Dials)
summary(sep_contact$Dials)
summary(sep_sched$Dials)

summary(july$Dials)
summary(jul_no$Dials)
summary(jul_contact$Dials)
summary(jul_sched$Dials)


#Plot the data for all of September by the number of dials/lead
ggplot(data = sept)+
  geom_bar(
    mapping = aes(x = Dials, fill = contacted))+
  labs(y = "Leads")

#Plot the data for September contacted by the number of dials/lead
ggplot(data = sep_contact)+
  geom_bar(
    mapping = aes(x = Dials))+
  labs(y = "Leads")

#Plot the data for September scheduled by the number of dials/lead
ggplot(data = sep_sched)+
  geom_bar(
    mapping = aes(x = Dials))+
  labs(y = "Leads")

#Plot the data for all of July by the number of dials
ggplot(data = july)+
  geom_bar(
    mapping = aes(x = Dials, fill = Lead.Status))+
  labs(title = "July 2017 Leads by # of Dials and Status",x = "# of Dials", y = "Leads")

#Plot the dials for the leads that were contacted
ggplot(data = jul_contact)+
  geom_bar(
    mapping = aes(x = Dials))+
  labs(y = "Leads")

#Plot the dials for the leads that were scheduled
ggplot(data = jul_sched)+
  geom_bar(
    mapping = aes(x = Dials, fill = Lead.Status))+
  labs(y = "Leads")

