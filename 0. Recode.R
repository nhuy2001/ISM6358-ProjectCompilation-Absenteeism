dat <- readxl::read_xlsx("Absenteeism_at_work.xlsx")
library(tidyverse)
library(lubridate)

dat <- dat %>% mutate("date" = make_date(year = year, month = abMonth, day = 1))

#Recode Weekday
dat$abWeekday[dat$abWeekday == 2] <- "Monday"
dat$abWeekday[dat$abWeekday == "3"] <- "Tuesday"
dat$abWeekday[dat$abWeekday == "4"] <- "Wednesday"
dat$abWeekday[dat$abWeekday == "5"] <- "Thursday"
dat$abWeekday[dat$abWeekday == "6"] <- "Friday"

#Recode Season
dat$season[dat$season == 1] <- "Summer"
dat$season[dat$season == "2"] <- "Fall"
dat$season[dat$season == "3"] <- "Winter"
dat$season[dat$season == "4"] <- "Spring"

#Recode Education
dat$education[dat$education == 1] <- "High School"
dat$education[dat$education == "2"] <- "Grad"
dat$education[dat$education == "3"] <- "Post-Grad"
dat$education[dat$education == "4"] <- "Doctorate"

dat$abReason2[dat$abReason %in% 1:21] <- "Sick"
dat$abReason2[dat$abReason == 22] <- "Patient Fowllow-up"
dat$abReason2[dat$abReason == 23] <- "Medical Consult"
dat$abReason2[dat$abReason == 24] <- "Blood Donation"
dat$abReason2[dat$abReason == 25] <- "Lab Exam"
dat$abReason2[dat$abReason == 26] <- "Unjust Absence"
dat$abReason2[dat$abReason == 27] <- "Physiotherapy"
dat$abReason2[dat$abReason == 28] <- "Dental Consult"

dat$season[dat$abMonth %in% c(12, 1, 2)] <- "Winter"
dat$season[dat$abMonth %in% c(3:5)] <- "Spring"
dat$season[dat$abMonth %in% c(6:8)] <- "Summer"
dat$season[dat$abMonth %in% c(9:11)] <- "Fall"

dat <- dat %>% subset(select = -c(abMonth, year))

toFactor <- c("abWeekday", "season", "education", "disciplinaryFailure", "alcohol", "smoke", "abReason2")

for (i in toFactor){
  dat[[i]] <- dat[[i]] %>% as.factor()
}

wkDLv <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
seasonLv <- c("Winter", "Spring", "Summer", "Fall")
educationLv <- c("High School", "Grad", "Post-Grad", "Doctorate")

dat$abWeekday <- dat$abWeekday %>% factor(levels = wkDLv)
dat$season <- dat$season %>% factor(levels = seasonLv)
dat$education <- dat$education %>% factor(levels = educationLv)

dat[52,]$ID <- 28
dat <- dat %>% filter(!is.na(abReason2))
dat <- dat %>% subset(select = -c(disciplinaryFailure))

dat$month <- month(dat$date)
dat$year <- year(dat$date)

save(dat, file = "data_recode.RData")
