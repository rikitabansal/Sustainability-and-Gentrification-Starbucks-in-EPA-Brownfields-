#1 import dataset with treatment and control zipcodes
library(readxl)
event_study_ <- read_excel("~/junior/14.33/long paper/event study .xlsx")
starbucks_zip <- read_excel("~/junior/14.33/long paper/starbucks locations/starbucks zip.xlsx")

#2 merge dataframes 
#2.1 consider only starbucks in zipcodes that appear in treatment or control group, match dataframes by year and zip
starbucks_zip$Year = as.numeric(substring(starbucks_zip$Year, 1, 4))
library(plyr)
event <- event_study_[c(1:3)]
byzipdate <- merge(starbucks_zip, event, by = c("Zipcode"), all.x = FALSE, all.y = TRUE)
names(byzipdate)[4] <- "bfyr"
names(byzipdate)[3] <- "sbxyr"
#2.2 count total # starbucks per zipcode
library(plyr)
total <- count(starbucks_zip, "Zipcode")
#pre 2005 starbcuks per zipcode without duplicates
pre2005 <- starbucks_zip[which(starbucks_zip$Year < 2005),]
pre2005 <- pre2005[!duplicated(pre2005$Address),]
#2.3 remove duplicates and calculate year of opening, keep track of number of starbucks per zip
byzipdate <- byzipdate[!duplicated(byzipdate[c(1,2,3)]),]
byzipdate <- byzipdate[!duplicated(byzipdate$Zipcode),]
#byzipdate$openedpost <- byzipdate$sbxyr - byzipdate$bfyr

#3 build dataframe for event study
#3.1 construct balanced panel that goes from year 2005 to 2020
years <- vector()
for (i in 2005:2020){
  years <- append(years, i)
}
balpanel_b <- data.frame(date = rep((years), length(byzipdate$Zipcode)), Zipcode = rep(byzipdate$Zipcode, each = length(years)))
#3.2 merge balanced panel with starbucks zipcode info
library(dplyr)
balpanel <- left_join(balpanel_b, byzipdate)
#3.3 add number of starbucks per zipcode year
#remove duplicated entries and calculate total starbucks per zipcode 
matching <- ifelse(balpanel$date == balpanel$sbxyr, 1, 0)
balpanel$newsbx <- matching
#sum new starbucks over the zipcode
library(data.table)
setDT(balpanel)[,totalsbx:=cumsum(newsbx), Zipcode]
countpre2005 <- ifelse(balpanel$Zipcode %in% pre2005$Zipcode, 1,0 )
balpanel$totalsbx <- balpanel$totalsbx + countpre2005
#3.4 look at 5 years before and after grant receival (can look at more)
balpanel$"m-5" <- 0
balpanel$"m-4" <- 0
balpanel$"m-3" <- 0
balpanel$"m-2" <- 0
balpanel$"m-1" <- 0
balpanel$"m0" <- 0
balpanel$"m+1" <- 0
balpanel$"m+2" <- 0
balpanel$"m+3" <- 0
balpanel$"m+4" <- 0
balpanel$"m+5" <- 0
#compute dummies such that m = 1 if date-bfyr = +/- m
balpanel$`m-5` <- ifelse(balpanel$sbxyr - balpanel$bfyr == -5, 1, 0)
balpanel$`m-4`<- ifelse(balpanel$sbxyr - balpanel$bfyr == -4, 1, 0)
balpanel$`m-3`<- ifelse(balpanel$sbxyr - balpanel$bfyr == -3, 1, 0)
balpanel$`m-2`<- ifelse(balpanel$sbxyr - balpanel$bfyr == -2, 1, 0)
balpanel$`m-1`<- ifelse(balpanel$sbxyr - balpanel$bfyr == -1, 1, 0)
balpanel$`m0`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 0, 1, 0)
balpanel$`m+1`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 1, 1, 0)
balpanel$`m+2`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 2, 1, 0)
balpanel$`m+3`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 3, 1, 0)
balpanel$`m+4`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 4, 1, 0)
balpanel$`m+5`<- ifelse(balpanel$sbxyr - balpanel$bfyr == 5, 1, 0)
balpanel$yearx <- ifelse(balpanel$sbxyr > balpanel$bfyr, 1, 0)
balpanel$yearx[is.na(balpanel$yearx)] <- 0
#replace all remaining values with 0
balpanel$newsbx[is.na(balpanel$newsbx)] <- 0
balpanel$totalsbx[is.na(balpanel$totalsbx)] <- 0
#dummy of m=1 if new starbucks within treatment period and 0 otherwise 


#4 Run regression
#4.1 omit relative time indicator for m= -1 year prior to grant
balpanel <- subset(balpanel, select=-`m0`)
#dummy = 1 if 
#4.2 regress cumulative starbucks on time indicators, zipcode fixed effects, and controls relative to year m
library(lfe)
#fixed effects linear model
reg <- felm(formula = totalsbx ~ Treated + date*yearx | date + Zipcode | 0 | Zipcode, data = balpanel)
summary(reg)
