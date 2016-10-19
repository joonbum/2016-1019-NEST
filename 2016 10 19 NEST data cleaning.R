## NEST data clean-up
## Author: Joonbum Lee (joonbum@mit.edu)
## Last modified: 09/28/2016



# 1. Load ----

# load libraries
library(ggplot2)
library(plyr)
library(magrittr)
library(gdata)
library(sjPlot)
library(sjmisc)

# load a custom function
source("~/Dropbox (Personal)/Works/R/Fuction/nest.glance.duration.without.transition.endglance.R") # this is a custom function to convert continuous glance data to discrete formats






# 2. Clean ----

# set working directory
setwd("~/Documents/Local data archive/Nest data copy 2/March 2016 Data/") # you probably need to change this directory to your local directory

# make file list
temp <- list.files(pattern="*.csv")



# ::::2.1 Create a single table including all timeline data----

# create an empty data frame
data.nest.raw <-data.frame(matrix(ncol = 0, nrow = 0))
# create a progress bar
pb<-txtProgressBar(min = 0, max = length(temp), style = 3)

# loop to read each file and bind into one big table. this will take some time.
for (i in 1:length(temp)) {
  #temp2 = read.csv(temp[i], header = TRUE)
  #temp2 <- read.xls(temp[i], sheet = 1, header=TRUE)%>%subset(!(EYEGLANCE==""))%>%subset(select=c("System.Time_Stamp","vtti.timestamp","vtti.file_id","EYEGLANCE","HANDSONWHEEL","DETAILEDTASK"))
  temp2 <- read.csv(temp[i], header=TRUE)
  names(temp2)[1]<-"System.Time_Stamp"
  temp2<-subset(temp2,EYEGLANCE!="")%>%subset(select=c("System.Time_Stamp","vtti.timestamp","vtti.file_id","EYEGLANCE","HANDSONWHEEL","DETAILEDTASK"))
  temp2$file_index <- i
  temp2$ref <- temp[i]
  data.nest.raw <- rbind(data.nest.raw,temp2)
  setTxtProgressBar(pb, i)
}




# ::::2.2 Create a single table including trip level summary----

# create an empty data frame
data.nest.sum <-data.frame(matrix(ncol = 0, nrow = 0))
# create a progress bar
pb<-txtProgressBar(min = 0, max = length(temp), style = 3)

# loop to read each file and bind into one big table. this will take some time.
for (i in 1:length(temp)) {
  #temp2 <- read.xls(temp[i], sheet = 1, header=TRUE) 
  temp2 = read.csv(temp[i], header = TRUE)%>%subset(select=c("EVENT_ID","EVENTSEVERITY1","FAULT","SUBJECTNUMBER","DRIVERBEHAVIOR1","DRIVERBEHAVIOR2",
                                                             "DRIVERBEHAVIOR3","DRIVERIMPAIRMENTS","FIRSTSECONDARYTASK1","FIRSTSECONDARYTASK2","FIRSTSECONDARYTASK3",
                                                             "FIRSTSECONDARYTASK4","FIRSTSECONDARYTASK5","FIRSTSECONDARYTASK6","SECONDSECONDARYTASK1",
                                                             "SECONDSECONDARYTASK2","SECONDSECONDARYTASK3","SECONDSECONDARYTASK4","SECONDSECONDARYTASK5",
                                                             "SECONDSECONDARYTASK6","THIRDSECONDARYTASK1","THIRDSECONDARYTASK2","THIRDSECONDARYTASK3",
                                                             "THIRDSECONDARYTASK4","THIRDSECONDARYTASK5","THIRDSECONDARYTASK6","SURPRISED","makeName","modelYear",
                                                             "ageGroup","sex","milesLstYr","annualMiles","ownLngth","numViol","numCrash","dhiVMIScore","dhiUFOVScore"))
  temp2$file_index <- i
  temp2$ref <- temp[i]
  data.nest.sum <- rbind(data.nest.sum,temp2[1,])
  setTxtProgressBar(pb, i)
}




# 3. Analyze----

# read preprocessed tables
data.glance<-read.csv("~/Documents/Local data archive/Nest data copy/Cleaned data/2016 06 15 nest glance data_new version.csv") # I saved the first file from above and read it from my local machine to avoid repeating
data.demo<-read.csv("~/Documents/Local data archive/Nest data copy/Cleaned data/2016 06 15 nest demo and task data_new version.csv") # I saved the second file and read it from my local machine


# find start time to add clock
data.glance.time <- ddply(data.glance, .(file_index), summarise,
                          start_time = min(vtti.timestamp))


data.glance <- join(data.glance, data.glance.time, by="file_index")

# add clock to subset only first 20 seconds
data.glance$clock <- (data.glance$vtti.timestamp - data.glance$start_time)/1000


# check coding length
data.sum.time<-ddply(data.glance, .(file_index), summarise,
                     start_time = min(vtti.timestamp),
                     end_time = max(vtti.timestamp))


# add a coding length variable
data.sum.time$coding_duration <- (data.sum.time$end_time - data.sum.time$start_time)/1000
data.demo.short<-data.demo[c("file_index", "EVENTSEVERITY1", "SUBJECTNUMBER")]
data.sum.time2<-join(data.sum.time,data.demo.short, by="file_index")

# check distribution
ggplot(data.sum.time2, aes(x=coding_duration))+geom_histogram(binwidth=0.1, fill="red", colour="black")+facet_grid(EVENTSEVERITY1~., scale="free")+theme_bw(16)+xlab("Coding length (sec)")+ylab("Count")+
  geom_vline(xintercept = c(29, 31))

# check outlier
data.abnormal.length<-subset(data.sum.time2, (EVENTSEVERITY1%in%c("Crash","Near-Crash"))&((coding_duration < 29)|(coding_duration > 31)))
data.list.outlier <- unique(data.abnormal.length$SUBJECTNUMBER)


# get glance duration data

# feed only first 20 sec
data.glance.20 <- subset(data.glance, clock <= 20)

data.duration <- nest.glance.duration.without.transition.endglance(data.glance.20) # function to add end glance


# join glance duration data and trip level summary
data.all <- join(data.duration, data.demo, by="ref")

# exclude outliers (N = 204 -> N = 191) (N of files = 1180 -> 1085)
data.all.clean <- subset(data.all, !(SUBJECTNUMBER%in%data.list.outlier))

data.sum.time2<-ddply(data.all.clean, .(file_index, EVENTSEVERITY1), summarise,
                      total_time = sum(final.duration))

min(data.sum.time2$total_time, na.rm=TRUE)
max(data.sum.time2$total_time, na.rm=TRUE)


names(data.all.clean)
ggplot(data.all.clean, aes(x=EVENTSEVERITY1))+geom_bar()
ggplot(data.all.clean, aes(x=EVENTSEVERITY1))+geom_bar()+facet_grid(ageGroup~sex)
unique(data.all.clean$sex)
head(data.all.clean)
