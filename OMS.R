pacman::p_load(pacman,outliers,rio,psych,ggplot2,tidyverse,dplyr,devtools,ggpubr,pracma,glue,smooth,forecast,CombMSC,stringr,datetime,lubridate,gridExtra)
data <- import('/Users/udesh/Downloads/ML and DataScience Resources/Focus Data/original dataset.csv')
data.copy <- data

#convert Start time to ISO 8601 format
data.copy$`Start Time` <- str_replace(data.copy$`Start Time`,"\\..*","")
data.copy$`Start Time` <- str_replace(data.copy$`Start Time`,"T"," ")

#convert End time to ISO 8601 format
data.copy$`End Time` <- str_replace(data.copy$`End Time`,"\\..*","")
data.copy$`End Time` <- str_replace(data.copy$`End Time`,"T"," ")

#convert stat and end times to datetime format
data.copy$`Start Time` <- as.POSIXct(data.copy$`Start Time`)
data.copy$`End Time`<- as.POSIXct(data.copy$`End Time`)

#calculate time spent on each task
data.copy <- mutate(data.copy, 'Time Spent' = (data.copy$`End Time`- data.copy$`Start Time`))

#subset the OMS data
oms <- str_detect(data.copy$Tag, "OMS")
oms.data <- data.copy[oms,]
oms.data$`Time Spent` <- as.numeric(round(oms.data$`Time Spent`/60))
#Remove sessions that I ended without completing.
oms.data <- oms.data[oms.data$`Is Success`,]

#drop notes,tree type and success column
oms.data <-select(oms.data,-c(4,5,6))

#change column names
names(oms.data) <- c("Start", "End", "Week Number", "Session Time (minutes)" )

#calculate totals for each week so far
total.time <- as.data.frame(aggregate(x = oms.data$`Session Time (minutes)`, by = list(oms.data$`Week Number`), FUN = sum))
names(total.time) <- c('Week', 'Total Time (minutes)')

#Calculate moving average
avg <- c(movavg(c(oms.data[,4]),n = (NROW(oms.data)) - 1, type = 's'))
oms.data <- mutate(oms.data, "Moving Average" = avg)

#plot the data
ggplot(data = oms.data, aes(x = oms.data$Start,y = oms.data$`Session Time (minutes)`, color = as.factor(oms.data$`Week Number`))) + 
  geom_point() + 
  labs(x = "Time Since Start Of Intro To Analytics Modeling (days)") + 
  labs(y = 'Session Length (Minutes)') +
  labs(color = 'Weeks')+
  labs(title = "Intro To Analytics Modeling Study Sessions Week 1 Onwards") +
  theme(plot.title = element_text(hjust = 0.5))

#Seperate start and end dates to their own column. Remove the times
oms.data <- mutate(oms.data, "Start Date" = as.character(oms.data$Start))
oms.data$`Start Date` <- str_replace(oms.data$`Start Date`," ",".")
oms.data$`Start Date` <- str_replace(oms.data$`Start Date`,"\\..*","")
oms.data$`Start Date`<- as.POSIXct(oms.data$`Start Date`)
oms.data <- mutate(oms.data, "End Date" = as.character(oms.data$End))
oms.data$`End Date` <- str_replace(oms.data$`End Date`," ",".")
oms.data$`End Date` <- str_replace(oms.data$`End Date`,"\\..*","")
oms.data$`End Date`<- as.POSIXct(oms.data$`End Date`)

#calculate the total time spent on each day, add to the dataframe
daily.total <- as.data.frame(aggregate(x = oms.data$`Session Time (minutes)`, by = list(oms.data$`Start Date`), FUN = sum))
names(daily.total) <- c("Date", "Total Time (minutes)")

#plot the data with vertical lines for assignment due dates
ggplot(daily.total, aes(x = daily.total$Date, y = daily.total$`Total Time (minutes)`), color = as.factor(daily.total$Date)) +
  geom_line() +
  labs(x = "Date") +
  labs(y = "Total Time Per Day (Minutes)") +
  geom_vline(xintercept = daily.total$Date[10], color = 'red', linetype = 'dotted', size = 1.2) +
  geom_vline(xintercept = daily.total$Date[17], color = 'red', linetype = 'dotted', size = 1.2) +
  geom_vline(xintercept = daily.total$Date[25], color = 'red', linetype = 'dotted', size = 1.2) +
  geom_vline(xintercept = daily.total$Date[31], color = 'red', linetype = 'dotted', size = 1.2) +
  labs(title = "Intro To Analytics Modeling : Total Study Time per day with Assignment Deadlines") +
  theme(plot.title = element_text(hjust = 0.5)) +

write.csv(oms.data,"/Users/udesh/Downloads/ML and DataScience Resources/Focus Data/CleanedOMS.csv", row.names = FALSE)


