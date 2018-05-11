# All coding is done in MacOS Sierra version 10.12.6
# @author - Anindya Chatterjee
# Uber Supply demand case study
# Plots are created in Tableau


install.packages("lubridate")
install.packages("magrittr")
install.packages("dplyr")
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)

#Load the data into a masterdata frame
masterdata_frame<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE,na.strings = TRUE)

#Structure the requestTimestamp and dropTimestamp in prope formats
masterdata_frame$Request.timestamp<-parse_date_time(x=masterdata_frame$Request.timestamp,orders = 
                                                      c("%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M"))

masterdata_frame$Drop.timestamp<-parse_date_time(x=masterdata_frame$Drop.timestamp,orders = 
                                                      c("%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M"))

#Create a separate column for Request Date
masterdata_frame$RequestDate <- substr(masterdata_frame$Request.timestamp,1,10)

#Create a separate column Request Hour for timeslot analysis
masterdata_frame$RequestHour <- substr(masterdata_frame$Request.timestamp,12,13)

# Plot 1 - Show the cabs request count at different hours of the day from airport/city
# This will help us to identify the timeslots where there is the most demand for cabs from airport/city

#create time slot column based on the below logic to continue with 
#analysis based on requests in different time slots

for (i in 1:nrow(masterdata_frame))
{  
    uberRequest <- masterdata_frame[i, ]
    if(uberRequest$RequestHour =="00" | uberRequest$RequestHour=="01" | uberRequest$RequestHour=="02" |uberRequest$RequestHour=="03")
    {
      masterdata_frame[i,"Time_Slot"] <- "Early Morning"
    }
    else if(uberRequest$RequestHour =="04" | uberRequest$RequestHour=="05" | uberRequest$RequestHour=="06" 
          | uberRequest$RequestHour=="07" | uberRequest$RequestHour=="08" | uberRequest$RequestHour=="09")
    {
      masterdata_frame[i,"Time_Slot"] <- "Morning"
    }
    else if(uberRequest$RequestHour =="10" | uberRequest$RequestHour=="11" | uberRequest$RequestHour=="12" 
            | uberRequest$RequestHour=="13" | uberRequest$RequestHour=="14" | uberRequest$RequestHour=="15"
            | uberRequest$RequestHour=="16")
    {
      masterdata_frame[i,"Time_Slot"] <- "Mid Day"
    }
    else if(uberRequest$RequestHour =="17" | uberRequest$RequestHour=="18" | uberRequest$RequestHour=="19" 
            | uberRequest$RequestHour=="20" | uberRequest$RequestHour=="21")
    {
      masterdata_frame[i,"Time_Slot"] <- "Evening"
    }
    else
    {
      masterdata_frame[i,"Time_Slot"] <- "Late Night"
    }
}

# Plot 2 - Cabs request count per time slots
# Plot 3 - Trips completed count per time slots

# From the Trip status analysis per time slots we see that the issues lies 
# duirng the evening and morning time slots

#Problem 1 : Large number of requests got cancelled during the morning slot
morning_df <- subset(masterdata_frame,masterdata_frame$Time_Slot=="Morning")
morning_df_plot <- ggplot(morning_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot_morning <- morning_df_plot+geom_bar(stat="count",position = "dodge")+
  ggtitle("Morning Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
  annotate("text", x=-Inf,y=Inf,label="Airport vs City", hjust=-.1,vjust=1)
#view the plot
plot_morning


#Number of trips cancelled during the Morning time slot
total_trips_cancelled <- length(which(morning_df$Status=="Cancelled"))
#Number of trips cancelled from airport during the morning time slot
airport_trip_cancel <- length(which((morning_df$Pickup.point=="Airport") & (morning_df$Status == "Cancelled")))
# Number of trips cancelled from city during the Morning time slot
city_trip_cancel <- length(which((morning_df$Pickup.point=="City") & (morning_df$Status == "Cancelled")))
# Percentage of trips cancelled from city
trip_cancel_city_percent <- (city_trip_cancel/total_trips_cancelled*100)
# Percentage of trips cancelled from airport
trip_cancel_airport_percent <- (airport_trip_cancel/total_trips_cancelled*100)
# Trips requested from city to airport during morning slot
demand_city_morning <- length(which(morning_df$Pickup.point=="City"))
# Trips completed from city to airport during morning slot
demand_city_morning_completed <- length(which((morning_df$Pickup.point=="City") & (morning_df$Status=="Trip Completed")))


# Problem 2: Unavailability of cabs during the Evening time slot

evening_df <- subset(masterdata_frame,masterdata_frame$Time_Slot=="Evening")
evening_df_plot <- ggplot(evening_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot_evening <- evening_df_plot+geom_bar(stat="count",position = "dodge")+
  ggtitle("Evening Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
  annotate("text", x=-Inf,y=Inf,label="Airport vs City", hjust=-.1,vjust=1)  
#view the plot
plot_evening



# No of requests with no cars available for evening time slot
total_nocab_available <- length(which(evening_df$Status=="No Cars Available"))
# No of requests with no cars available from airport during evening
airport_nocab_available <- length(which((evening_df$Pickup.point=="Airport") & (evening_df$Status == "No Cars Available")))
# No of requests with no cars availablefrom city during evening
city_nocab_available <- length(which((evening_df$Pickup.point=="City") & (evening_df$Status == "No Cars Available")))
# Percentage of no cars available from city
city_nocab_available_percent <- (city_nocab_available/total_nocab_available*100)
# Percentage of no cars available from airport
airport_nocab_available_percent <- (airport_nocab_available/total_nocab_available*100)
#Demand at the airport during the evening
demand_airport_evening <- length(which(evening_df$Pickup.point=="Airport"))
#No of trips completed from airport in the evening
demand_airport_evening_completed <- length(which((evening_df$Pickup.point=="Airport") & (evening_df$Status=="Trip Completed")))


