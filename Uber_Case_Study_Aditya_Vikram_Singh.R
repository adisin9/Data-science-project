## Detailed comments mentioned with every code

# installing packages
install.packages("parsedate")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")

library(parsedate)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
# Retreiving the 'uber Request data.csv' file in R
Uber_Request_Data <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE,na.strings = TRUE)

# View the data
View(Uber_Request_Data)

# Converting date timestamp to a common format: 

Uber_Request_Data$Request.timestamp <- parse_date_time(Uber_Request_Data$Request.timestamp, orders = c("%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M" ))
Uber_Request_Data$Drop.timestamp <- parse_date_time(Uber_Request_Data$Drop.timestamp, orders = c("%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M"))


# Separating date and time into separate columns date timestamp into date and time
# for both ride request time and ride drop/completion time

Uber_Request_Data$Ride.Request.Date <- as.Date(Uber_Request_Data$Request.timestamp, format = "%d-%m-%Y")
Uber_Request_Data$Ride.Request.Time <- format(Uber_Request_Data$Request.timestamp, "%H:%M:%S") 

Uber_Request_Data$Drop.Request.Date <- as.Date(Uber_Request_Data$Drop.timestamp, format = "%d-%m-%Y")
Uber_Request_Data$Drop.Request.Time <- format(Uber_Request_Data$Drop.timestamp, format = "%H:%M:%S")

# Creating another column to have the 'Hour' so that it can be used to analyse the number of rides that were
# requested every hour. 

Uber_Request_Data$Ride.Request.Hour <- format(Uber_Request_Data$Request.timestamp, "%H")

# Creating another column 'Day' using the date timestamp so that it can be analysed which days have the maximum 
# ride request.

Uber_Request_Data$Request_Day <- weekdays(Uber_Request_Data$Request.timestamp)

# Creating another column 'Time Slot' to analyse which time period had the maximum/Minimum ride request:

Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "04" & Uber_Request_Data$Ride.Request.Hour < "08"] <- c("Early Morning")
Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "08" & Uber_Request_Data$Ride.Request.Hour < "12"] <- c("Morning")
Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "12" & Uber_Request_Data$Ride.Request.Hour < "16"] <- c("Afternoon")
Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "16" & Uber_Request_Data$Ride.Request.Hour < "20"] <- c("Evening")
Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "20" & Uber_Request_Data$Ride.Request.Hour <= "23"] <- c("Night")
Uber_Request_Data$Time_slot[Uber_Request_Data$Ride.Request.Hour >= "00" & Uber_Request_Data$Ride.Request.Hour < "04"] <- c("Late Night")


# Total Ride requested slot wise for all days which will give us the frequency of rides cancelled, completed
# and no cars were available during different slots

Total_request_slot_wise_plot <- ggplot(Uber_Request_Data,aes(x=factor(Time_slot), fill=factor(Status))) + geom_bar(stat="count", position = "dodge") + ggtitle("Rides Requested during different time slots") +
  labs(x="Time Slots",y="Rides Requested")+ labs(fill="Ride Status") +
    scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled")) +
  scale_x_discrete(limits=c("Early Morning","Morning","Afternoon",
                            "Evening","Night", "Late Night"))
Total_request_slot_wise_plot

# Checking and Calculating total trips completed, total trips cancelled and total ride request where no cars were available: 
 ## There are two ways by which it can be done. In order to find the total number of rides of all the three status (Completed, Cancelled and No Cars Available),
 ## we can simply use the length funtion which will give us the count. 

Number_of_Trips_completed <- length(which(Uber_Request_Data$Status == "Trip Completed"))
Number_of_Trips_Cancelled <- length(which(Uber_Request_Data$Status == "Cancelled"))
Number_Of_Trips_No_Cars_Available <- length(which(Uber_Request_Data$Status == "No Cars Available"))

Number_of_Trips_Cancelled
Number_of_Trips_completed
Number_Of_Trips_No_Cars_Available

## In order to perform a deeper analysis, we can create separate data frames for these ride status:

Trips_Completed_df <- subset(Uber_Request_Data, Uber_Request_Data$Status == "Trip Completed")
Trips_Cancelled_df <- subset(Uber_Request_Data, Uber_Request_Data$Status == "Cancelled")
Trips_Where_No_Cars_Available_df <- subset(Uber_Request_Data, Uber_Request_Data$Status == "No Cars Available")

View(Trips_Cancelled_df)
View(Trips_Completed_df)
View(Trips_Where_No_Cars_Available_df)
nrow(Trips_Where_No_Cars_Available_df)
nrow(Trips_Cancelled_df)
nrow(Trips_Completed_df)

# Total demand,supply and gap provided below: 

Total_Demand <- length(Uber_Request_Data$Request.id)
Total_Supply <- Number_of_Trips_completed
Total_Gap <- Total_Demand - Total_Supply

Total_Demand
Total_Supply
Total_Gap

# We can calculate the opportubity lost:

Opportunity_Lost <- (Total_Gap/Total_Demand)*100
Opportunity_Lost 

# Total number of request hourwise (for all days): 

Hourly_Request_Count_Plot <- ggplot(Uber_Request_Data,aes(x=factor(Uber_Request_Data$Ride.Request.Hour), fill=factor(Pickup.point)))+geom_bar(stat='count',position = "dodge")+
                                                          ggtitle("Hourly Request for Cabs")+
                                                          labs(x="Time in Hours", y="Number of Cabs Requested")+
                                                          labs(fill="Pickup Point")

Hourly_Request_Count_Plot

# Plot for rides requested from Airport and City: 

pickup_point_plot <- ggplot(Uber_Request_Data,aes(x=Uber_Request_Data$Pickup.point,fill=factor(Status) ,group=factor(Uber_Request_Data$Status))) +
  geom_bar(position = position_dodge(width=NULL)) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .99), size = 3,vjust=-.4) +
  labs(x="Pick up points",y="Number of cabs requested")+ labs(fill="Status")

pickup_point_plot

# Count for rides requested from Airport (Pickup Point) to the City: 

Rides_Requested_from_Airport <- length(which(Uber_Request_Data$Pickup.point == "Airport"))
Rides_Requested_from_Airport_which_were_cancelled <- length(which(Uber_Request_Data$Pickup.point == "Airport" & Uber_Request_Data$Status == "Cancelled"))
Rides_Requested_from_Airport_where_No_Cars_were_Available <- length(which(Uber_Request_Data$Pickup.point == "Airport" & Uber_Request_Data$Status == "No Cars Available"))

Rides_Requested_from_Airport
Rides_Requested_from_Airport_which_were_cancelled
Rides_Requested_from_Airport_where_No_Cars_were_Available

 ## Creating data frame for rides requested from airport for analysis:
Airport_Ride_Requests_df <- subset(Uber_Request_Data, Uber_Request_Data$Pickup.point == "Airport")
View(Airport_Ride_Requests_df)

# Count for rides requested from City (Pickup Point) to the Airport:

Rides_Requested_from_City <- length(which(Uber_Request_Data$Pickup.point == "Airport"))
Rides_Requested_from_City_where_No_Cars_were_Available <- length(which(Uber_Request_Data$Pickup.point == "City" & Uber_Request_Data$Status == "No Cars Available"))
Rides_Requested_from_City_which_were_cancelled <- length(which(Uber_Request_Data$Pickup.point == "City" & Uber_Request_Data$Status == "Cancelled"))

Rides_Requested_from_City
Rides_Requested_from_City_where_No_Cars_were_Available
Rides_Requested_from_City_which_were_cancelled

## Creating data frame for rides requested from city for analysis:

City_Ride_Requests_df <- subset(Uber_Request_Data, Uber_Request_Data$Pickup.point == "City")
 View(City_Ride_Requests_df)
 
# As per the "Hourly_Request_Count_Plot", we see that cancellation frequency is highest in the early morning: 
#Number of trips cancelled for the Early Morning time slot
# Using 'Trips_Cancelled_df' data frame which was created earlier in this file.
 
Total_rides_cancelled <- length(which(Trips_Cancelled_df$Status=="Cancelled" & Trips_Cancelled_df$Time_slot == "Early Morning"))
Total_rides_cancelled

#Number of trips cancelled from airport for Early Morning Slot

Airport_Ride_Cancellation <- length(which((Trips_Cancelled_df$Pickup.point=="Airport" & Trips_Cancelled_df$Time_slot == "Early Morning")))
Airport_Ride_Cancellation

# Number of trips cancelled from city for Early Morning Slot
City_Ride_Cancellation <- length(which((Trips_Cancelled_df$Pickup.point=="City" & Trips_Cancelled_df$Time_slot == "Early Morning")))
City_Ride_Cancellation

# Percentage of trips cancelled from city out of total trips cancelled during early morning Slot

Percantage_Rides_Cancelled_City <- (City_Ride_Cancellation/Total_rides_cancelled)*100
Percantage_Rides_Cancelled_City

# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
Percantage_Rides_Cancelled_Airport <- (Airport_Ride_Cancellation/Total_rides_cancelled)*100
Percantage_Rides_Cancelled_Airport

# Rest of the plotting done using Tableau.

# Writing file to CSV so that it can be used with Tableau
write.csv(Uber_Request_Data,  file = "Uber_Reference_File.csv", row.names = FALSE)
