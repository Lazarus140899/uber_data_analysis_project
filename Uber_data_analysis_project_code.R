# Installing the required packages:
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")



# Now activating the packages:
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)



# Creating a vector of colors for our plots:
colours <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")



# Reading in the monthly data :
uber_april_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\april_14.csv")
uber_may_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\may_14.csv")
uber_june_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\june_14.csv")
uber_july_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\july_14.csv")
uber_august_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\august_14.csv")
uber_september_14 <- read.csv("D:\\uber_dataset\\uber_dataset_csv\\september_14.csv")




# Checking the data in the following csv files:
print(uber_april_14)
print(uber_may_14)
print(uber_june_14)
print(uber_july_14)
print(uber_august_14)
print(uber_september_14)



# Combining the csv files :
uber_data_2014 <- rbind(uber_april_14, uber_may_14, uber_june_14,uber_july_14,uber_august_14,uber_september_14)



# Checking the dataset uber_data_2014: 
glimpse(uber_data_2014)


# Adding Date.Time column with format mm/dd/yy hh/mm/ss : 
uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time ,format= "%m/%d/%Y %H:%M:%S")
uber_data_2014$Time <- format(as.POSIXct(uber_data_2014$Date.Time ,format= "%m/%d/%Y %H:%M:%S"),format = "%H:%M:%s")



# Checking the Time column: 
glimpse(uber_data_2014$Time)



# Converting the time to universal standard:
uber_data_2014$Date.Time <- ymd_hms(uber_data_2014$Date.Time)



# Extracting the day, month, year, weekofday, hour, minute, seconds from the given datetime data:
uber_data_2014$day <- factor(day(uber_data_2014$Date.Time))
uber_data_2014$month <- factor(month(uber_data_2014$Date.Time, label = TRUE))
uber_data_2014$year <- factor(year(uber_data_2014$Date.Time))
uber_data_2014$weekofday <- factor(wday(uber_data_2014$Date.Time,label = TRUE)) 



# Using Time vector to extract the hours minutes and seconds: 
uber_data_2014$hours <- factor(hour(hms(uber_data_2014$Time)))
uber_data_2014$minutes <-factor(minute(hms(uber_data_2014$Time)))
uber_data_2014$seconds <- factor(second(hms(uber_data_2014$Time)))




# Plotting the trips by the hours in a day:
uber_hour_data <- uber_data_2014 %>%
  group_by(hours) %>%
  dplyr::summarize(Total=n())
datatable(uber_hour_data)


# Plotting the trips by the months and hours:
uber_month_hour <- uber_data_2014 %>% 
  group_by(month,hours) %>%
  dplyr::summarize(Total=n())
datatable(uber_month_hour)



# Using ggplot2 function to plot the graph:

#1. Plotting number of trips with respect to hours traveled:
ggplot(uber_hour_data, aes(hours, Total)) + 
  geom_bar(stat = "Identity", fill = "SteelBlue", color = "Red" ) + 
  ggtitle("Trips every hour") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10), 
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size = 10),
        plot.title = element_text(color = "Blue",
                                  size = 14, family = "Courier"))




# Plotting the number of trips with hour and month:
ggplot(uber_month_hour, aes(hours, Total)) +
  geom_bar(stat= "Identity",  aes(fill= month)) +
  ggtitle("Trips by hour and month") +
  theme(axis.title.x = element_text(color="Black", size=10),
        axis.title.y = element_text(color="Black", size=10),
        axis.text.x = element_text(color = "Black", size = 8), 
        axis.text.y = element_text(color = "Black",size=8),
        plot.title = element_text(color = "Red",size=14,
                                  family = "Courier" ))





# PLOTTING THE DATA BY TRIPS DURING EVERY DAY OF THE MONTH
uber_day_data<- uber_data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total=n())
datatable(uber_day_data)



uber_day_month_data <- uber_data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total=n())
datatable(uber_day_month_data)




# Now we are going to plot the above data:


#1. plotting the number of trips taken everyday:
ggplot(uber_day_data, aes(day, Total)) +
  geom_bar(stat = "Identity", fill= "DarkGreen", color = "Red") +
  ggtitle("Trips everyday") + 
  theme(axis.title.x=element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12),
        axis.text.x = element_text(color= "black", size = 9),
        axis.text.y = element_text(color="black", size  =9),
        plot.title= element_text(color = "black", size = 14,
                                 family = "Courier"))


#2. Plotting the trips by day and month:
ggplot(uber_day_month_data, aes(day, Total, fill = month)) +
  geom_bar(stat = "Identity",position = "dodge") +
  ggtitle("Trips by day and month") + 
  theme(axis.title.x=element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12),
        axis.text.x = element_text(color= "black", size = 9),
        axis.text.y = element_text(color="black", size  =9),
        plot.title= element_text(color = "black", size = 14,
                                 family = "Courier"))




# Check whether use of dodge is needed or not:




# NUMBER OF TRIPS TAKING PLACE DURNG MONTHS IN A YEAR:

# To find the number of trips made in each month of the year
uber_month_group <- uber_data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total=n())
datatable(uber_month_group)


uber_month_weekday_data <- uber_data_2014 %>%
  group_by(month, weekofday) %>%
  dplyr::summarize(Total = n())
datatable(uber_month_weekday_data)


# Plotting of the number of trips made in each month of the year:
ggplot(uber_month_group, aes(x= month, y = Total)) +
  geom_bar(stat="Identity", aes(fill = month)) +
  ggtitle("Number of trips in each month ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family = "Courier" ))


ggplot(uber_month_weekday_data, aes(month, Total, fill = weekofday)) +
  geom_bar(stat="Identity",  position = "dodge", color = "black") +
  ggtitle(" Trips by day and month ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14, 
                                  family= "Courier"))


# str(uber_data_2014)



# FINDING OUT THE NUMBER OF TRIPS BY BASES:

# To find the number of trips taken out by passengers from each of the following bases.
ggplot(uber_data_2014, aes(Base)) +
  geom_bar(fill = "darkblue") +
  ggtitle(" Number of trips by pasengers from the bases ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14, 
                                  family= "Courier"))


# To find the number of trips taken out by passengers from each of the bases.
ggplot(uber_data_2014, aes(Base,fill= month)) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle(" Trips by bases and month ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family ="Courier"))

# To find the number of trips taken out by passengers from each of the bases on the days of the week.
ggplot(uber_data_2014, aes(Base,fill= weekofday)) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle(" Trips by bases and weekofday ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))
#str(uber_data_2014)


# CREATING A HEATMAP VISUALIZATION OF DAY,HOUR AND MONTH:-

# Plotting of heatmap:


#1 By hour and day
uber_day_and_hour <- uber_data_2014 %>%
  group_by(day, hours) %>%
  dplyr::summarize(Total = n())
datatable(uber_day_and_hour)


ggplot(uber_day_and_hour, aes(x=day,y = hours, fill= Total)) +
  geom_tile(color = "white") +
  ggtitle(" Heat map by hour and day") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))





#2 By month and day
ggplot(uber_day_month_data, aes(y = month,x=day,fill= Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by month and day") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))







#3 By month and day of week
ggplot(uber_month_weekday_data, aes(x = weekofday,y= month,fill= Total)) +
  geom_tile( color = "white") +
  ggtitle(" Heatmap by month and day of week") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))





#4 Heatmap that delineates month and bases
uber_month_base_data <- uber_data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total=n())
datatable(uber_month_base_data)


ggplot(uber_month_base_data, aes(x=Base,y = month ,fill= Total)) +
  geom_tile(color = "white") +
  ggtitle(" Heatmap by Month and Bases ") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))





#5 By bases and day of week
uber_weekofday_base_data <- uber_data_2014 %>%
  group_by(Base, weekofday) %>%
  dplyr::summarize(Total=n())
datatable(uber_weekofday_base_data)


ggplot(uber_weekofday_base_data, aes(x=Base,y=weekofday,fill=Total)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Bases and Day of week") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))




#str(uber_data_2014)





# CREATING A MAP OF VISUALIZATION OF RIDES IN NEW YORK 

# We are now going to use geo-plots to visualize the rides in New York city

min_lat  <- 40.5774
max_lat  <- 40.9176
min_long <- -74.15
max_long <- -73.7004


ggplot(uber_data_2014, aes(x=Lon,y=Lat)) +
  geom_point(size=1,color="blue") +
  scale_x_continuous(limits = c(min_long,max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) + theme_map() +
  ggtitle("New York City map based on Uber rides during 2014 (Apr-Sep)") + 
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))



# Plotting of geo-plots of New York City uber rides for 2014(Apr-Sep) by base
ggplot(uber_data_2014, aes(x=Lon,y=Lat)) +
  geom_point(size=1,aes(color= Base)) +
  scale_x_continuous(limits = c(min_long,max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) + theme_map() +
  ggtitle("New York City map based on Uber rides during 2014 (Apr-Sep) by base") +
  theme(axis.title.x= element_text(color = "black", size =12), 
        axis.title.y = element_text(color = "black", size = 12), 
        axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black", size = 8),
        plot.title = element_text(color= "black", size = 14,
                                  family="Courier"))



