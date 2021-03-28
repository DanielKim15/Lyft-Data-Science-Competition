library(tidyverse)
library(gridExtra)
library(ggplot2)
library(lubridate) #works with dates and times
library(dplyr)
library(forecast)
library(MLmetrics)
#install.packages("forecast")
#install.packages("MLmetrics")
#install.packages("ggpubr")
#install.packages("lubridate")
library(ggpubr)
library(lubridate)

###Step 1: Import Dataset and clean
Driver_ID_Dataset <- read_csv("/Users/dannykim/Downloads/driver_ids - driver_ids.csv", na=c(" ","NA","NULL","PrivacySuppressed"))
Ride_ID_Dataset <- read_csv("/Users/dannykim/Downloads/ride_ids - ride_ids.csv", na=c(" ","NA","NULL","PrivacySuppressed"))
Ride_Timestamp_Dataset <- read_csv("/Users/dannykim/Downloads/ride_timestamps (3).csv", na=c(" ","NA","NULL","PrivacySuppressed"))


view(Driver_ID_Dataset)
View(Ride_ID_Dataset)
View(Ride_Timestamp_Dataset)

length(Ride_Timestamp_Dataset$ride_id)
length(Ride_ID_Dataset)
length(Driver_ID_Dataset)

#Combining Driver and Ride ID dataset
Driver_Rider_Dataset_Test <- left_join(Ride_ID_Dataset,Driver_ID_Dataset, by = c("driver_id" = "driver_id"))

Driver_Rider_Dataset_Test <- mutate(Driver_Rider_Dataset_Test, cost_per_mile = round((ride_distance / 1609.344) * 1.15, digits = 2) , cost_per_minute = round (( ride_duration / 60 ) * .22 , digits = 2), surge_charge = round ((ride_prime_time * 0.01) * (cost_per_mile + cost_per_minute) , digits = 2) , net_pay = cost_per_mile + cost_per_mile + surge_charge + 2 + 1.75, gross_pay = round (net_pay - (.20 * net_pay), digits= 2))
Driver_Rider_Dataset_Test <- mutate(Driver_Rider_Dataset_Test, emp_recorded_minutes = (difftime(driver_onboard_date, "2016-06-30", unit = "hours")) * -1 )

view(Driver_Rider_Dataset_Test)
str(Driver_Rider_Dataset_Test)

#Creating a column that states morning, afternoon, night
Ride_Timestamp_Dataset$timestamp <- ymd_hms(Ride_Timestamp_Dataset$timestamp)

# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")

Ride_Timestamp_Dataset$Time_of_day <- cut(x=hour(Ride_Timestamp_Dataset$timestamp), breaks = breaks, labels = labels, include.lowest=TRUE)

View(Ride_Timestamp_Dataset) #Success
str(Ride_Timestamp_Dataset)
#qqplot
#qqplot(Driver_Rider_Combo_Dataset$Net_Pay)
#shapiro.test(Driver_Rider_Combo_Dataset$Net_Pay)

#Combining Driver_Rider_Combo_Dataset with Ride_Timestamp Dataset
Complete_Dataset <- left_join(Driver_Rider_Dataset_Test,Ride_Timestamp_Dataset, by = c("ride_id" = "ride_id"))
view(Complete_Dataset) #Becareful when you run this, it makes the computer heat up for some reason
Complete_Dataset <- na.omit(Complete_Dataset) #Gets rid of all of the NA
str(Complete_Dataset)

length(Complete_Dataset$Net_Pay)
Complete_Dataset$Net_Pay


#to show the lifetime value
summarize_group(group_cols = c(`driver_id` = "driver_id"),group_funs = c("none"),gross_pay_mean = round(mean(gross_pay), digits= 2),emp_recorded_minutes_med = median(emp_recorded_minutes, na.rm = TRUE),LTV = gross_pay_mean*emp_recorded_minutes_med)


###Step 2: Testing time
#What we need so far: To find drivers lifetime value we need to find 1. how long the rider has been active 2.what each rider monthly revenue is
#It seems like I'm going to have to do a for loop for this to count all the drivers thats been active


##First thing to test: Find out if its normally distributed or not. 
ggqqplot(Complete_Dataset$cost_per_minute) #qqplot test, holy shit its really skewed
ggdensity(Complete_Dataset$cost_per_minute, #Goddamn its skewed to the left
          main = "Density of cost per minute",
          xlab = "Cost per minute")

#Due to my results, we might have to do a nonparametric test?

##Second: Visualization

#Scatter Plot
ggplot(data = Driver_Rider_Dataset_Test) + 
  geom_point(mapping = aes(x = ride_duration, y = gross_pay)) +
  xlab("Ride duration") +
  ylab("Gross Pay") +
  ggtitle("Ride Duration and Gross Pay") +
  ylim(c(0,160)) +
  xlim(c(0,8000))


ggplot(data = Complete_Dataset) + #It seems that Night time has the least duration, Afternoon has the longest duration, and Morning is longer than Evening Duration. Payment wise it seems simialar
  geom_point(mapping = aes(x = ride_duration, y = gross_pay, color=factor(Time_of_day))) +
  xlab("Ride duration") +
  ylab("Gross Pay") +
  ggtitle("Ride Duration and Gross Pay") +
  ylim(c(0,160)) +
  xlim(c(0,8000))


ggplot(data = Complete_Dataset) + #It seems that Night time has the least duration, Afternoon has the longest duration, and Morning is longer than Evening Duration. Payment wise it seems simialar
  geom_point(mapping = aes(x = cost_per_mile, y = gross_pay, color=factor(Time_of_day))) +
  xlab("Cost per mile") +
  ylab("Gross Pay") +
  ggtitle("Ride Duration and Gross Pay") +
  ylim(c(0,160)) +
  xlim(c(0,8000))
#Histograms

#Boxplot
ggboxplot(Complete_Dataset, x = "Time_of_day", y = "ride_distance", 
          color = "Time_of_day", palette = c("#00AFBB", "#E7B800"),
          ylab = "ride duration", xlab = "ride distance")

##Third: Test
#Wilcox test
wilcox.test(Time_of_day~ride_duration, data = Complete_Dataset, exact = FALSE) #For wilcox, its like a two sample t test

length(Complete_Dataset$ride_duration)
length(Complete_Dataset$ride_distance)

#Kruskal test
Kruskal_test_Rideduration_Timeday_test1 <- kruskal.test(ride_duration ~ Time_of_day, data = Complete_Dataset) #so there is significant difference between the days
Kruskal_test_Rideduration_Timeday_test2 <- pairwise.wilcox.test(Complete_Dataset$ride_duration, Complete_Dataset$Time_of_day, p.adjust.method = "BH") #They're all different from each other

kruskal.test(cost_per_mile ~ Time_of_day, data = Complete_Dataset) #so there is significant difference between the days
pairwise.wilcox.test(Complete_Dataset$cost_per_mile, Complete_Dataset$Time_of_day, p.adjust.method = "BH") 


#Try to find a way to find the medians from each of the day
Complete_Dataset$cost_per_mile

Complete_Dataset %>%
  gather(Variable, value, -(ride_duration:Time_of_day))%>%
  group_by(ride_duration, Time_of_day) %>%
  summarise(Median = median(value, na.rm = TRUE))
#idea: change Time_of_day data type from characters to factors


?apply
?sapply
summary(Complete_Dataset[,"Time_of_day"]) #It shows how many nights, mornings, afternoons, evenings there are

#Logistic Regression


