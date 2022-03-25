#load packages
library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

#load data
trips_q1<- read.csv("Divvy_Trips_2020_Q1.csv")
trips_frame1<- read.csv("202004-divvy-tripdata.csv")
trips_frame2<- read.csv("202005-divvy-tripdata.csv")
trips_frame3<- read.csv("202006-divvy-tripdata.csv")

trips_july<- read.csv("202007-divvy-tripdata.csv")
trips_aug<- read.csv("202008-divvy-tripdata.csv")
trips_sept<- read.csv("202009-divvy-tripdata.csv")
trips_oct<- read.csv("202010-divvy-tripdata.csv")
trips_nov<- read.csv("202011-divvy-tripdata.csv")
trips_dec<- read.csv("202012-divvy-tripdata.csv")



#scan through data

glimpse(trips_q1)
glimpse(trips_frame1)
glimpse(trips_july)
glimpse(trips_aug)
glimpse(trips_sept)
glimpse(trips_oct)
glimpse(trips_nov_dec)


#combine some of the dfs
#join q1 with apr
trips_end_apr<-rbind(trips_q1,trips_frame1)
trips_may_june<-rbind(trips_frame2,trips_frame3)
trips_nov_dec<-rbind(trips_nov,trips_dec)


#scan through data

glimpse(trips_end_apr)
glimpse(trips_may_june)
glimpse(trips_july)
glimpse(trips_aug)
glimpse(trips_sept)
glimpse(trips_oct)
glimpse(trips_nov_dec)

#check duplicated data
sum(duplicated(trips_end_apr))
sum(duplicated(trips_aug))
sum(duplicated(trips_may_june))
sum(duplicated(trips_july))
sum(duplicated(trips_sept))
sum(duplicated(trips_oct))
sum(duplicated(trips_nov_dec))
    
###Custom Functions#####
#create function to replace missing values with values from another column

missingsFunc <- function(df) {
  df %>% 
    #replace missing values
    mutate(endstation_name2=coalesce(df$end_station_name,df$start_station_name)) %>% 
    mutate(endstation_id2=coalesce(end_station_id,start_station_id)) %>% 
    mutate(end_lat2=coalesce(end_lat,start_lat)) %>% 
    mutate(end_lng2=coalesce(end_lng,start_lng))
}

#create function to drop original end station cols 

dropCols<-function(df){
  #create vector of columns to drop
  drop_cols<-c("end_station_name", "end_station_id", "end_lat", "end_lng")
  df<-df[,!(names(df) %in% drop_cols)]
  
}

#rename end_station  columns

renameCols<-function(df){
  colnames(df)<-c("ride_id","rideable_type","started_at","ended_at",
                  "start_station_name","start_station_id","start_lat","start_lng",
                  "member_casual","end_station_name","end_station_id","end_lat","end_lng")
}

#create function to convert start and end datetimes to datetime objects

dtFunc<- function(df){
  df<-df %>% 
    mutate(started_at=strptime(df$started_at,format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(ended_at=strptime(df$ended_at,format = "%Y-%m-%d %H:%M:%S")) 
}


#create function to create day and month columns

dayFunc<- function(df){
  df<-df %>% 
    mutate(start_day=format(df$started_at, format = "%a")) %>% 
    mutate(start_month=format(df$started_at, format = "%b"))
}

#create duration column

timeDif<- function(df){
  df<-df %>% 
    mutate(duration_mins=as.numeric(round(difftime(ended_at,started_at,units="min"))))
}

#create function to delete rows with invalid duration
delDuration<- function(df){
  df<-df %>% 
    filter(df$duration>=0)
}
 


###Create function to apply all cleaning functions


cleanFunc<- function(df){
  df<-df %>% 
    missingsFunc() %>% 
    dropCols()
  colnames(df)=renameCols(df)
  df<-df %>% dtFunc() %>% timeDif() %>% dayFunc() %>% delDuration()
  
}


### Apply functions to dataframes ####
trips_end_apr<-cleanFunc(trips_end_apr)
trips_may_june<-cleanFunc(trips_may_june)
trips_july<-cleanFunc(trips_july)
trips_aug<-cleanFunc(trips_aug)
trips_sept<-cleanFunc(trips_sept)
trips_oct<-cleanFunc(trips_oct)
trips_nov_dec<-cleanFunc(trips_nov_dec)



#create function to drop longitude and latitude columns
dropCord<-function(df){
  #create vector of columns to drop longitude and latitude columns
  drop_cols2<-c("start_lat", "start_lng", "end_lat", "end_lng")
  df<-df[,!(names(df) %in% drop_cols2)]
  
}

trips_end_apr<-dropCord(trips_end_apr)
trips_may_june<-dropCord(trips_may_june)
trips_july<-dropCord(trips_july)
trips_aug<-dropCord(trips_aug)
trips_sept<-dropCord(trips_sept)
trips_oct<-dropCord(trips_oct)
trips_nov_dec<-dropCord(trips_nov_dec)

glimpse(trips_aug)


head(trips_nov_dec)
##make one big dataframe

trips_df<- rbind(trips_end_apr,trips_may_june,trips_july,trips_aug,trips_sept,
                 trips_oct,trips_nov_dec)


###DESCRIPTIVE ANALYSIS####


all_time_sum<-trips_df %>%
  select(start_day,duration_mins) %>%
  summarize(mean_duration=mean(duration_mins),max_duration=max(duration_mins))
all_time_sum  

#create function to calculate the mode

getmode <- function(colmn) {
  unique_col <- unique(colmn)
  unique_col[which.max(tabulate(match(colmn, unique_col)))]
}

#get mode day and station
getmode(trips_df$start_day)
getmode(trips_df$start_month)

#duration summaries
time_sum_by_user<-trips_df %>%
  select(member_casual,duration_mins) %>%
  group_by(member_casual) %>% 
  summarize(mean_duration=mean(duration_mins),max_duration=max(duration_mins))
time_sum_by_user 

time_sum_by_day<-trips_df %>%
  select(start_day,duration_mins) %>%
  group_by(start_day)%>% 
  summarize(mean_duration=mean(duration_mins),max_duration=max(duration_mins))
time_sum_by_day

time_sum_by_month<-trips_df %>%
  select(start_month,duration_mins) %>%
  group_by(start_month)%>% 
  summarize(mean_duration=mean(duration_mins),max_duration=max(duration_mins))
time_sum_by_month

#month and day summaries

month_rides_sum<-trips_df %>% 
  select(start_month) %>% 
  count(start_month) 
month_rides_sum

month_rides_sum_by_user<-trips_df %>% 
  select(start_month,member_casual) %>% 
  group_by(member_casual) %>% 
  count(start_month) %>% 
  arrange(start_month)
month_rides_sum_by_user


day_rides_sum<-trips_df %>% 
  select(start_day) %>% 
  count(start_day) 
day_rides_sum

day_rides_sum_by_user<-trips_df %>% 
  select(start_day,member_casual) %>% 
  group_by(member_casual) %>% 
  count(start_day) %>% 
  arrange(start_day)
day_rides_sum_by_user

#create separate member and casual df
memberdf<-trips_df %>% 
  select(member_casual,start_day,start_month,start_station_name,duration_mins) %>% 
  filter(member_casual=="member")  


casualdf<-trips_df %>% 
  select(member_casual,start_day,start_month,start_station_name,duration_mins) %>% 
  filter(member_casual=="casual")  


str(memberdf)
#find mode day, month for each member type

getmode(memberdf$start_day)
getmode(memberdf$start_month)
getmode(casualdf$start_day)
getmode(casualdf$start_month)

#explore rideables
trips_df %>% 
  select(rideable_type,member_casual) %>% 
  group_by(rideable_type,member_casual) %>% 
  count(rideable_type)  


#combine summaries

monthly_sum<-time_sum_by_month %>% 
  inner_join(month_rides_sum,by="start_month")
monthly_sum

day_sum<-time_sum_by_day %>% 
  inner_join(day_rides_sum,by="start_day")
day_sum

#export data frames for further analysis

write.csv(trips_df,"cyclistic_trips.csv", row.names = FALSE)
write.csv(monthly_sum,"monthly_sum.csv", row.names = FALSE)
write.csv(day_sum,"day_sum.csv", row.names = FALSE)
write.csv(day_rides_sum_by_user,"day_rides_sum_by_user.csv", row.names = FALSE)
write.csv(month_rides_sum_by_user,"month_rides_sum_by_user.csv", row.names = FALSE)



