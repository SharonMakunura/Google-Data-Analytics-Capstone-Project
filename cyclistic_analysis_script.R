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
difftime(time2, time1, units = "min")

timeDif<- function(df){
  df<-df %>% 
    mutate(duration_mins=as.numeric(round(difftime(ended_at,started_at,units="min"))))
}

#create function to delete rows with invalid duration
delDuration<- function(df){
  df<-df %>% 
    filter(df$duration>=0)
}
 
#create functions to differentiate members and casuals

memberFunc<- function(df){
  df<-df %>% 
    filter(df$member_casual=="member")
}
casualFunc<- function(df){
  df<-df %>% 
    filter(df$member_casual=="casual")
}

### Apply functions to dataframes ####
trips_end_apr<-trips_end_apr %>% 
  missingsFunc() %>% 
  dropCols() 
colnames(trips_end_apr)=renameCols(trips_end_apr)

trips_may_june<-trips_may_june %>% 
  missingsFunc() %>% 
  dropCols() 
colnames(trips_may_june)=renameCols(trips_may_june)

trips_july<-trips_july %>%
  missingsFunc() %>% 
  dropCols() 
colnames(trips_july)=renameCols(trips_july)

trips_aug<-trips_aug %>%
  missingsFunc() %>% 
  dropCols() 
colnames(trips_aug)=renameCols(trips_aug)

trips_sept<-trips_sept %>%
  missingsFunc() %>% 
  dropCols() 
colnames(trips_sept)=renameCols(trips_sept)

trips_oct<-trips_oct %>%
  missingsFunc() %>% 
  dropCols() 
colnames(trips_oct)=renameCols(trips_oct)

trips_nov_dec<-trips_nov_dec %>%
  missingsFunc() %>% 
  dropCols() 
colnames(trips_nov_dec)=renameCols(trips_nov_dec)


head(trips_nov_dec)

###create datetime#####

trips_end_apr<-dtFunc(trips_end_apr)
trips_may_june<-dtFunc(trips_may_june)
trips_july<-dtFunc(trips_july)
trips_aug<-dtFunc(trips_aug)
trips_sept<-dtFunc(trips_sept)
trips_oct<-dtFunc(trips_oct)
trips_nov_dec<-dtFunc(trips_nov_dec)

###create day####


trips_end_apr<-dayFunc(trips_end_apr)
trips_may_june<-dayFunc(trips_may_june)
trips_july<-dayFunc(trips_july)
trips_aug<-dayFunc(trips_aug)
trips_sept<-dayFunc(trips_sept)
trips_oct<-dayFunc(trips_oct)
trips_nov_dec<-dayFunc(trips_nov_dec)


###create duration####
trips_end_apr<-timeDif(trips_end_apr)
trips_may_june<-timeDif(trips_may_june)
trips_july<-timeDif(trips_july)
trips_aug<-timeDif(trips_aug)
trips_sept<-timeDif(trips_sept)
trips_oct<-timeDif(trips_oct)
trips_nov_dec<-timeDif(trips_nov_dec)

##check for errors, 0 duration

trips_end_apr %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_end_apr$duration<0) 
trips_may_june %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_may_june$duration<0) 
trips_july %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_july$duration<0) 
trips_aug %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_aug$duration<0) 
trips_sept %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_sept$duration<0) 
trips_oct %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_oct$duration<0) 
trips_nov_dec %>% 
  select(start_station_name,end_station_name,started_at,ended_at,duration) %>% 
  filter(trips_nov_dec$duration<0) 

#delete invalid durations
trips_end_apr<-delDuration(trips_end_apr)
trips_may_june<-delDuration(trips_may_june)
trips_july<-delDuration(trips_july)
trips_aug<-delDuration(trips_aug)
trips_sept<-delDuration(trips_sept)
trips_oct<-delDuration(trips_oct)
trips_nov_dec<-delDuration(trips_nov_dec)


##make one big dataframe

trips_df<- rbind(trips_end_apr,trips_may_june,trips_july,trips_aug,trips_sept,
                 trips_oct,trips_nov_dec)


###DESCRIPTIVE ANALYSIS####


all_time_sum<-trips_df %>%
  select(start_day,duration_mins) %>%
  summarize(mean_duration=mean(duration_mins),max_duration=max(duration_mins))
all_time_sum  

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

month_rides_sum<-trips_df %>% 
  select(start_month) %>% 
  count(start_month) 
month_rides_sum

month_rides_sum_by_user<-trips_df %>% 
  select(start_month,member_casual) %>% 
  group_by(member_casual) %>% 
  count(start_month)  
month_rides_sum_by_user


day_rides_sum<-trips_df %>% 
  select(start_day) %>% 
  count(start_day) 
day_rides_sum

day_rides_sum_by_user<-trips_df %>% 
  select(start_day,member_casual) %>% 
  group_by(member_casual) %>% 
  count(start_day)  
day_rides_sum_by_user


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


###CREATE GRAPHS####

ggplot(data = trips_df)+ 
  geom_bar(mapping=aes(x=start_day))
