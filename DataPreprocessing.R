install.packages("tidyverse")
install.packages("broom")
install.packages("janitor")
install.packages("scales")
library("janitor")
library("tidyverse")
library(scales)

# Loading all the csv files of last 12 months
df1 <- read_csv("~/CaseStudy-1/Dataset/202105-divvy-tripdata.csv")
df2 <- read_csv("~/CaseStudy-1/Dataset/202104-divvy-tripdata.csv")
df3 <- read_csv("~/CaseStudy-1/Dataset/202103-divvy-tripdata.csv")
df4 <- read_csv("~/CaseStudy-1/Dataset/202102-divvy-tripdata.csv")
df5<- read_csv("~/CaseStudy-1/Dataset/202101-divvy-tripdata.csv")
df6 <- read_csv("~/CaseStudy-1/Dataset/202012-divvy-tripdata.csv")
df7 <- read_csv("~/CaseStudy-1/Dataset/202011-divvy-tripdata.csv")
df8 <- read_csv("~/CaseStudy-1/Dataset/202010-divvy-tripdata.csv")
df9 <- read_csv("~/CaseStudy-1/Dataset/202009-divvy-tripdata.csv")
df10 <- read_csv("~/CaseStudy-1/Dataset/202008-divvy-tripdata.csv")
df11 <- read_csv("~/CaseStudy-1/Dataset/202007-divvy-tripdata.csv")
df12 <- read_csv("~/CaseStudy-1/Dataset/202006-divvy-tripdata.csv")
nrow(df1)

################################################# Data Cleaning #################################################

# 1)Concatenating all the CSVs into a single dataframe
binded_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
View(binded_df)

# 2)Removing any empty rows or columns present
binded_df <- remove_empty(binded_df, which=c("cols"))
binded_df <- remove_empty(binded_df, which=c("rows"))

# Data frame dimensions
dim(binded_df)

# Data frame summary
glimpse(binded_df)
summary(binded_df)

# 3)count() returns unique values of the variable passed
binded_df %>% 
  count(start_station_name)

# 4)omitting NA values in the entire dataframe
new_binded_df <- na.omit(binded_df)

# dimensions and summary of new dataframe
dim(new_binded_df)
dim(binded_df)
glimpse(new_binded_df)
summary(new_binded_df)

# 5)Checking for missing values
count(filter(new_binded_df, start_station_name==''),start_station_name, member_casual,sort=TRUE)

new_binded_df$started_at <- as.POSIXct(new_binded_df$started_at, "%Y-%m-%d %H:%M:%S")
new_binded_df$ended_at <- as.POSIXct(new_binded_df$ended_at, "%Y-%m-%d %H:%M:%S")

glimpse(new_binded_df)

# 6)Removing duplicates
print(nrow(new_binded_df))
new_binded_df_no_dups <- new_binded_df[!duplicated(new_binded_df$ride_id), ]
print(nrow(new_binded_df_no_dups))

############################################### Data Manipulation ##############################################

# Creating new features to understand the data better
# Starting with
# 1) riding_time
clean_df <- new_binded_df_no_dups
clean_df <- clean_df %>% 
  mutate(riding_time = as.numeric(ended_at-started_at)/60)

# 2) year_month
clean_df <- clean_df %>% 
  mutate(year_month=paste(strftime(clean_df$started_at, "%Y"), "-",
                          strftime(clean_df$started_at, "%m"),
                          paste("(", strftime(clean_df$started_at, "%b"),")",sep="")))

#Removing year_month with "2021 - 06 (Jun)" values from the data frame
clean_df <- filter(clean_df, year_month!="2021 - 06 (Jun)")

# 3) Weekday
clean_df <- clean_df %>% 
  mutate(weekday=strftime(clean_df$ended_at, "%a"))

# 4) start_hour
clean_df <- clean_df %>% 
  mutate(start_hour=strftime(clean_df$ended_at, format = "%H",tz = "UTC"))

unique(clean_df$start_hour)

# Summary of resultant clean data frame
glimpse(clean_df)
View(clean_df)


################################################## Data Analysis ##############################################

# 1)Let's start by comparing the number of members and casual riders
df <- clean_df
df %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100)

ggplot(df, aes(member_casual, fill=member_casual))+
  geom_bar()+
  labs(title="Chart-1 Member vs Casual distribution")+
  scale_y_continuous(labels=comma)

# 2)year_month vs member_casual
df %>% 
  group_by(year_month) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(year_month)

ggplot(df, aes(year_month, fill=member_casual))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous(labels=comma)

# 3)start_hour vs member_casual
df %>% 
  group_by(start_hour) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100) %>% 
  arrange(start_hour)

ggplot(df, aes(start_hour, fill=member_casual))+
  geom_bar()+
  facet_wrap(~weekday)+
  scale_y_continuous(labels=comma)
View(df)

# 4) Splitting the start_hour into morning, afternoon, and evening
df <- mutate(df, hour_of_the_day=ifelse(df$start_hour<12, "Morning",
                                        ifelse(df$start_hour>=12 & df$start_hour<19, "Afternoon", "Evening")))

ggplot(df, aes(start_hour, fill=member_casual))+
  geom_bar()+
  facet_wrap(~hour_of_the_day, scales = "free")+
  scale_y_continuous(labels=comma)+
  coord_flip()

# 5) weekday vs member_casual
df %>% 
  group_by(weekday) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)

ggplot(df, aes(weekday, fill=member_casual))+
  geom_bar()+
  scale_y_continuous(labels=comma)

# 6) rideable_type vs member_casual
df %>% 
  group_by(rideable_type) %>%
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100,
            members_count=sum(member_casual=="member"),
            members_percent=(sum(member_casual=="member")/length(ride_id))*100,
            casual_count=sum(member_casual=="casual"),
            casual_percent=(sum(member_casual=="casual")/length(ride_id))*100)

ggplot(df, aes(rideable_type, fill=member_casual))+
  geom_bar()+
  scale_y_continuous(labels=comma)+
  facet_wrap(~weekday)

# 7) Riding_time vs start_hour
# Printing the values in each quantiles with 5% difference
quantiles <- quantile(df$riding_time, seq(0,1,by=0.05))
quantiles

# Considering only the values in the 5-95% interval
new_df_without_outliers <- df %>% 
  filter(riding_time > as.numeric(quantiles['5%'])) %>% 
  filter(riding_time < as.numeric(quantiles['95%']))

final_df <- new_df_without_outliers

# member_casual vs riding_time
final_df %>% 
  group_by(member_casual) %>% 
  summarize(mean=mean(riding_time),
            first_quarter=quantile(riding_time, 0.25),
            median=median(riding_time),
            third_quarter=quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

ggplot(final_df, aes(x=member_casual, y=riding_time, fill=member_casual))+
  geom_boxplot()+
  facet_wrap(~weekday)

# weekday vs riding_time
final_df %>% 
  group_by(weekday) %>% 
  summarize(mean=mean(riding_time),
            first_quarter=quantile(riding_time, 0.25),
            median=median(riding_time),
            third_quarter=quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

ggplot(final_df, aes(x=weekday, y=riding_time, fill=member_casual))+
  geom_boxplot()

# year_month vs riding_time
final_df %>% 
  group_by(year_month) %>% 
  summarize(mean=mean(riding_time),
            first_quarter=quantile(riding_time, 0.25),
            median=median(riding_time),
            third_quarter=quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

ggplot(final_df, aes(x=year_month, y=riding_time, fill=member_casual))+
  geom_boxplot()+
  coord_flip()

# start_hour vs riding_time
final_df %>% 
  group_by(start_hour) %>% 
  summarize(mean=mean(riding_time),
            first_quarter=quantile(riding_time, 0.25),
            median=median(riding_time),
            third_quarter=quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

ggplot(final_df, aes(x=start_hour, y=riding_time, fill=member_casual))+
  geom_boxplot()+
  coord_flip()





  



