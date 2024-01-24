- 👋 Hi, I’m @JamesChinyahara
- 👀 I’m interested in learning all things software especially data analysis
- 🌱 I’m currently learning data analysis using the R programming
- 💞️ I’m looking to collaborate on anything involving R for data analysis
- 📫 How to reach me jchinyahara@gmail.com
- 😄 Pronouns: He
- ⚡ Fun fact: I live  and work in Zimbabwe

<!---
JamesChinyahara/JamesChinyahara is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
#Cyclistic Bike Share Program

##Tripdata_2023_Q1

install.packages("tidyverse")
library("tidyverse")

read_csv("2023_Q1_cleaned_export_final.csv")

tripdata <- read_csv("2023_Q1_cleaned_export_final.csv")

View(tripdata)

install.packages("skimr")
library("skimr")

str(Summary_weekday_with_count)


skim_without_charts(tripdata)

glimpse(tripdata)

View(tripdata)

cleaned <- separate(tripdata, started_on, into = c("day","date","month","year" ))


cleaned %>% group_by(day) %>% drop_na() %>% summarise(Average_Duration=mean(Duration_hrs_min_ss))

install.packages("ggplot2")
library("ggplot2")



dailyusage <- cleaned %>% select(day, Duration_hrs_min_ss, `Member_casual(trimmed)`)


dailyusage %>% 
  group_by(`Member_casual(trimmed)`) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))


finaltripdata <- dailyusage %>% 
  group_by(`Member_casual(trimmed)`) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))

install.packages("ggplot2")
library("ggplot2")


cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`)

tripdaily <- cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`) %>% 
  group_by(day) %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))

dailyusage %>% 
  group_by(day) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))

Weekday_Usage <- finaltripdata <- dailyusage %>% 
  group_by(`Member_casual(trimmed)`) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))



library("ggplot2")

ggplot(data = finaltripdata)+geom_bar(mapping = aes(x=Member_casual(trimmed), y=average_duration))

cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`) %>% 
  group_by(day) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))

Weekday_summary <- cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`) %>% 
  group_by(day) %>% 
  drop_na() %>% 
  summarise(average_duration=mean(Duration_hrs_min_ss))


cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`) %>% 
  group_by(day) %>% 
  drop_na() %>% 
  summarise(n=n(),average_duration=mean(Duration_hrs_min_ss))

Summary_weekday_with_count<-cleaned %>% 
  select(`rideable_type(trimmed)`,day, date,Duration_hrs_min_ss,`Member_casual(trimmed)`) %>% 
  group_by(day) %>% 
  drop_na() %>% 
  summarise(n=n(),average_duration=mean(Duration_hrs_min_ss))


Summary_weekday_with_count$day<- as.factor(Summary_weekday_with_count$day)
levels(Summary_weekday_with_count$day)

Summary_weekday_with_count$day <- factor(Summary_weekday_with_count$day,
                                         levels = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

str(Summary_weekday_with_count)

ggplot(data = Summary_weekday_with_count)+geom_bar(mapping = aes(x=day))

head(Summary_weekday_with_count)

ggplot(data = Summary_weekday_with_count)+
  geom_col(mapping = aes(x=day, y=average_duration))


ggplot(data = finaltripdata)+
  geom_col(mapping = aes(x='Member_casual(trimmed)', y='average_duration'))

head(finaltripdata)
