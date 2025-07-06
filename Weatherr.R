# Load
library(readxl) # to read excel files
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(lubridate) # for working with dates

# load the dataset
weather<-read_excel("weatherHistory.xlsx")

# checking the first few column names
colnames(weather)

# rename
weather<-weather %>% rename(
  date=`Formatted Date`,
  summary=`Summary`,
  precipt_type=`Precip Type`,
  temp=`Temperature (C)`,
  app_temp=`Apparent Temperature (C)`,
  humidity=`Humidity`,
  wind_speed=`Wind Speed (km/h)`,
  wind_bearing=`Wind Bearing (degrees)`,
  visibility= `Visibility (km)`,
  loud_cover=`Loud Cover`,
  pressure=`Pressure (millibars)`,
  daily_summ=`Daily Summary`
)

colnames(weather)<-make.names(colnames(weather))

str(weather) # check structure of dataset

summary(weather) # summary statistics

weather$date<-as.POSIXct(weather$date,format="%Y-%m-%d %H:%M:%OS %z")

# extracting only the date part

weather$only_date<-as.Date(weather$date)

# month-year coln

weather$month_year<-floor_date(weather$date,unit="month")

# missing values

weather<-weather%>%
  filter(!is.na(temp),!is.na(app_temp),!is.na(humidity),!is.na(precipt_type))

daily_avg<-weather%>% group_by(only_date) %>%
  summarise(avg_temp=mean(temp),avg_humidity=mean(humidity)
)

ggplot(daily_avg,aes(x=only_date))+
  geom_line(aes(y=avg_temp),color="red",size=1)+
  geom_line(aes(y=avg_humidity*100),color="blue",linetype="dashed")+
  labs(title="Daily Avg Temp and Humidity",
       x="Date",y="Temp (C)/Humidity(%)",
       caption="Humidity is scaled to 0-100%")+
  theme_minimal()
# Monthly avg

monthly_avg <- weather %>%
  group_by(month_year) %>%
  summarise(
    avg_temp = mean(temp),
    avg_app_temp = mean(app_temp)
  )

ggplot(monthly_avg, aes(x = month_year)) +
  geom_line(aes(y = avg_temp, color = "Actual Temp"), size = 1.2) +
  geom_line(aes(y = avg_app_temp, color = "Apparent Temp"), size = 1.2) +
  labs(title = "Monthly Actual vs Apparent Temperature",
       x = "Month", y = "Temperature (°C)") +
  scale_color_manual(values = c("Actual Temp" = "orange", "Apparent Temp" = "purple")) +
  theme_minimal()
                              
weather %>%
  group_by(precipt_type) %>%
  summarise(avg_temp = mean(temp)) %>%
  ggplot(aes(x = precipt_type, y = avg_temp, fill = precipt_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Avg Temp by Precipitation Type", x = "Precipitation", y = "Avg Temp (°C)") +
  theme_minimal() +
  theme(legend.position = "none")

weather %>%
  group_by(summary) %>%
  summarise(avg_temp = mean(temp)) %>%
  arrange(desc(avg_temp)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(summary, avg_temp), y = avg_temp, fill = summary)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Weather Types by Avg Temperature", x = "Summary", y = "Avg Temp (°C)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("monthly_temp_plot.png", width = 8, height = 5)
