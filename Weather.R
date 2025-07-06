library(readxl) # to read excel files
library(dplyr) # for data manipulation
library(ggplot2) # for plotting

weather<-read_excel("weatherHistory.xlsx")

head(weather) # see the first few rows

str(weather) # check structure of dataset
summary(weather) # summary stats of numeric columns

# renaming the column
weather<-weather %>% rename(date=`Formatted Date`)


# cleaning column names
colnames(weather)<-make.names(colnames(weather))

colnames(weather)# checking col names

head(weather$date)

weather$date<-as.POSIXct(weather$date,format="%Y-%m-%d %H:%M:%OS %z")

# check for missing values

colSums(is.na(weather))

# extract month from date
weather$month<-format(weather$date,"%B")

# avg temp per month

avg_temp<-weather %>%group_by(month)%>% summarise(avg_temp=mean(Temperature..C.,na.rm=TRUE))

print(avg_temp)

# Line plot

ggplot(weather,aes(x=date,y=Temperature..C.))+
  geom_line(color="blue")+
  labs(title="Temperature over time",x="Date",y="Temperature (C)")