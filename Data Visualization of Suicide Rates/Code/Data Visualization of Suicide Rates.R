library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

data <- read.csv('G:/2-Study/R/Data Visualization of Suicide Rates/master.csv')

# 1.Deaths from Suicide by the year worldwide
year_data <- data %>% group_by(year) %>%
  summarise(Total = sum(suicides_no))
datatable(year_data)

ggplot(year_data, aes(year, Total)) + 
  geom_bar( stat = "identity", fill = "#BC8F8F", color = "blue") +
  ggtitle("Deaths from Suicide Every Year") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# 2.Deaths from Suicide by the year and age
ay_data <- data %>% group_by(age, year) %>%
  summarise(Total = sum(suicides_no))
# bar graph
ggplot(ay_data, aes(year, Total, fill = age)) + 
  geom_bar( stat = "identity") +
  ggtitle("Deaths from Suicide by Year and Age") +
  scale_y_continuous(labels = comma)
# line graph
ggplot(data=ay_data, aes(x=year ,y=Total,group=age, colour=age)) +
  geom_line()+    
  ggtitle("Deaths from Suicide by Year and Age")+
  geom_point()

# 3. Deaths from Suicide by year and gender
sy_data <- data %>% group_by(sex,year) %>%
  summarise(Total = sum(suicides_no))
datatable(sy_data)
# bar graph
ggplot(data=sy_data, aes(year,Total,fill=sex))+
 geom_bar( stat = "identity") +
 ggtitle("Deaths from Suicide by Year and Sex") +
 scale_y_continuous(labels = comma)
# line graph
ggplot(data=sy_data, aes(x=year ,y=Total,group=sex, colour=sex)) +
  geom_line()+    
  ggtitle("Deaths from Suicide by Year and Sex")+
  geom_point()
# pie chart
x_data <- data %>% group_by(sex) %>%
  summarise(Total = sum(suicides_no))
datatable(x_data)
pct <- round(x_data$Total/sum(x_data$Total)*100)
ggplot(data=x_data, aes(x="",y=Total,fill=sex))+
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Deaths from Suicide by Gender - Female(23%) - male(77%)") 

# 4. Deaths from Suicide by generation 
g_data <- data %>% group_by(generation,year) %>%
  summarise(Total = sum(suicides_no))
datatable(g_data)
# pie chart
ggplot(data=g_data, aes(x="",y=Total,fill=generation))+
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Deaths from Suicide by Generation") 
# line graph
ggplot(data=g_data, aes(x=year ,y=Total,group=generation, colour=generation)) +
  geom_line(linetype=2)+    
  ggtitle("Deaths from Suicide by Generation")+
  geom_point()


# 5. Deaths from suicide by Generation and gender
gs_data <- data %>% group_by(generation,sex) %>%
  summarise(Total = sum(suicides_no))
datatable(gs_data)
# bar chart
ggplot(data=gs_data, aes(generation, Total, fill=sex))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Deaths from suicide by Generation and Sex")+
  scale_y_continuous(labels = comma)

# 6. Deaths from Suicide by country
cp_data <- data %>% group_by(ï..country, year) %>%
  summarise(Total = sum(suicides_no),sum(population))
datatable(cp_data)
cp_data$Total <- (100*cp_data$Total) / cp_data$`sum(population)`
datatable(cp_data)

temp_data <- cp_data %>% group_by(ï..country) %>%
  summarise(Suicide_rate = mean(Total)) %>%
  arrange(desc(Suicide_rate))
datatable(temp_data)

# horizontal bar chart
ggplot(data=temp_data[1:25,], aes(ï..country, Suicide_rate))+
  geom_bar(stat = "identity",fill = 'red')+
  ggtitle("Top 10 Countries where Deaths from Suicide is the highest")+
  coord_flip()
ggplot(data=temp_data[75:100,], aes(ï..country, Suicide_rate))+
  geom_bar(stat = "identity",fill = 'green')+
  ggtitle("Top 10 Countries where Deaths from Suicide is the lowest")+
  coord_flip()
