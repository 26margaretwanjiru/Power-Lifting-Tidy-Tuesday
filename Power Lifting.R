# Packages

library(tidyverse)
library(lubridate)
library(gganimate)

# Data

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")


# Data Structure
str(ipf_lifts)

summary(ipf_lifts)

#Extracting the year and month columns

ipf_lifts<-ipf_lifts%>%  
  mutate(date1 = as_datetime(date)) %>% 
  mutate(Year=format(date,"%Y"),Month=format(date,"%B"))

# Group by sex and year

count_lift <- ipf_lifts%>%
  group_by(Year, sex)%>%
  count(sex)

# Data Viz
# manual colors

mycols <- c("black","red")


# The gif

lift_gif <- count_lift%>%
  filter(Year > 2010)%>%
  ggplot(aes(x = " ", y = n, fill = sex)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = as.factor(n)), position = position_stack(vjust = 0.5), size = 8, 
            color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  transition_states(Year, transition_length = 3, state_length = 1)+
  labs(title = "International Powerlifting \n Year : {closest_state}", 
       subtitle = "The increase/ decrease of men and women\n for the last 9 years")+
  theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size=16, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 12, face = "italic", color = "blue"))+
  labs(caption  = "Data Source: OpenPowerlifting.org\n Plot by @magwanjiru")

animate(lift_gif, fps = 10, width = 400, height = 600)
anim_save("lift_gif.gif")
