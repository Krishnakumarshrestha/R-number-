source("pkg.R")
source("data.R")



plot3<-np_df %>%
  mutate(date=lubridate::mdy(date),
         mortality_rate=(death/confirmed)) %>%
  filter(mortality_rate >0) %>%
  filter(date >= lubridate::ymd("2020-10-10"))%>%
  ggplot()+
  aes(x=date,y=mortality_rate)+
  geom_point()+ 
  geom_line()+
  ggtitle("Mortality Rate in Nepal")
