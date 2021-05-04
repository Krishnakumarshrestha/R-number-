source("pkg.R")
source("data.R")

plot5<-
  np_df %>%
  mutate(date=lubridate::mdy(date))%>%
  pivot_longer(3:5,names_to = "state",values_to = "number")%>%
  ggplot()+
  aes(x=date,y=number,col=state)+
  geom_point()+
  ggtitle("Cumulative Positive Cases,recovered,dead")
plot5
