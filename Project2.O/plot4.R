source("pkg.R")
source("data.R")

plot4<-
np_df %>%
  mutate(date=lubridate::mdy(date))%>%
  ggplot()+
  aes(x=date,weight=confirmed)+
    geom_bar()+
  ggtitle("Cumulative Positive Cases")

plot4