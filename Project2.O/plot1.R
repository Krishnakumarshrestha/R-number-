source("pkg.R")
source("data.R")
head(np_df)
head(in_df)
np_df<- np_df %>%
  
  select(date,confirmed)
colnames(np_df)<-c("date","Nepal")


in_df<- in_df %>%
  
  select(date,confirmed)
colnames(in_df)<-c("date","India")


df<- np_df %>% full_join(in_df)

df<- df %>%
  pivot_longer(2:3,names_to = "country","confirmed")
df <- df %>%
  mutate(date=lubridate::mdy(date))


df <- df %>%
  filter(date >=as.Date("2021-03-01"))

df<- df %>%
  group_by(country) %>%
  mutate(per_day=
           ifelse(value==0,
                  value, 
                  value -lag(value))) %>%
  ungroup()


p1<-df %>% 
  
  filter(country=="Nepal")%>%
  ggplot()+
  aes(x=date,y=per_day, label =per_day)+
  ylim(c(0,8000))+
  geom_col(fill="red")+
  geom_text(hjust = -0.05, angle=90,size=3)+
  ggtitle("Comparison between Nepal and India (Daily Cases)")+
  theme(plot.title = element_text(hjust = 0.5,size =15,color = "black"),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())

p2<- df %>% 
  
  filter(country=="India")%>%
  ggplot()+
  aes(x=date,y=per_day, label =per_day)+
  ylim(c(0,500000))+
  geom_col(fill="blue") +
  geom_text(hjust = -0.05, angle=90,size=3)+
  theme(
    axis.text.y =element_blank(),
    axis.ticks.y =element_blank())
figure <- ggarrange( p1, p2,
                     labels = c("NEPAL", "INDIA"),
                   ncol = 1, nrow = 2)
figure

