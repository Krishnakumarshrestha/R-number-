test<- read_csv("positive-rate-daily-smoothed.csv")





test <- test %>%
  select(Code,Day,short_term_positivity_rate) %>%
  filter(Code %in% c("NPL","IND"))


plot2 <- test %>%
  filter(Day>=lubridate::ymd("2021-4-1")) %>%
  ggplot()+
  aes(x=Day,weight=short_term_positivity_rate,fill=Code,
      label=paste0(short_term_positivity_rate," %"))+
  
  geom_bar()+
  ggtitle("Total confirmed per test conducted",
          subtitle = "This is 7 days rolling average")+
  facet_wrap(~Code)+
  geom_text(aes(y=short_term_positivity_rate),hjust=-0.05,angle=90)+
  ylab("Positivity % (positivity to testing ratio)")+
  ylim(c(0,40))
plot2

