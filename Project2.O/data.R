library(tidyverse)

# code to extract confirmed case

df_confirmed<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")


df_confirmed <- df_confirmed %>%
  select(`Country/Region`,5:ncol(df_confirmed))
df_confirmed <- df_confirmed %>%
  pivot_longer(2:ncol(df_confirmed),names_to = "date")
df_np_confirmed<- df_confirmed %>%
  filter(`Country/Region`=="Nepal")
colnames(df_np_confirmed) <- c("country","date","confirmed")
df_in_confirmed<- df_confirmed %>%
  filter(`Country/Region`=="India")
colnames(df_in_confirmed) <- c("country","date","confirmed")


# death data extract
df_death<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
df_death <- df_death %>%
  select(`Country/Region`,5:ncol(df_death))

df_death<- df_death %>%
  pivot_longer(cols=2:ncol(df_death),names_to = "date",values_to = "death")
np_df_death<- df_death %>%
  filter(`Country/Region`=="Nepal")
colnames(np_df_death)<-c("country","date","death")
in_df_death <- df_death %>%
  filter(`Country/Region` =="India")
colnames(in_df_death)<-c("country","date","death")
# recovered data extract

df_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
df_recovered <- df_recovered %>%
  select(`Country/Region`,5:ncol(df_recovered))
df_recovered <- df_recovered %>%
  pivot_longer(cols = 2:ncol(df_recovered),
               names_to = "date",
               values_to = "recovered")
np_df_recovered<- df_recovered %>%
  filter(`Country/Region`=="Nepal")
colnames(np_df_recovered)<-c("country","date","recovered")
in_df_recovered <- df_recovered %>%
  filter(`Country/Region`=="India")
colnames(in_df_recovered)<-c("country","date","recovered")

np_df<- df_np_confirmed %>%
  full_join(np_df_death)
np_df <- np_df %>%
  full_join(np_df_recovered)
# final data 
# all of them are cumulative



in_df<- df_in_confirmed %>%
  full_join(in_df_death)
in_df <- in_df %>%
  full_join(in_df_recovered)
