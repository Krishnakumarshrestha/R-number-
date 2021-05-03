# loading data from source file
source("data.R")
source("pkg.R")
#Here we define the incubation period and generation time based on literature estimates for 
# Covid-19 (see here for the code that generates these estimates).

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#An arbitrary number of delay distributions are supported with the most common use case
#likely to be a incubation period followed by a reporting delay.
set.seed(100)
reporting_delay <- estimate_delay(rlnorm(500,  log(3), 1),
                                  max_value = 25, bootstraps = 1)

df<-np_df %>%
  
  mutate(date=lubridate::mdy(date)) %>%
  mutate(per_day=
           ifelse(confirmed==0,
                  confirmed, 
                  confirmed -lag(confirmed)))

cases <- df %>%
  select(date,per_day) %>%
  filter(per_day > 50)

colnames(cases)<-c("date","confirm")

cases<-cases[250:nrow(cases),]

out <- epinow(reported_cases = cases, 
              generation_time = generation_time,
              delays = delay_opts(incubation_period, reporting_delay),
              rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
              # here we define the quality of the gaussian process approximation
              # if the fit to data appears poor try increasing basis_prop and
              # potentially the boundary_scale (see ?gp_opts for details)
              # though this will likely increase runtimes.
              gp = gp_opts(basis_prop = 0.2),
              # in some instances stan chains can get stuck when estimating in 
              # these instances consider using the future fitting mode by passing 
              # `future = TRUE, max_execution_time = 60 * 30` to stan_opts and calling 
              # `future::plan("multiprocess")` prior to running epinow this will time out
              # chains after 30 minutes but still return results from completed chains
              stan = stan_opts(),
              horizon = 14, 
              target_folder = "results",
              logs = file.path("logs", Sys.Date()),
              return_output = TRUE, 
              verbose = TRUE)
