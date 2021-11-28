# general testing of what needs to happen in shiny app

library(tidyverse)
library(jsonlite)
library(ggplot2)
library(egg)
library(plotly)

# Employment data from department of labor oui.doleta.gov/unemployment/claims.asp
unemployment <- fromJSON("./data/us_unemployment.txt")$r539cyState$week %>%
  select(-ReflectingWeekEnded) %>%
  mutate(
    weekEnded = as.Date(weekEnded,"%m/%d/%Y"),
    InitialClaims = parse_number(InitialClaims),
    ContinuedClaims = parse_number(ContinuedClaims),
    CoveredEmployment = parse_number(CoveredEmployment),
    InsuredUnemploymentRate=parse_number(InsuredUnemploymentRate),
  )

# COVID policy data from CoronaNet Project
policy_data <- read.csv("./data/coronanet_release_United States of America.csv") %>%
  mutate(
    date_start = as.Date(date_start,"%Y-%m-%d"),
    date_announced = as.Date(date_announced,"%Y-%m-%d"),
    date_end = as.Date(date_end,"%Y-%m-%d"),
    date_updated = as.Date(date_updated,"%Y-%m-%d")
  )

cases <- read.csv("data/time_series_covid19_confirmed_US.csv")
cases <- cbind(cases[,1:11],cases[,seq(15,681,7)]) %>%
  mutate(type = "cases")

deaths <- read.csv("data/time_series_covid19_deaths_US.csv")
deaths <- cbind(deaths[,1:11],deaths[,seq(16,681,7)]) %>%
  mutate(type = "deaths")

merged <- rbind(cases, deaths)

covid <- merged %>%
  pivot_longer(12:107, "date", "X") %>%
  mutate(date=as.Date(date,"%m.%d.%y")) %>%
  group_by(type,Province_State,date) %>%
  summarize(value=sum(value)) %>%
  mutate(new_value=value-lag(value, default=0)) %>%
  ungroup(Province_State, type)


joined <- inner_join(covid, unemployment, by=c("Province_State"="stateName","date"="weekEnded"))
filtered <- policy_data %>%
  filter(province=="North Carolina")

# Unemployment chart
p1 <- joined %>%
  filter(type=="cases",Province_State=="North Carolina") %>%
  pivot_longer(c("InitialClaims","ContinuedClaims"), names_to="claim_type", values_to="claims") %>%
  ggplot(aes(x=date, y=claims, fill=claim_type)) + 
  geom_bar(stat="identity") + 
  geom_vline(data=filtered,mapping=aes(xintercept=date_start))

# Cases chart
p2 <- joined %>%
  filter(Province_State=="North Carolina") %>%
  ggplot(aes(x=date,y=new_value,fill=type)) + 
  geom_area() +
  geom_vline(data=filtered,mapping=aes(xintercept=date_start))

# stacked charts
ggp <- ggarrange(p1, p2, ncol=1, heights=c(3,1))


  