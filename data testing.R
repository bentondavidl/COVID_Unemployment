# general testing of what needs to happen in shiny app

library(tidyverse)
library(jsonlite)
library(ggplot2)

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

ggplot(unemployment,aes(x=weekEnded, y=ContinuedClaims, fill=stateName)) + geom_bar(stat="identity")

# policy_data <- read.csv("./data/coronanet_release_United States of America.csv")

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
  group_by(date,Province_State,type) %>%
  summarize(value=sum(value)) %>%
  ungroup(Province_State, date)

ggplot(covid, aes(x=date,y=value,fill=type)) + geom_point()

inner_join(covid, unemployment, by=c("Province_State"="stateName","date"="weekEnded"))
