library(jsonlite)
library(tidyverse)
library(ggrepel)
library(formattable)

# OPPGAVE 1

link <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

covid <- link

covid <- covid %>% 
  mutate(percent_vac = fully_vaccinated_pct_of_pop * 100)
covid$name <- state.abb[match(covid$name, state.name)]
covid[is.na(covid)] <- "DC"

covid$fully_vaccinated_pct_of_pop = as.numeric(covid$fully_vaccinated_pct_of_pop)
covid$deaths_per_100k = as.numeric(covid$deaths_per_100k)
covid$name = as.character(covid$name)


covid %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name)) +
  geom_point(color="green4", size=3) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x= "Prosentandel fullvaksinert",
       y= "Dødsrate") +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = .81, by = 0.05)) +
  geom_label_repel(size = 4,
                   label.size = 0,
                   label.padding = 0.15,
                   label.r = 0,
  )  +
  theme_gray() +
  annotate(geom="text", x=0.56, y=17, 
           label=" 🢆Lower Vaccination rate, higher death rate") +
  annotate(geom="text", x=0.73, y=8, 
           label="🢆 Higer Vaccination rate, Lower death rate")

# Oppgave 2

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covid)


covid %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name), col="green") +
  geom_point(color = "green", size = 1) +
  geom_smooth(method = lm) +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = .80, by = 0.05)) +
  geom_label_repel(size= 3.5,
                   label.size = 0,
                   label.padding = 0,
                   label.r = 0,
  )  +
  geom_point(color= "green", size = 3) + 
  labs(title ="Corona",
       x="prosentandel fullvaksinerte",
       y="Dødsrate") + 
  theme_bw() +
  annotate(geom="text", x=0.56, y=17, 
           label="🢆Lower Vaccination rate, higher death rate") +
  annotate(geom="text", x=0.73, y=8, 
           label="🢆 Higer Vaccination rate, Lower death rate")

#basert på det man ser i grafen kan man se at dødsraten synker 
#gjennomsnittelig i takt med prosentandel som er fullvaksinerte.
#Hvis det er flere fullvaksinerte vil dødsraten som regel synke, men det er noen unntak.
#florida har for eksempel relativt høy dødsrate selv om de har relativt stor prosentandel fullvaksinerte. 
#Grunnen til dette kan være at Florida er en mer liberal stat enn mange av de andre så de har 
#hatt færre restriskjoner lenger enn de andre satene som kan ha hatt en invirkning på dødsraten i staten relativt til de andre statene.