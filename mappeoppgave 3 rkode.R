install.packages('xml2')
install.packages('XML')
install.packages('rvest')
install.packages('tidyverse')
library(tidyverse)
library(rvest)
library(XML)
library(xml2)
#Jobbet med Herman og Amund.

#Oppgave 1

#Laster inn url
url <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132%22")
  tabell <- url %>% html_table(fill = TRUE)
  bil <- tabell[[1]]
  bil <-bil[-1,]
  colnames(bil) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP", "STOPP", "Avvik")
  #Gjør bil numeric 
  bil$STOPP <- substr(bil$STOPP, 0, 3) %>% 
    as.numeric(bil$STOPP) 
  bil$WLTP <- substr(bil$WLTP, 0, 3) %>% 
    as.numeric(bil$WLTP)
  #Lager plot for faktisk kjørelengde 
  bil1 <-  bil %>%
    ggplot(aes(x =WLTP, y = STOPP)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1.03, col="red")+
    xlim(200,700)+ylim(200,700)+
    labs(title="Forhold mellom oppgitt kjørelengde og faktisk kjørelengde")+
    annotate("text", x=275, y=400, label="Linje som viser hva kjørelengden egentlig skulle vært ->")+
    annotate("text", x= 550, y=250, label="Prikkene viser den faktiske kjørelengden til bilene")+
  theme_bw()
  
  print(bil1)
  
  
  #Oppgave 2
  bil2 <- lm(STOPP ~ WLTP, data = bil)
  
    summary(bil2)
  bil1 + geom_smooth(method=lm, aes(x=WLTP, y=STOPP))
  
  #Man kan tydelig se at det er en nedgang fra forventet kjørelengde og faktisk kjørelengde. 
  #Samtlige biler i dette datasettet er under kjørelengden som de respektive selskapene markedsfører at bilene skal ha.
  
 
  