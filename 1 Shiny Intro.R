################### R Shiny Apps: Introduction ######################
#                                                                   #
#                                                                   #
#       Workshop by CorrelAid e.V. at University of Konstanz        #
#                                                                   #
#Datum : 22.06.2023                                                 #
#Autor: Jan-Henning Weinert                                         #
#####################################################################

#R und RStudio

#Wie führe ich Code im Skript aus? Was ist die Konsole?

#Wie stoppe ich den Code wieder?


#R als Taschenrechner
17+25
28/7
x <- 10
y <- 5
xy <- x+y
xy
1:10
c(1,2,3,4,5)

#R ist eine speziell für statistische Anwendungen entwickelte Sprache
x <- 1:10
y <- 15:24
(result <- t.test(x,y))
str(result)
#indexing
result$p.value
result$conf.int[2]



#packages
install.packages(c("vtable", "magrittr"))



#R Data Sets
data()



#Datensatz faithful enthält die Wartezeit und die Dauer für 
#Eruptionen des 'Old Faithful' Geysirs im Yellowstone National Park, USA.
faithful
head(faithful)
tail(faithful)
summary(faithful)



#packages
install.packages(c("vtable", "magrittr"))


vtable::st(faithful)

library(vtable)
st(faithful)

#Da R Open-Source ist und jeder ein Package schreiben und veröffentlichen kann,
#gibt es häufig mehrere ans Ziel zu kommen 
#Tidyverse


#Der pipe Operator %>%
library(magrittr)

summary(subset(faithful, waiting > 85))

faithful %>% 
  subset(waiting > 85) %>% 
  summary()



#Ein Histogram der Wartezeiten 
x    <- faithful$waiting
bins <- seq(min(x), max(x), length.out = 15 +1)
    
hist(x, breaks = bins, col = "#75AADB", border = "white",
     xlab = "Waiting time to next eruption (in mins)",
     main = "Histogram of waiting times")



#Mit Shiny werden unsere Plots dynamisch <- REACTIVE!
#Beispiel der Shiny Entwickler
library(shiny)
runExample("01_hello")
