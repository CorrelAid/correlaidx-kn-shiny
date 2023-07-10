################### RShiny Apps: Course Outline #####################
#                                                                   #
#                                                                   #
#       Workshop by CorrelAid e.V. at University of Konstanz        #
#                                                                   #
#Date:  22.06.23                                                    #
#Author: Jan-Henning Weinert                                        #
#####################################################################
#Notiz: Damit der Run App Button einwandfrei funktioniert duerfen keine Umlaute
#im Skript verwendet werden, nicht in Strings, und auch nicht in den Kommentaren.
#Wird alles markiert und ausgefuehrt, funktioniert die App auch so, aber Bilder werden nicht angezeigt

library(shiny) # shiny app
library(ggplot2) # plotting
library(magrittr) # pipe operator %>%
library(shinythemes) # fluidpage(theme = shinytheme("cerulean"))


#runExample("01_hello")

#UI ----
ui<- fluidPage(theme = shinytheme("cosmo"),
  navbarPage("R Shiny Apps", #mehrere Seiten. Es muss ein Name angegeben werden, dieser kann aber auch " " sein.
             #erste Seite: Willkommen screen mit dem Shiny Logo
             tabPanel(title = "Willkommen",
                       tags$img(src ="Shiny-logo.png",
                          width = 800,
                          height = 450,
                          alt = "Shiny Logo",
                          style="display: block; margin-left: auto; margin-right: auto;")
                      
                     # HTML('<center><img src="www/Shiny-logo.png" width="400" height="400"></center>')
                      ),
             #zweite Seite: siehe shiny::runExample("01_hello"). Sowohl ui als auch server teil sind per copy paste aus dem Shiny Example geholt
             tabPanel("Beispiel von den Shiny Entwicklern",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "bins",
                                      label = "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30)
                        ),
                        mainPanel(
                          plotOutput(outputId = "distPlot")
                        )
                      )
                      ),
             #Dritte Seite: Die eigene App bestehend aus zwei Inputs fuer Text und einem Output fuer Text
             tabPanel("die erste Mini-App",
                      sidebarLayout(
                        sidebarPanel(
                          textInput(inputId = "first_name", label = "Vorname"),
                          textInput(inputId = "last_name", label = "Nachname")),
                        mainPanel(
                          h3("ganzer Name"),
                          #Tausche verbatimTextOutput durch Textoutput aus und sieh was passiert
                          verbatimTextOutput(outputId = "example")
                        )
                      )
                      ),
             #Vierte Seite: Die grosse App
             tabPanel("gemeinsames Projekt",
                      titlePanel("CO2-Dataset "), #Titel oben
                      sidebarLayout(
                        #Sidebarpanel mit Inputs
                        sidebarPanel(
                          helpText("In diesem Experiment wurde bei verschiedenen Graspflanzen aus Mississippi und Quebec bei
                                    unterschiedlichen CO2-Konzentrationen gemessen, wieviel CO2 sie an einem Tag aufnehmen. Bei
                                    jeweils 50% der Pflanzen wurde auch die Umgebungstemperatur angepasst."),
                          #Dropdown Menu um zwischen den zwei Kategorien chilled und nonchilled auszuwaehlen
                          selectInput(inputId = "select",
                                      label = "Waehle ein Treatment aus",
                                      choices = levels(CO2$Treatment)), #levels(factor) gibt genau die Moeglichkeiten, die ein Faktor haben kann, zurueck
                          #sliderInput um outlier weglassen zu koennen. Beachte min = min(conc) und max = max(conc) anstatt min=95, max=1000. Immer Hardcode vermeiden!
                          sliderInput(inputId = "slider",
                                      label = "Outlier aus den Daten entfernen",
                                      min = min(CO2$conc),
                                      max = max(CO2$conc),
                                      value = c(min(CO2$conc), max(CO2$conc))), # Gibt man dem Slider als Argument fuer value einen Vektpr mit zwei Werten, so wird es zwei
                                                                                # bewegliche Slider geben. Der Ausgabewert des Sliders ist dann eine Liste, daher muss man 
                                                                                # einen Index verwenden um an die Inputwerte ranzukommen: input$slider[1] und input$slider[2]
                          #Ein Haekchen setzen fuer facet_wrap in ggplot()
                          checkboxInput(inputId = "check", label = "Plot aufteilen?", value = FALSE)
                        ),
                        #Mainpanel fuer die Outputs
                        mainPanel(
                          #Navbarpage mehrere Tabs, Error falls kein Name gegeben wird!
                          navbarPage("Analyse",
                                     #Erster Tab mit Plot und plot downloaden
                                     tabPanel("Plot",
                                              plotOutput("plot"),
                                              downloadButton("download", "Diesen Plot als png runterladen") #downloadbutton koennte auch im SidebarPanel stehen, ich habe den Button lieber direkt beim Plot
                                     ),
                                     #Zweiter Tab in der App, enthaelt lediglich die Tabelle. Die Funktion DatatableOutput ist im Package DT enthalten, daher kann man die Funktion ueber DT:: callen anstatt das ganze Package DT zu laden
                                     tabPanel("Tabelle",
                                              DT::dataTableOutput("table")
                                    )
                                )
                            )
                        )
                    )
                )
)


#Server ----
server <- function(input, output, session){
  
  
  #Der Server Teil fuer die zweite Seite der App: Auch hier habe ich alles aus dem Shiny Example kopiert
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  
  #Dritte Seite: Die zwei Textbausteine miteinander verketten
  output$example <- renderText(paste(input$first_name, input$last_name))
  
  
  #Vierte Seite: Die gemeinsame App
  
  #Daten reactive aufbereiten, selectInput kontrolliert das Treatment sliderInput kontrolliert die maximal und minimalwerte der Konzentration
  data <- reactive({
    CO2 %>%
      subset(CO2$Treatment == input$select) %>%
      subset(CO2$conc >= input$slider[1] & CO2$conc <= input$slider[2])%>%
      na.omit()
  })
  
  #Den Plot erstellen, Wichtig: Die Klammern in data() nicht vergessen. data() ist jetzt eine lokale reaktive Funktion, die inputs und outputs hat und kein statischer Wert mehr.
  # +{if(input$check)facet_wrap(~Type)} Wenn die checkbox angeklickt wird ist ihr Ausgabewert TRUE und der Befehl wird ausgefuehrt, ansonsten wird diese Zeile beim plotten einfach geskippt
  #Der Plot wird hier nicht direkt gezeichnet, sondern als reaktives Objekt p() gespreichert. Das ist zum downloaden sehr nuetzlich.
    p <- reactive(ggplot(data = data(), aes(x = conc, y= uptake))
          +{if(input$check)facet_wrap(~Type)}
          +geom_point(size = 3)
          +theme_linedraw()
          +theme(strip.text = element_text(size = 18))
          +xlab("Konzentration in mL/L")
          +ylab("CO2-Aufnahme in mikromol/qm*sek"))
    
    #Erst hier wird der soeben erstellte Plot als p() aufgerufen und an die UI zurueckgegeben
    output$plot <- renderPlot({
      p()
  })
  
  
  

  #Um den Plot zum Download zur Verfuegung zu stellen kann man einfach p() wieder aufrufen. Wenn der Plot nicht als p() gespeichert werden wuerde,
  #muessten wir den Plot in der Funktion DownloadHandler noch einmal erstellen. Die Speicherung macht den Code lesbarer, weniger fehleranfaellig und effizienter
  output$download <- downloadHandler(
    filename = function(){
      paste("plot_CO2.",Sys.Date(),".png")
    },
    content = function(file){
      png(file)
      print(p())
      dev.off()
    })
  
  
#Die Tabelle fuer den zweiten Tab in der App wird einfach mit der Funktion renderDT erstellt. Da die Funktion im renderDT Package ist rufe ich sie ueber
  #DT::renderDT auf. So muss nicht das ganze Package DT geladen werden
  output$table <- DT::renderDT(
    data()
  )
  
}


#Nachdem sowohl UI und Server definiert worden, werden sie als Argumente der Funktion shinyApp verwendet. Diese Funktion fuehrt die ganze App aus
shinyApp(ui, server)