################### R Shiny Apps: Names App #########################
#                                                                   #
#                                                                   #
#       Workshop by CorrelAid e.V. at University of Konstanz        #
#                                                                   #
#Datum: 22.06.2023                                                  #
#Autor: Jan-Henning Weinert                                         #
####################################################################


library(shiny)

ui <- fluidPage(
  #Titel
  titlePanel(h2("Titel")),
  #Layout mit Sidebar und Main Panel
  sidebarLayout(
    sidebarPanel(
      #Zwei Textinputs mit unterschiedlicher InputId
      textInput(inputId = "first_name", label = "Vorname"),
      textInput(inputId = "last_name", label = "Nachname")),
    mainPanel(
      h3("Name"),
      #TextOutput mit eigener OutputId
      #verbatimTextOutput?
      textOutput(outputId = "full_name")
  )
))

server <- function(input, output, session){
  #Im Output$full_name werden die Ausgabewerte der beiden Inputs einfach miteinander verkettet
  #renderText, da wir einen TextOutput in der UI verwenden
  #renderPrint?
  output$full_name <- renderText(paste(input$first_name, input$last_name))
}

shinyApp(ui = ui, server = server)

