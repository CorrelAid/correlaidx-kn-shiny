
################### R Shiny Apps: Template ##########################
#                                                                   #
#                                                                   #
#       Workshop by CorrelAid e.V. at University of Konstanz        #
#                                                                   #
#Datum: 22.06.2023                                                  #
#Autor: Jan-Henning Weinert                                         #
#####################################################################

#runExample("01_hello")



library(shiny)
library(tidyverse)


ui <- fluidPage(
  navbarPage(title = "NavbarPage()", #Die Navigationsbar oben funktioniert nur, wenn ein Titel angegeben wird
              #Innerhalb der Funktion NavbarPage() kann man dann mehrere sogenannte TabPanels definieren
              tabPanel("Tab 1",
              titlePanel("CO2"),
              sidebarLayout(
                sidebarPanel(
                   selectInput(inputId = "select",
                               label = "select",
                               choices = levels(CO2$Treatment))
                ),
                mainPanel(
                    plotOutput(outputId = "plot")
                )
              )
            ),
            #Ein zweites TabPanel daneben
            tabPanel("Tab 2",
             titlePanel("Tab 2"),
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
        )
      )
    )
  )

server <- function(input, output, session){
  
  output$plot <- renderPlot({
    data <- CO2 %>%
      filter(Treatment == input$select)
    
    ggplot(data, aes(x= conc, y= uptake)) + geom_point() + theme_minimal()
  })
  
}
shinyApp(ui = ui, server = server)