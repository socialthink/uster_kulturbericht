library(RCurl)
library(knitr)
library(dplyr)
library(shiny)
library(shinydashboard)

daten <- read.csv("https://raw.githubusercontent.com/GesellschaftStadtUster/kultur_foerderbeitrage/main/daten_kulturfoerderung_uster.csv")
daten <- daten %>% arrange(desc(DatumEntscheid))
ubersichtdaten <- daten

auswahlbereich <- as.list(c("ALLE BEREICHE",names(table(daten$Foerderbereich))))
auswahljahr<- as.list(c("ALLE JAHRE",names(table(format(as.Date(daten$DatumEntscheid), "%Y")))))
auswahlfoerderart<- as.list(c("ALLE FOERDERARTEN",names(table(daten$Foerderart))))

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Kulturbericht Uster"),
  dashboardSidebar(

    
    sidebarMenu(
    selectInput("plot_bereich",  label = "Förderbereich:", 
                choices = auswahlbereich, selected = 1),
    selectInput("plot_jahr",  label = "Jahr:", 
                choices = auswahljahr, selected = 1),
    selectInput("plot_foerderart",  label = "Förderart:", 
                choices = auswahlfoerderart, selected = 1)
    )
  ),
  
  
  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
     
      
    )
    fluidRow(
      box(tableOutput("tab1", height = 250)),
      
      
      
    )
    
  )
)

server <- function(input, output) {

  output$plot1 <- renderPlot({
    
    if (input$plot_bereich=="ALLE BEREICHE") {
      ubersichtdaten2 <- ubersichtdaten
    } else {
      ubersichtdaten2 <- ubersichtdaten %>% filter(Foerderbereich == input$plot_bereich)
    }
    if (input$plot_jahr=="ALLE JAHRE") {
      ubersichtdaten3 <- ubersichtdaten2
    } else {
      ubersichtdaten3 <- ubersichtdaten2 %>% filter(as.Date(DatumEntscheid) <= as.Date(paste0(input$plot_jahr,"-12-31"))) %>% filter(as.Date(DatumEntscheid) >= as.Date(paste0(input$plot_jahr,"-1-1")))
    }
    if (input$plot_foerderart=="ALLE FOERDERARTEN") {
      ubersichtdaten4 <- ubersichtdaten3
    } else {
      ubersichtdaten4 <- ubersichtdaten3 %>% filter(Foerderart == input$plot_foerderart)
    } 
    
    
    hist(ubersichtdaten4$FoerderbeitragCHF/1000, breaks=seq(0, max(daten$FoerderbeitragCHF)/1000, length.out = as.integer(max(daten$FoerderbeitragCHF)/5000)) ,xlim=c(0,max(daten$FoerderbeitragCHF)/1000), ylim=c(0,dim(daten)[1]) , xlab = "Förderbeiträge [in 1'000 Franken']",ylab = "Häufigkeit [Anz. Beiträge]", main = "Höhe der einzelnen Beiträge in der Kulturförderung")
  })
  

  output$tab1 <- renderTable({
    if (input$plot_bereich=="ALLE BEREICHE") {
      ubersichtdaten2 <- ubersichtdaten
    } else {
      ubersichtdaten2 <- ubersichtdaten %>% filter(Foerderbereich == input$plot_bereich)
    }
    if (input$plot_jahr=="ALLE JAHRE") {
      ubersichtdaten3 <- ubersichtdaten2
    } else {
      ubersichtdaten3 <- ubersichtdaten2 %>% filter(as.Date(DatumEntscheid) <= as.Date(paste0(input$plot_jahr,"-12-31"))) %>% filter(as.Date(DatumEntscheid) >= as.Date(paste0(input$plot_jahr,"-1-1")))
    }
    if (input$plot_foerderart=="ALLE FOERDERARTEN") {
      ubersichtdaten4 <- ubersichtdaten3
    } else {
      ubersichtdaten4 <- ubersichtdaten3 %>% filter(Foerderart == input$plot_foerderart)
    } 
    
    ubersichtdaten4$DatumEntscheid <- format(as.Date(ubersichtdaten4$DatumEntscheid), "%d.%m.%Y")
    ubersichtdaten4
  })
  
}

shinyApp(ui, server)