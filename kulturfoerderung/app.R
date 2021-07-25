library(shiny)
library(RCurl)
library(knitr)
library(dplyr)
library(bslib)


daten <- read.csv("https://raw.githubusercontent.com/GesellschaftStadtUster/kultur_foerderbeitrage/main/daten_kulturfoerderung_uster.csv")
ubersichtdaten <- daten %>% arrange(desc(DatumEntscheid))

auswahlbereich <- as.list(c("ALLE BEREICHE",names(table(daten$Foerderbereich))))
auswahljahr<- as.list(c("ALLE JAHRE",names(table(format(as.Date(daten$DatumEntscheid), "%Y")))))
auswahlfoerderart<- as.list(c("ALLE FOERDERARTEN",names(table(daten$Foerderart))))


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "united"),
    # Application title
    titlePanel("Kulturförderung der Stadt Uster - ein Überblick"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("plot_bereich",  label = "Förderbereich:", 
                        choices = auswahlbereich, selected = 1),
            selectInput("plot_jahr",  label = "Jahr:", 
                        choices = auswahljahr, selected = 1),
            selectInput("plot_foerderart",  label = "Förderart:", 
                        choices = auswahlfoerderart, selected = 1),
            HTML("<b>Datengrundlage</b><br>
Die Auswertung basiert auf <a href=\"https://opendata.swiss/de/dataset/beitrage-der-kulturforderung-der-stadt-uster-ab-2018\" target=_blank>offenen Daten der Stadt Uster</a>. Die Daten stehen unter <a href=\"https://creativecommons.org/licenses/by/4.0/\" target=_blank>CC BY 4.0</a> zur Verfügung.
<br><br>
<b>Auswertung</b><br>
Die <a href=\"https://github.com/socialthink/uster_kulturbericht\" target=_blank>Auswertung</a> wurde von <a href=\"https://socialthink.ch/\" target=_blank>Andreas Wyss</a> auf Grundlage von <a href=\"https://de.wikipedia.org/wiki/R_(Programmiersprache)\" target=_blank>R</a> und <a href=\"https://shiny.rstudio.com/\" target=_blank>Shiny</a> entwickelt und steht unter einer <a href=\"https://github.com/socialthink/uster_kulturbericht/blob/master/LICENSE\" target=_blank>MIT Lizenz</a> zur Verfügung."
            
        )),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                    tabPanel("Tabelle",tableOutput("tab1")),
                    tabPanel("Beitrag pro Jahr",plotOutput("plot2")),
                    tabPanel("Höhe der Beiträge",plotOutput("plot1"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    daten <- read.csv("https://raw.githubusercontent.com/GesellschaftStadtUster/kultur_foerderbeitrage/main/daten_kulturfoerderung_uster.csv")
    ubersichtdaten <- daten %>% arrange(desc(DatumEntscheid))
    
    auswahlbereich <- as.list(c("ALLE BEREICHE",names(table(daten$Foerderbereich))))
    auswahljahr<- as.list(c("ALLE JAHRE",names(table(format(as.Date(daten$DatumEntscheid), "%Y")))))
    auswahlfoerderart<- as.list(c("ALLE FOERDERARTEN",names(table(daten$Foerderart))))

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
        colnames(ubersichtdaten4) <- c("Datum","Gesuchssteller","Projekt","Förderbereich","Förderart","CHF")
        ubersichtdaten4
    })
    
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
        hist(ubersichtdaten4$FoerderbeitragCHF/1000, breaks=seq(0, max(daten$FoerderbeitragCHF)/1000, length.out = as.integer(max(daten$FoerderbeitragCHF)/5000)) ,xlim=c(0,max(daten$FoerderbeitragCHF)/1000), ylim=c(0,dim(daten)[1]) , xlab = "Förderbeitrag [in 1'000 Franken']",ylab = "Häufigkeit [Anz. Beiträge]", main = "Höhe der einzelnen Beiträge in der Kulturförderung")
    })
    
    output$plot2 <- renderPlot({
            
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
            
            
            
            ## Jahresbeiträge Summe berechnen
            
            if(dim(ubersichtdaten4)[1]==0){
                summe <- 0
                anzahljahre <- 0
                startjahr <- "keine Förderbeiträge\nmit dieser Auswahl"
            } else {
                ubersichtdaten4$DatumEntscheid <- as.Date(ubersichtdaten4$DatumEntscheid)
                anzahljahre <- as.numeric(format(max(as.Date(ubersichtdaten4$DatumEntscheid)), format="%Y")) - as.numeric(format(min(as.Date(ubersichtdaten4$DatumEntscheid)), format="%Y"))
                startjahr <- as.numeric(format(min(as.Date(ubersichtdaten4$DatumEntscheid)), format="%Y"))
                wert <- ubersichtdaten4 %>% filter(DatumEntscheid >= as.Date(paste0(startjahr,"-01-01")) & DatumEntscheid <= as.Date(paste0(startjahr,"-12-31")))
                summe <- sum(wert$FoerderbeitragCHF)
                jahre <- startjahr
            }
            
            if (anzahljahre==0) {
                jahre <- startjahr
            } else {
                for(i in 1:anzahljahre){
                    wert <- ubersichtdaten4 %>% filter(DatumEntscheid >= as.Date(paste0(startjahr+i,"-01-01")) & DatumEntscheid <= as.Date(paste0(startjahr+i,"-12-31")))
                    wert <- sum(wert$FoerderbeitragCHF)
                    summe <- c(summe,wert)
                    jahre <- c(jahre,startjahr+i)
                }
            }
            
            
            
            ## Plot
            
            barplot(summe/1000,names.arg = jahre, ylim=c(0,600), main = "Summe pro Jahr", ylab = "Förderbeitrag pro Jahr [in 1'000 Franken]")
            
            
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
