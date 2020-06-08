library("shiny")
library("shinydashboard")
source("getdata.R") 
library("ggplot2")
countries <- get_countries()
body <- dashboardBody(
  fluidRow(
    box(title="Year or Age Analysis",
        radioButtons(inputId = "what", "By Year or Age",  c("Year" = "Year", "Age" = "Age"))
        
        ),
    box(title = "Country",
        
        selectInput("country", label = h3("Select box"), 
                    choices = list("Australia" = "AUS", 
                                   "Bulgaria" = "BGR", 
                                   "Belarus" = "BLR", 
                                   "Canada" = "CAN", 
                                   "Switzerland" = "CHE", 
                                   "Chile" = "CHL", 
                                   "Denmark" = "DNK", 
                                   "Spain" = "ESP", 
                                   "Estonia"= "EST", 
                                   "Finland" = "FIN", 
                                   "France" = "FRA", 
                                   "Northern Ireland" = "IRL", 
                                   "Scotland" = "SCO", 
                                   "England and Wales" = "GBR", 
                                   "Greece" = "GRC", 
                                   "Hong Kong" = "HKG", 
                                   "Croatia" = "HRV", 
                                   "Hungary" = "HUN", 
                                   "Ireland" = "IRL", 
                                   "Iceland" = "ISL", 
                                   "Israel" = "ISR" ,
                                   "Italy" = "ITA", 
                                   "Japan" = "JPN", 
                                   "Republic of Korea" = "KOR", 
                                   "Lithuania" = "LTU", 
                                   "Luxembourg" = "LUX", 
                                   "Latvia" = "LVA", 
                                   "Netherlands" = "NLD", 
                                   "Norway" = "NOR", 
                                   "Poland" = "POL", 
                                   "Portugal" = "PRT", 
                                   "Russia" = "RUS", 
                                   "Slovakia" = "SVK", 
                                   "Slovenia" = "SVN", 
                                   "Sweden" = "SWE", 
                                   "Taiwan" = "TWN", 
                                   "Ukraine" = "UKR", 
                                   "The United States of America" = "USA"
                    ),
                    selected = 1) # Q: What could selected mean?
    )
        
   
  ),
  
  fluidRow(
    box(title = "",plotOutput("Plot1")),
     
    box(
      title = "", solidHeader = TRUE,
      plotOutput("Plot2")
    )
  
  ),
  
  fluidRow(
    box(
      title = "", background = "light-blue",
      plotOutput("Plot3")
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Row layout"),
  dashboardSidebar(),
  body
)


server <- function(input, output) {
 
  # DEFINE WHAT IS RENDERED (plot, table, text etc.)
  output$Plot1 <- renderPlot({ # RENDER A PLOT
    print("Plot1")
    hold <- get_population_1(input$country)
    cc <- filter(countries,ABBR==input$country)
    country_name <- cc$COUNTRY
    
    if (input$what == "Year") {
      yy <- hold[[1]]
     
      y <- yy$Total
      year_min <- min(yy$Year)
      year_max <- max(yy$Year)
      x <- year_min:year_max
      
      string = paste("Annaul Population for ",country_name)
      plot(x,as.numeric(y),main=string,ylab="Population",xlab="Year ")
    } else {
      yy <- hold[[2]]
      y <- yy$Total
      x <- 0:100
      string = paste("Mean Population byt Age for ",country_name)
      plot(x,as.numeric(y),main=string,ylab="Population",xlab="Age")

  }
    print("Done")
  }) # close renderPlot()
  
  output$Plot2 <- renderPlot({ # RENDER A PLOT
    print("gettting plot 2 data")
    hold <- get_population_1(input$country)
    cc <- filter(countries,ABBR==input$country)
    country_name <- cc$COUNTRY
    if (input$what == "Year") {
      yy <- hold[[1]]
      y <- yy$FemalePct - 50
      yy$y <- y
      year_min <- min(yy$Year)
      year_max <- max(yy$Year)
     
      x <- year_min:year_max
      
    
      yy$x <- x
      yy$cat <- ifelse(as.numeric(yy$y)<0, 1, 2)
      yy$cat <- as.factor(yy$cat)
      print("CCCAATT")
      if (1 %in% yy$cat) { 
        cols = c("red","blue")
      } else {
        cols = c("blue")
          print("aqua")
        }
      print(length(filter(yy,cat == 1)))
       string = paste("Annaul Population for ",country_name)
       title <- paste(country_name," ","Percentage Female - 50, i.e. < 0 more Male, > 0, More Female")
        ggplot(yy, aes(x=x, y=y)) +
          geom_bar(stat = "identity",aes(fill=cat)) + ggtitle(title) +xlab("Year") + ylab("50 - Female %") + scale_fill_manual(values =cols)
    } else {
      yy <- hold[[2]]
      y <- yy$FemalePct - 50
      yy$y <- y
      x <- 0:100
      yy$x <- x
      yy$cat <- ifelse(as.numeric(yy$Hypo)<.05, 1, 2)
      yy$cat <- as.factor(yy$cat)
      if (1 %in% yy$cat) { 
        cols = c("red","blue")
      } else {
        cols = c("blue")
        print("aqua")
      }
      
      title <- paste(country_name," ","Age Percentage Female - 50, i.e. < 0 more Male, > 0, More Female")
      ggplot(yy, aes(x=x, y=y)) +
        geom_bar(stat = "identity",aes(fill=cat)) + ggtitle(title) +xlab("Age") + ylab("50 - Female %")  + scale_fill_manual(values =cols)
    }
  }) # close renderPlot()
  
  
  output$Plot3 <- renderPlot({ # RENDER A PLOT
    print("Plot2")
    hold <- get_population_1(input$country)
    if (input$what == "Year") {
      yy <- hold[[1]]
      
      y <- yy$Growth
      yy$y <- y 
      year_min <- min(yy$Year)
      year_max <- max(yy$Year)
      x <- year_min:year_max
      yy$x <- x
    
      yy$cat <- ifelse(as.numeric(yy$y)<0, 1, 2)
      yy$cat <- as.factor(yy$cat)
      if (1 %in% yy$cat) { 
        cols = c("red","blue")
      } else {
        cols = c("blue")
        print("aqua")
      }
      cc <- filter(countries,ABBR==input$country)
      country_name <- cc$COUNTRY
      string = paste("Annaul Population for ",country_name)
      #    plot(x,as.numeric(y),main=string,ylab="Population",xlab="Year ")
      title <- paste(country_name," ","Growth Rate as a Percentage")
      ggplot(yy, aes(x=x, y=y)) +
        geom_bar(stat = "identity",aes(fill=cat)) + ggtitle(title)+xlab("Year") + ylab("Growth Rate (%)")  + scale_fill_manual(values =cols)
    } else {
      
      yy <- hold[[2]]
     
      y <- yy$Growth
      yy$y <- y
     
      x <- 0:100
      yy$x <- x
      yy$cat <- ifelse(as.numeric(yy$y)<0, 1, 2)
      yy$cat <- as.factor(yy$cat)
      if (1 %in% yy$cat) { 
        cols = c("red","blue")
      } else {
        cols = c("blue")
        print("aqua")
      }
      cc <- filter(countries,ABBR==input$country)
      country_name <- cc$COUNTRY
  
      title <- paste(country_name," ","Growth Rate between Ages as a Percentage")
      ggplot(yy, aes(x=x, y=y)) +
        geom_bar(stat = "identity",aes(fill=cat)) + ggtitle(title)+xlab("Age") + ylab("Growth Rate(%)")  + scale_fill_manual(values =cols)
      
    }
  }) # close renderPlot()
  
  
  
}

# Preview the UI in the console
shinyApp(ui = ui, server)





