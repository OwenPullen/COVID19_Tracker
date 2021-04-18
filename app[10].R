### Owen's COVID-19 vaccine Tracker
### Contact: Owen Pullen, owenpullen@hotmail.co.uk
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("ShinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(EpiCurve)) install.packages("EpiCourve", repos = "https://cran.us.r-project.org")

#OWID Data
VaccineData <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
Wales <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Wales.csv")
Angola <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Angola.csv")
Brazil <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Brazil.csv")
China <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/China.csv")
UK <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/United%20Kingdom.csv")
Germany <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Germany.csv")
France <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/France.csv")
India <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/India.csv")
Israel <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Israel.csv")
Italy <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Italy.csv")
Denmark <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Denmark.csv")
SouthK <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/South%20Korea.csv")
USA <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/United%20States.csv")
Ireland <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Ireland.csv")
# JHU Data
ND <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv")
NC <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv")

# Dates
VaccineData$date <- as.Date(VaccineData$date)
Wales$date <- as.Date(Wales$date)
Angola$date <- as.Date(Angola$date)
Brazil$date <- as.Date(Brazil$date)
China$date <- as.Date(China$date)
UK$date <- as.Date(UK$date)
Germany$date <- as.Date(Germany$date)
France$date <- as.Date(France$date)
India$date <- as.Date(India$date)
Israel$date <- as.Date(Israel$date)
Italy$date <- as.Date(Italy$date)
Denmark$date <- as.Date(Denmark$date)
SouthK$date <- as.Date(SouthK$date)
USA$date <- as.Date(USA$date)
Ireland$date <- as.Date(Ireland$date)
ND$date <- as.Date(ND$date)
NC$date <- as.Date(NC$date)

# Subsetting data
ND <- subset(ND, date > as.Date("2021-01-01")) 
NC <- subset(NC, date > as.Date("2021-01-01"))
nc <- NC %>% 
  dplyr::select(where(is.numeric)) %>% abs()
nc$date <- NC$date
nd <- ND %>%
  dplyr::select(where(is.numeric)) %>% abs()
nd$date <- ND$date

## Graphs

country_vac_plot <- function(df) {
  ggplot(df, aes(date, people_vaccinated)) +
    geom_line(color = "Blue", size = 2) +
    labs(x = "Date", y = "Number of People Given 1st Vaccine Dose")
}

country_fully_vac_plot <- function(fvdf) {
  ggplot(fvdf, aes(date, people_fully_vaccinated)) +
    geom_bar(position = "stack", stat = "identity", fill = "red") +
    labs(x = "Date", y = "Number of People Fully Vaccinated")
}
g2 <- function(db) {
  ggplot(db, aes(date, d)) + geom_line(color = "navy", 
   size = 1) + geom_point(alpha=1, color = "navy") +
    labs(x = "1st Jan 2021 - Present", y = "Number of New Deaths Per Day")
}
g3 <- function(dc) {
  EpiCurve(dc, date = "date", freq = "c", period = "value",
    colors ="#9900ef", ylabel = "Number of Positive Tests Per Day", 
     xlabel=sprintf("1st Jan 2021 - Present", min(dc$date), max(dc$date)), 
      square = F)
}

# ui ---------------------------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("sandstone"), title = "Owen's COVID-19 Vaccine Tracker", 
   
             tabPanel("1st Vaccine Dose",
               sidebarLayout(
                 sidebarPanel(
                   pickerInput("country", "Select Country",
                    choices = c("Germany", "France", "Brazil", "Wales", "UK", 
                      "South Korea", "India", "Italy", "Israel", "Denmark", "USA", "Ireland"),
                         selected = c("UK"),
                           multiple = FALSE),
                   "Number of people given 1st vaccine dose, data from Our World in Data"),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("1st Dose Only", 
                              plotOutput("country_plot"))
                   )
                 ))),
               
               tabPanel("Fully Vaccinated",
                        sidebarLayout(
                          sidebarPanel(
                            pickerInput("fullvac", "Select Country",
                             choices = c("Germany", "France", "Brazil", "Wales", "UK", 
                              "South Korea", "India", "Italy", "Israel", "Denmark", "USA", "Ireland"),
                                selected = c("UK"),
                                 multiple = FALSE),
                            "Number of people fully vaccinated, data from Our World in Data"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Fully Vaccinated",
                                       plotOutput("fullyvac_plot")
                              )
                            )
                          )
                        )),
             
             tabPanel("Daily Cases Epidemic Curve",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("cases", "Select Country",
                                      choices = c("Germany", "France", "Brazil", "UK", 
                                                  "South Korea", "India", "Italy", "Israel", "Denmark", "USA", "Ireland"),
                                      selected = c("UK"),
                                      multiple = FALSE),
                          "Number of new cases per day in 2021, data from John Hopkins University"),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("New Cases", 
                                     plotOutput("cases_plot"))
                          ))
                      )),
             
             tabPanel("Daily Deaths",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("deaths", "Select Country",
                                      choices = c("Germany", "France", "Brazil", "UK", 
                                                  "South Korea", "India", "Italy", "Israel", "Denmark", "USA", "Ireland"),
                                      selected = c("UK"),
                                      multiple = FALSE),
                          "Number of new deaths per day in 2021, data from John Hopkins University"),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Daily Deaths", 
                                     plotOutput("deaths_plot"))
                          ))
                      ))
)
  
# Server -------------------------------------------------------------------------------
server <- function(input, output, session) {
  
    country_reactive_df = reactive({
      if (input$country=="Germany") { 
        df = Germany
      }
      if (input$country=="France") { 
        df = France 
      }
      if (input$country=="Brazil") {
        df = Brazil
      }
      if (input$country=="Wales") {
        df = Wales
      }
      if (input$country=="UK") {
        df = UK
      }
      if (input$country=="South Korea") {
        df = SouthK
      }
      if (input$country=="India") {
        df = India
      }
      if (input$country=="Israel") {
        df = Israel
      }
      if (input$country=="Italy") {
        df = Italy
      }
      if (input$country=="Denmark") {
        df = Denmark
      }
      if (input$country=="USA") {
      df = USA
      }
      if (input$country=="Ireland") {
      df = Ireland
      }
      df
    })
  
  output$country_plot <- renderPlot({
    country_vac_plot(country_reactive_df())
  })
  
  reactive_fvdf = reactive({
    if (input$fullvac=="Germany") {
    fvdf = Germany
    }
    if (input$fullvac=="France") {
    fvdf = France
    }
    if (input$fullvac=="Brazil") {
    fvdf = Brazil
    }
    if (input$fullvac=="Wales") {
    fvdf = Wales
    }
    if (input$fullvac=="UK") {
    fvdf = UK
    }
    if (input$fullvac=="South Korea") {
    fvdf = SouthK
    }
    if (input$fullvac=="India") {
    fvdf = India
    }
    if (input$fullvac=="Israel") {
    fvdf = Israel
    }
    if (input$fullvac=="Italy") {
    fvdf = Italy
    }
    if (input$fullvac=="Denmark") {
    fvdf = Denmark
    }
    if (input$fullvac=="USA") {
    fvdf = USA
    }
    if (input$fullvac=="Ireland") {
    fvdf = Ireland
    }
    fvdf
  })
  output$fullyvac_plot <- renderPlot({
    country_fully_vac_plot(reactive_fvdf()) 
  })
  
  cases_reactive_dc = reactive({
    if (input$cases=="Germany") { 
      dc = select(nc, date, Germany)
      dc$c = dc$Germany
    }
    if (input$cases=="France") {
      dc = select(nc, date, France)
      dc$c = dc$France
    }
    if (input$cases=="Brazil") { 
      dc = select(nc, date, Brazil)
      dc$c = dc$Brazil
    }
    if (input$cases=="Ireland") { 
      dc = select(nc, date, Ireland)
      dc$c = dc$Ireland
    }
    if (input$cases=="UK") { 
      dc = select(nc, date, United.Kingdom)
      dc$c = dc$United.Kingdom
    }
    if (input$cases=="South Korea") { 
      dc = select(nc, date, South.Korea)
      dc$c = dc$South.Korea
    }
    if (input$cases=="India") { 
      dc = select(nc, date, India)
      dc$c = dc$India
    }
    if (input$cases=="Italy") {
      dc = select(nc, date, Italy)
      dc$c = dc$Italy
    }
    if (input$cases=="Israel") { 
      dc = select(nc, date, Israel)
      dc$c = dc$Israel
    }
    if (input$cases=="Denmark") {
      dc = select(nc, date, Denmark)
      dc$c = dc$Denmark
    }
    if (input$cases=="USA") {
      dc = select(nc, date, United.States)
      dc$c = dc$United.States
    }
    dc
  })
  
  output$cases_plot <- renderPlot({
    g3(cases_reactive_dc()) #
  })
  
  deaths_reactive_db = reactive({
    if (input$deaths=="Germany") {
      db = select(nd, date, Germany)
      db$d = db$Germany
    }
    if (input$deaths=="France") { 
      db = select(nd, date, France)
      db$d = db$France
    }
    if (input$deaths=="Italy") { 
      db = select(nd, date, Italy)
      db$d = db$Italy
    }
    if (input$deaths=="Brazil") { 
      db = select(nd, date, Brazil)
      db$d = db$Brazil
    }
    if (input$deaths=="India") { 
      db = select(nd, date, India)
      db$d = db$India
    }
    if (input$deaths=="UK") { 
      db = select(nd, date, United.Kingdom)
      db$d = db$United.Kingdom
    }
    if (input$deaths=="Israel") { 
      db = select(nd, date, Israel)
      db$d = db$Israel
    }
    if (input$deaths=="Denmark") {
      db = select(nd, date, Denmark)
      db$d = db$Denmark
    }
    if (input$deaths=="USA") {
      db = select(nd, date, United.States)
      db$d = db$United.States
    }
    if (input$deaths=="South Korea") {
      db = select(nd, date, South.Korea)
      db$d = db$South.Korea
    }
    if (input$deaths=="Ireland") {
      db = select(nd, date, Ireland)
      db$d = db$Ireland
    }
    db
  })
  
  output$deaths_plot <- renderPlot({
    g2(deaths_reactive_db()) #
  })
}  
shinyApp(ui = ui, server = server)