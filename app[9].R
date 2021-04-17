### Shiny app template
### Owen's vaccine Tracker
### Contact: Owen Pullen, owenpullen@hotmail.co.uk
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("ShinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

#Data
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
France <- as.data.frame(France)

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


# ui ---------------------------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("sandstone"), title = "Owen's Vaccine Tracker", 
   
             tabPanel("1st Vaccine Dose",
               sidebarLayout(
                 sidebarPanel(
                   pickerInput("country", "Select Country",
                    choices = c("Germany", "France", "Brazil", "Wales", "UK", 
                      "Angola", "India", "Italy", "Israel", "Denmark"),
                         selected = c("Germany"),
                           multiple = FALSE)),
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
                              "South Korea", "India", "Italy", "Israel", "Denmark"),
                                selected = c("Germany"),
                                 multiple = FALSE)),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Fully Vaccinated",
                                       plotOutput("fullyvac_plot")
                              )
                            )
                          )
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
      if (input$country=="Angola") {
        df = Angola
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
      df ### EP: you need this at the end to basically 'print' the reactive element
    })
  
  output$country_plot <- renderPlot({
    country_vac_plot(country_reactive_df()) ### EP: unchanged but now calls the function rather than the static plot
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
    fvdf
  })
  output$fullyvac_plot <- renderPlot({
    country_fully_vac_plot(reactive_fvdf()) 
  })
}  
shinyApp(ui = ui, server = server)
