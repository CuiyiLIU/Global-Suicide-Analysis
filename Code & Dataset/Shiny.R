library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse) # general
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries
library(readxl)
library(DT)
library(highcharter)
library(viridis)


data <- read_excel("master.xlsx")

data_1 <- data %>% 
  select(-c(`HDI for year`, `suicides/100k pop`)) %>%
  rename(gdp_for_year = `gdp_for_year ($)`, 
         gdp_per_capita = `gdp_per_capita ($)`, 
         country_year = `country-year`) %>%
  as.data.frame()

data_1 <- data_1 %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))


data_1 <- data_1 %>%
  filter(year != 2016) %>% # I therefore exclude 2016 data
  select(-country_year)

minimum_years <- data_1 %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 6) %>%
  arrange(years)

data_1 <- data_1 %>%
  filter(!(country %in% head(minimum_years$country, 20)))

minimum_years_2 <- data_1 %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 6) %>%
  arrange(years)

data_1$age <- gsub(" years", "", data_1$age)
data_1$sex <- ifelse(data_1$sex == "male", "Male", "Female")

data_1$continent <- countrycode(sourcevar = data_1[, "country"],
                                origin = "country.name",
                                destination = "continent")

#Pick up color for continent.
continent_color <- c('#058DC7', '#50B432', '#ED561B', '#FFF263', '#6AF9C4','#FF9655')

# Reclassify countries that have been coded as 'Americas', by countrycode(), into 'North America' and 'South America'. 
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
data_1$continent[data_1$country %in% south_america] <- 'South America'
data_1$continent[data_1$continent=='Americas'] <- 'North America'


data_nominal <- c('country', 'sex', 'continent')
data_1[data_nominal] <- lapply(data_1[data_nominal], function(x){factor(x)})

data_1$age <- factor(data_1$age, 
                     ordered =TRUE, 
                     levels = c("5-14",
                                "15-24", 
                                "25-34", 
                                "35-54", 
                                "55-74", 
                                "75+"))


data_1 %>% group_by(generation) %>% summarize(rows=n())
data_1$generation <- factor(data_1$generation, 
                            ordered = TRUE, 
                            levels = c("G.I. Generation", 
                                       "Silent",
                                       "Boomers", 
                                       "Generation X", 
                                       "Millenials", 
                                       "Generation Z"))

data <- as_tibble(data_1)



# Create a custom theme for the plots. 
custom_theme <- hc_theme(
  colors = c('#5CACEE', 'green', 'red'),
  chart = list(
    backgroundColor = '#FAFAFA', 
    plotBorderColor = "black"),
  xAxis = list(
    gridLineColor = "E5E5E5", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#E5E5E5", 
    minorGridLineColor = "#E5E5E5", 
    tickColor = "#E5E5E5", 
    title = list(style = list(color = "#333333"))), 
  yAxis = list(
    gridLineColor = "#E5E5E5", 
    labels = list(style = list(color = "#333333")), 
    lineColor = "#E5E5E5", 
    minorGridLineColor = "#E5E5E5", 
    tickColor = "#E5E5E5", 
    tickWidth = 1, 
    title = list(style = list(color = "#333333"))),   
  title = list(style = list(color = '#333333', fontFamily = "Lato")),
  subtitle = list(style = list(color = '#666666', fontFamily = "Lato")),
  legend = list(
    itemStyle = list(color = "#333333"), 
    itemHoverStyle = list(color = "#FFF"), 
    itemHiddenStyle = list(color = "#606063")), 
  credits = list(style = list(color = "#666")),
  itemHoverStyle = list(color = 'gray'))

global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population)))*100000

country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

model1 <- lm(suicides_per_100k ~ gdp_per_capita, data = country_mean_gdp)

# Pick color for gender.
sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink

gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_gdp, by = c("suicides_per_100k", "gdp_per_capita")) %>%
  select(country, continent, gdp_per_capita, suicides_per_100k)

model2 <- lm(suicides_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)

country_total_population <- data %>%
  group_by(country, continent) %>%
  summarize(total_suicides = sum(as.numeric(suicides_no)), total_population= sum(as.numeric(population)))
country_total_population$total_suicides <- round(country_total_population$total_suicides, 2)
country_total_population$total_population <- round(country_total_population$total_population, 2)

model3 <- lm(total_suicides ~ total_population, data = country_total_population)

population_suicide_no_outliers <- model3 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% 
  inner_join(country_total_population, by = c('total_suicides', 'total_population')) %>%
  select(country, continent, total_population, total_suicides)

model4 <- lm(total_suicides ~ total_population, data = population_suicide_no_outliers)

### Suicides by country {.no-title .no-padding .colored }
# Create a tibble with suicide per capita by country for 1985-2015. 
country_tibble <- data %>%
  select(country, suicides_no, population) %>%
  group_by(country) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))


# Create tibble for our line plot.  
overall_tibble <- data %>%
  select(year, suicides_no, population) %>%
  group_by(year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 



ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'Suicides Analysis', titleWidth = 190),
                    dashboardSidebar(width = 190,
                                     sidebarMenu(
                                       menuItem("Data Overview", tabName = "overview", icon = icon("fas fa-globe")),
                                       menuItem("Country Trend", tabName = "countrySearch", icon = icon("search")),
                                       menuItem('Exploratory Analysis', icon = icon("dashboard"),
                                                menuSubItem("Worldwide", icon = icon("fas fa-chart-line"),tabName = "worldwideExp"),
                                                menuSubItem("Continent", icon = icon("fas fa-chart-line"), tabName = "continentExp"),
                                                menuSubItem("Country", icon = icon("fas fa-chart-line"),tabName = "countryExp")),
                                       menuItem('Inferential Analysis', icon = icon("dashboard"),
                                                menuSubItem('Confidence Interval', icon = icon("fas fa-chart-line"),tabName = 'ci'),
                                                menuSubItem("Linear Regression", icon = icon("fas fa-chart-line"), tabName = "liReg"))
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'overview',
                                fluidPage(
                                  fluidRow(
                                    column(4,
                                           selectInput('country',
                                                       'Country',
                                                       c('All',
                                                         unique(as.character(data$country)
                                                         )))),
                                    
                                    column(4,
                                           selectInput('year',
                                                       'Year',
                                                       c('All',unique(as.character(data$year)
                                                       )))),
                                    
                                    column(4,
                                           selectInput('age',
                                                       'Age',
                                                       c('All', unique(as.character(data$age)
                                                       ))))
                                    
                                  ),
                                  
                                  fluidRow(
                                    DT::dataTableOutput('table')
                                  ),
                                  fluidRow(
                                    downloadButton("downloadData", "Download")
                                  )
                                
                                )
                        ),
                        
                        tabItem(tabName = "countrySearch",
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(
                                      helpText("Global suicice number over time"),
                                      selectInput(
                                        "var1",
                                        "Select different variables",
                                        c("All", unique(as.character(data$country)))
                                      ),
                                      sliderInput("var2",
                                                  "Year over time",
                                                  min = min(data$year),max = max(data$year),value = c(min,max), step = 5)
                                    ),
                                    
                                    mainPanel(
                                        wellPanel( plotOutput("country_suicide"))
                                    )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "worldwideExp",
                                fluidPage(
                                  tabBox(
                                    width = 12,
                                    tabPanel("Worldwide", 
                                             highchartOutput("worldwideLinePlot")
                                             ),
                                    tabPanel("Worldwide By Gender", 
                                             fluidRow(
                                               column(
                                                 width = 5,
                                                 highchartOutput("genderPiePlot")
                                               ),
                                               column(
                                                 width = 7,
                                                 plotOutput("genderBarLinePlot")
                                               )
                                             )
                                    ),
                                    tabPanel("Worldwide By Age", 
                                             fluidRow(
                                               column(
                                                 width = 5,
                                                 highchartOutput("agePiePlot")
                                               ),
                                               column(
                                                 width = 7,
                                                 plotOutput("ageBarLinePlot")
                                               )
                                             )
                                          )
                                  )
                                )
                        ), 
                        tabItem(tabName = "continentExp",
                                fluidPage(
                                  fluidRow(
                                    tabBox(
                                      width = 5,
                                      tabPanel("By Continent and Gender", highchartOutput("continentAndGenderBarPlot")),
                                      tabPanel("By Continent and Age", highchartOutput("continentAndAgeBarPlot"))
                                    ),
                                    box(
                                      width = 7, 
                                      #highchartOutput("continentBarPlot")
                                      highchartOutput("continentMapPlot")
                                    )
                                  )
                                )
                        ),  
                        tabItem(tabName = "countryExp",
                                fluidPage(
                                  fluidRow(
                                    tabBox(
                                      width = 5,
                                      tabPanel("By Country", highchartOutput("countryBarPlot")),
                                      tabPanel("By Country and Gender", highchartOutput("countryAndGenderBarPlot")),
                                      tabPanel("By Country and Age", highchartOutput("countryAndAgeBarPlot"))
                                    ),
                                    box(
                                      width = 7, 
                                      highchartOutput("countryMapPlot")
                                    )
                                  )
                                )
                        ), 
                        tabItem(tabName = "ci",
                                fluidRow(
                                  box(
                                    width = 6.3,
                                    numericInput(inputId = "ciChoosed", "Input CI:", 0.95, min = 0, max = 1, step = 0.01), 
                                    radioButtons("analyseBy", "Analyse By:",
                                                      c("Generation" = "generation",
                                                        "Continent" = "continent"
                                                        ), inline = TRUE),
                                    h5('CI Summary Table'),
                                    tableOutput("ci_table"),
                                    height = 424
                                    ),
                                  box(
                                    width = 5,
                                    #verbatimTextOutput("text")
                                    plotOutput("ciBoxPlot")
                                    )
                                )

                        ),
                        tabItem(
                          tabName = "liReg",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Trend and correlation with suicide numbers"),
                                selectInput(
                                  "var3",
                                  "Select different variables",
                                  choices = c("Population"= names(population_suicide_no_outliers)[3], "GDP" = "gdp_per_capita" )
                                )
                              ),
                              mainPanel(
                                h5("The chart shows the relationship between the chosen demographic variable and suicide number"),
                                plotOutput("plot1"),
                                h5("The simple linear regression model (bivariate analysis) is calculated for each demographic variable and shown below:"),                                               
                                h4("Slope"), 
                                textOutput("pred1slope"),
                                textOutput("pred2slope"),
                                h4("Intercept"),
                                textOutput("pred1intercept"),
                                textOutput("pred2intercept"),
                                h5("If p value is < 0.05, we conclude that the relationship between the independent variable and sales value is statistically significant"),
                                h4("P value of regression model"),
                                textOutput("pred1p"),
                                textOutput("pred2p")
                              )
                            )
                          )
                        )
                      )
                    )

          
)



server <- function(input, output){
  output$table <- renderDataTable(DT::datatable({
    data <- data
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$age != "All") {
      data <- data[data$age == input$age,]
    }
    data
  }))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        data <- data
        if (input$country != "All") {
          data <- data[data$country == input$country,]
        }
        if (input$year != "All") {
          data <- data[data$year == input$year,]
        }
        if (input$age != "All") {
          data <- data[data$age == input$age,]
        }
        data
      write.csv(data, file)
    }
  
  )

  
  output$country_suicide<- renderPlot({
    data <- data
    data = filter(data, year >= input$var2[1] & year <= input$var2[2])
    country_suicide <- data %>% 
      group_by(year, country) %>%
      summarize(population = sum(population), 
                suicides_no = sum(suicides_no), 
                suicides_per_100k = (suicides_no / population) * 100000)
    if (input$var1 != "All"){
      country_suicide <- country_suicide[country_suicide$country == input$var1,]
      ggplot(data = country_suicide, aes(x = year, y = suicides_per_100k))+
        geom_line(col = "orange", size = 1.5) + 
        geom_point(col = "deepskyblue3", size = 2.5) + 
        theme_light() +
        #        geom_smooth() + 
        labs(title = "Country Suicides average(per 100k)",
             subtitle = "Trend over time",
             x = "Year", 
             y = "Suicides_no per 100k") +
        scale_x_continuous(breaks = seq(1985, 2016, 2)) + 
        scale_y_continuous(breaks = seq(10, 20))
    }
    else {
      data <- data
      data_gs = filter(data, year >= input$var2[1] & year <= input$var2[2])
      global_suicide <- data_gs %>% 
        group_by(year) %>%
        summarize(population = sum(population), 
                  suicides_no = sum(suicides_no), 
                  suicides_per_100k = (suicides_no / population) * 100000)
      
      ggplot(data = global_suicide, aes(x = global_suicide$year, y = global_suicide$suicides_per_100k)) + 
        geom_line(col = "orange", size = 1.5) + 
        geom_point(col = "deepskyblue3", size = 2.5) + 
        theme_light() + 
        #        geom_smooth() + 
        geom_hline(yintercept = global_average, linetype = 2, color = "black", size = 1.5) +
        labs(title = "Global Suicides average(per 100k)",
             subtitle = "Trend over time",
             x = "Year", 
             y = "Suicides_no per 100k") + 
        scale_x_continuous(breaks = seq(1985, 2016, 2)) + 
        scale_y_continuous(breaks = seq(10, 20)) 
      
    }
  })
  
  #worldwide
  output$worldwideLinePlot <- renderHighchart({
    # Create tibble for our line plot.  
    overall_tibble <- data %>%
      select(year, suicides_no, population) %>%
      group_by(year) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
    
    # Create a line plot.
    highchart() %>% 
      hc_add_series(overall_tibble, hcaes(x = year, y = suicide_capita, color = suicide_capita), type = "line") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_title(text = "Worldwide suicides by year") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Suicides per 100K people"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "black", width = 1, dashStyle = "Dash", 
                 value = mean(overall_tibble$suicide_capita),
                 label = list(text = "Mean = 13.12", 
                              style = list(color = "black", fontSize = 11))))) %>%
      hc_legend(enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
 
  
  #Gender
  output$genderBarLinePlot <- renderPlot({
    sex_plot <- data %>%
      group_by(sex) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides (per 100k), by Sex",
           x = "Sex", 
           y = "Suicides per 100k") +
      theme(legend.position = "none") + 
      scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)
    
    sex_time_plot <- data %>% group_by(year, sex) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
      facet_grid(sex ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Sex", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Sex") + 
      theme(legend.position = "none") + 
      scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
    
    grid.arrange(sex_plot, sex_time_plot, ncol = 2)
  })
   
  ## Suicides by Age ##  
  output$ageBarLinePlot <- renderPlot({
    # Pick color for age. 
    age_color <- rev(plasma(6))
    
    age_plot <- data %>%
      group_by(age) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides per 100k, by Age",
           x = "Age", 
           y = "Suicides per 100k") +
      theme(legend.position = "none") + 
      scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

    ### by age with time (seperated)
    age_time_plot1 <- data %>%
      group_by(year, age) %>%
      summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
      ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
      facet_grid(age ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Age", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Age") + 
      theme(legend.position = "none") + 
      scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
    
    grid.arrange(age_plot, age_time_plot1, ncol = 2)
    
  })
  
  
  
  # Worldwide By Gender - Pie
  output$genderPiePlot <- renderHighchart({
    # First, create a tibble of suicide by Age. We will use this for our pie chart.
    # Create pie chart for sex. 
    pie_sex <- data %>% 
      group_by(sex) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    #color = c("#EE6AA7", "#87CEEB")
    highchart() %>% 
      hc_add_series(pie_sex, hcaes(x = sex, y = suicide_capita, color = c("#EE6AA7", "#87CEEB")), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_title(text = "<b>Suicides by Gender</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015", style = (list(fontSize = '10px'))) %>%
      hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                                  style = list(fontSize = 10)), 
                                size = 250))  %>% 
      hc_add_theme(custom_theme)
  })
  
 #Continent Bar plot
  output$continentBarPlot <- renderHighchart({
    continent_tibble <- data %>%
      select(continent, sex, suicides_no, population) %>%
      group_by(continent) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    highchart() %>%
      hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "column")  %>% 
      hc_title(text = "Suicides by Continent", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K people",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
      hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
    
  })
  
  
  #Continent map plot
  output$continentMapPlot <- renderHighchart({
    
    map_data <- download_map_data("custom/world-continents")
    
    # Create a tibble for continent.
    continent_tibble <- data %>%
      select(continent, suicides_no, population) %>%
      group_by(continent) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(suicide_capita)
    
    highchart() %>%
      hc_add_series_map(map_data, continent_tibble, value = "suicide_capita", joinBy = c('name','continent'), name = "Suicides (per 100K people)")  %>% 
      hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "pie", name = 'Suicides (per 100K people)')  %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "Suicides by Continent") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, valueSuffix = '') %>%
      hc_plotOptions(
        pie = list(center = c('10%', '80%'), size = 110, dataLabels = list(enabled = FALSE))) %>% 
      hc_add_theme(custom_theme)

  })
  
  output$agePiePlot <- renderHighchart({
    
    # Pick color for age. 
    age_color <- rev(plasma(6))
  
    pie_age <- data %>%
      select(age, suicides_no, population) %>%
      group_by(age) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(suicide_capita)
    
    # Create pie chart for Age. 
    highchart() %>% 
      hc_add_series(pie_age, hcaes(x = age, y = suicide_capita, 
                                   color = rev(plasma(6))), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%  
      hc_title(text = "<b>Worldwide suicides by Age</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015", style = (list(fontSize = '10px'))) %>%
      hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                                  style = list(fontSize = 10)), 
                                size = 250)) %>% 
      hc_add_theme(custom_theme)
  })
  
  
  
  #Worldwide by age - Bar
  output$ageBarPlot <- renderHighchart({
    # Pick color for age. 
    age_color <- rev(plasma(6))
    
    # Create tibble for age.  
    global_age_tibble <- data %>%
      select(age, suicides_no, population) %>%
      group_by(age) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    # Create bar chart of suicides by age.
    highchart() %>%
      hc_add_series(global_age_tibble, hcaes(x = age, y = suicide_capita, color = age_color), type = "column")  %>% 
      hc_title(text = "Global Suicides by Age", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K people",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
      hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })
 
  output$countryBarPlot <- renderHighchart({
    country_bar <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
      arrange(desc(suicide_capita))
    
    # Create interactive bar plot.
    highchart() %>%
      hc_add_series(country_bar, hcaes(x = country, y = suicide_capita, color = suicide_capita), type = "bar")  %>% 
      hc_tooltip(borderWidth = 1.5, 
                 pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = "Suicides by country") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_xAxis(categories = country_bar$country, 
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Suicides per 100K people")) %>%
      hc_plotOptions(bar = list(stacking = "normal", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
      hc_add_theme(custom_theme) 
  })
  
  # Continent and sex.
  output$continentAndGenderBarPlot <- renderHighchart({
    continent_sex_tibble <- data %>%
      select(continent, sex, suicides_no, population) %>%
      group_by(continent, sex) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    # Create histogram of suicides by continent and sex.
    highchart() %>%
      hc_add_series(continent_sex_tibble, hcaes(x = continent, y = suicide_capita, group = sex), type = "column")  %>% 
      hc_colors(colors = sex_color) %>%
      hc_title(text = "Suicides by continent and <b>Gender</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b> {point.sex} </b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K people",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
      hc_legend(verticalAlign = 'top', enabled = FALSE)%>% 
      hc_add_theme(custom_theme)
    
  })
  
  # Create a tibble for continent and age.
  output$continentAndAgeBarPlot <- renderHighchart({
    
    # Pick color for age. 
    age_color <- rev(plasma(6))
    
    continent_age_tibble <- data %>%
      select(continent, age, suicides_no, population) %>%
      group_by(continent, age) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    # Create histogram of suicides by continent and age.
    highchart() %>%
      hc_add_series(continent_age_tibble, hcaes(x = continent, y = suicide_capita, group = age), type = "column")  %>% 
      hc_colors(colors = age_color) %>%
      hc_title(text = "Suicides by continent and <b>Age</b>", style = (list(fontSize = '14px'))) %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b> {point.age} </b> <br> Suicides: <b>{point.y}</b>")) %>%
      hc_xAxis(categories = c("Africa", "Asia", "Europe", "North <br> America", "Oceania", "South <br> America"), labels = list(style = list(fontSize = 8))) %>%
      hc_yAxis(labels = list(style = list(fontSize = 10)),
               title = list(text = "Suicides per 100K people",
                            style = list(fontSize = 10)),
               plotLines = list(
                 list(color = "black", width = 1, dashStyle = "Dash", 
                      value = mean(overall_tibble$suicide_capita),
                      label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%    
      hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
      hc_add_theme(custom_theme)
  })

  # Country and sex.
  output$countryAndGenderBarPlot <- renderHighchart({
    # Create tibble for suicide by countries and sex. 
    country_bar_sex <- data %>%
      select(country, sex, suicides_no, population) %>%
      group_by(country, sex) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    country_tibble <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 
    
    # Create interactive bar plot.
    highchart() %>%
      hc_add_series(country_bar_sex, hcaes(x = country, y = suicide_capita, group = sex), type = "bar", color = sex_color)  %>% 
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_legend(enabled = TRUE, colorByPoint = TRUE) %>%
      hc_title(text = "Suicides by country and gender") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_xAxis(categories = country_tibble$country,
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Percentage of total suicides")) %>%
      hc_plotOptions(bar = list(stacking = "percent", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.4)) %>% 
      hc_add_theme(custom_theme)
  })

  #Country and Age.
  output$countryAndAgeBarPlot <- renderHighchart({
    # Pick color for age. 
    age_color <- rev(plasma(6))
    
    # Create tibble for suicide by countries and age.
    country_bar_age <- data %>%
      select(country, age, suicides_no, population) %>%
      group_by(country, age) %>%
      summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    # Create interactive bar plot.
    highchart() %>%
      hc_add_series(country_bar_age, hcaes(x = country, y = suicide_capita, group = age), type = "bar", color = age_color)  %>% 
      hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
      hc_title(text = "Suicides by country and age") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_xAxis(categories = country_tibble$country,
               labels = list(step = 1),
               min = 0, max = 25,
               scrollbar = list(enabled = TRUE)) %>%
      hc_yAxis(title = list(text = "Percent of total suicides")) %>%
      hc_plotOptions(bar = list(stacking = "percent", 
                                pointPadding = 0, groupPadding = 0, borderWidth = 0.5)) %>% 
      hc_add_theme(custom_theme)
  })
  output$countryMapPlot <- renderHighchart({
    country_tibble <- data %>%
      select(country, suicides_no, population) %>%
      group_by(country) %>%
      summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))
    
    # Create interactive world map.
    highchart() %>%
      hc_add_series_map(worldgeojson, country_tibble, value = "suicide_capita", joinBy = c('name','country'))  %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "Suicides by Country") %>% 
      hc_subtitle(text = "1985-2015") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "", valueSuffix = " suicides (per 100K people)") %>% 
      hc_add_theme(custom_theme)
  })

   output$plot1 <- renderPlot({
    if(input$var3=="gdp_per_capita"){
      ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicides_per_100k, col = continent)) + 
        geom_point() + 
        geom_smooth(method = "lm", aes(group = 1),size=1.25, alpha = 0.2) + 
        scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
        labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
             #             subtitle = "Plot with high CooksD countries",
             x = "GDP (per capita)", 
             y = "Suicides_no per 100k", 
             col = "Continent") + 
        theme_light()}
    else{
      ggplot(population_suicide_no_outliers, aes(x = total_population, y = total_suicides, col = continent)) +
        geom_point() +
        geom_smooth(method = 'lm', aes(group = 1)) +
        # scale_x_continuous(labels=scales::dollar_format(prefix="$"),
        #                    breaks = seq(0, 24000000, 4000000)) +
        labs(title = 'Correlation between Total population and Total suicides', x = "total_population", 
             y = "total_suicides", 
             col = "Continent") +
        theme_light()
    }
  })
  
  output$pred1p <- renderText({if(input$var3=='total_population'){anova(model2)$'Pr(>F)'[1]}})
  output$pred2p <- renderText({if(input$var3=='gdp_per_capita'){anova(model4)$'Pr(>F)'[1]}})
  
  output$pred1slope <- renderText({if(input$var3=='total_population'){model2[[1]][2]}})
  output$pred2slope <- renderText({if(input$var3=='gdp_per_capita'){model4[[1]][2]}})
  
  output$pred1intercept <- renderText({if(input$var3=='total_population'){model2[[1]][1]}})
  output$pred2intercept <- renderText({if(input$var3=='gdp_per_capita'){model4[[1]][1]}})
  
  
  #CI
  output$ci_table <- renderTable({
    confidence_level = input$ciChoosed
    data$suicides_per_100k = round((data$suicides_no / data$population) * 100000, 2)
    
    if (input$analyseBy == "continent") {
      analyseData <- data %>%
        select(continent, suicides_per_100k) %>%
        group_by(continent) %>%
        summarize(mean_suicides_per_100k = mean(suicides_per_100k), sd_suicides_per_100k = sd(suicides_per_100k), 
                  lower = mean(suicides_per_100k)-qnorm((1-confidence_level)/2+confidence_level)*sd(suicides_per_100k)/sqrt(nrow(data)),
                  upper = mean(suicides_per_100k)+qnorm((1-confidence_level)/2+confidence_level)*sd(suicides_per_100k)/sqrt(nrow(data)))
      
    }
    else if (input$analyseBy == "generation") {
      analyseData <- data %>%
        select(generation, suicides_per_100k) %>%
        group_by(generation) %>%
        summarize(mean_suicides = mean(suicides_per_100k), sd_suicides = sd(suicides_per_100k), 
                  lower = mean_suicides-qnorm((1-confidence_level)/2+confidence_level)*sd_suicides/sqrt(nrow(data)),
                  upper = mean_suicides+qnorm((1-confidence_level)/2+confidence_level)*sd_suicides/sqrt(nrow(data)))
        
    }
    analyseData
  })
  
  output$text <- renderText(
    qnorm((1-input$ciChoosed)/2+input$ciChoosed)
  )
  
  output$ciBoxPlot <- renderPlot({
    confidence_level = input$ciChoosed
    data$suicides_per_100k = round((data$suicides_no / data$population) * 100000, 2)
    
    if (input$analyseBy == "continent") {
    analyseData <- data %>%
          select(continent, suicides_per_100k) %>%
          group_by(continent) %>%
          summarize(mean_suicides_per_100k = mean(suicides_per_100k), sd_suicides_per_100k = sd(suicides_per_100k), 
                  lower = mean(suicides_per_100k)-qnorm((1-confidence_level)/2+confidence_level)*sd(suicides_per_100k)/sqrt(nrow(data)),
                  upper = mean(suicides_per_100k)+qnorm((1-confidence_level)/2+confidence_level)*sd(suicides_per_100k)/sqrt(nrow(data)))
    analyseData$continent <- factor(analyseData$continent)
        
    ggplot(analyseData, aes(x = continent, y = mean_suicides_per_100k)) + 
      geom_bar(position=position_dodge(), stat="identity", fill = "#5EB1B3") +
      geom_errorbar(aes(ymin=lower, ymax=upper),
                    width=.5,
                    position=position_dodge(),
                    color = "red") +
      xlab("Continent") + 
      ylab("Mean_Suicides")
    }
    else if (input$analyseBy == "generation") {
      analyseData <- data %>%
        select(generation, suicides_per_100k) %>%
        group_by(generation) %>%
        summarize(mean_suicides = mean(suicides_per_100k), sd_suicides = sd(suicides_per_100k), 
                  lower = mean_suicides-qnorm((1-confidence_level)/2+confidence_level)*sd_suicides/sqrt(nrow(data)),
                  upper = mean_suicides+qnorm((1-confidence_level)/2+confidence_level)*sd_suicides/sqrt(nrow(data)))
      
      ggplot(analyseData, aes(x = generation, y = mean_suicides)) + 
        geom_bar(position=position_dodge(), stat="identity", fill = "#5EB1B3") +
        geom_errorbar(aes(ymin=lower, ymax=upper),
                      width=.5,
                      position=position_dodge(),
                      color = "red") +
        xlab("Generation") + 
        ylab("Mean_Suicides")
    }

  })
  
}

shinyApp(ui = ui, server = server)          