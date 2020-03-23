library(ggplot2)
library(ggpubr)
library(dplyr)
library(corrplot)
library(broom)
library(flexdashboard) # Dashboard package
library(highcharter) # Interactive data visualizations
library(plotly) # Interactive data visualizations
library(viridis) # Color gradients
library(tidyverse) # Metapackge
library(countrycode) # Converts country names/codes
library(rjson) # JSON reader
library(crosstalk) # Provides interactivity for HTML widgets
library(DT) # Displaying data tables



## DATA IMPORT AND CLEANING ##
data <- read.csv("master.csv")# it must be imported as csv format
#glimpse(data) # will tidy up t

data <- data %>% 
  select(-c(`HDI.for.year`, `suicides.100k.pop`)) %>%
  as.data.frame()

colnames(data) = c("country","year","sex","age","suicides_no","population","country_year","gdp_for_year","gdp_per_capita","generation")

data <- data %>%
  filter(year != 2016, # filter out 2016 and countries with 0 data. 
         country != 'Dominica',
         country != 'Saint Kitts and Nevis')

# Fix the names of some of the countries in our data to match the country names 
# used by our map later on so that they'll be interpreted and displayed. 
data <- data %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))

# Reorder levels of age to be in chronological order. 
data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

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

#Worldwide
  
### Worldwide suicides {.no-padding}

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

### Worldwide suicides by Gender {.no-padding}

# Create tibble for sex and year. 
sex_tibble <- data %>%
  select(year, sex, suicides_no, population) %>%
  group_by(year, sex) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Pick color for gender.
sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink

# Create line plot
highchart() %>% 
  hc_add_series(sex_tibble, hcaes(x = year, y = suicide_capita, group = sex), type = "line", color = sex_color) %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Gender: <b>{point.sex}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Gender") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash",
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = 'black', fontSize = 11))))) %>% 
  hc_add_theme(custom_theme)

# Create tibble for sex   
global_sex_tibble <- data %>%
  select(sex, suicides_no, population) %>%
  group_by(sex) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

# Create bar chart of suicides by sex.
highchart() %>%
  hc_add_series(global_sex_tibble, hcaes(x = sex, y = suicide_capita, color = sex_color), type = "column")  %>% 
  hc_title(text = "Global Suicides by Sex", style = (list(fontSize = '14px'))) %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
  hc_xAxis(categories = c("female","male"), labels = list(style = list(fontSize = 8))) %>%
  hc_yAxis(labels = list(style = list(fontSize = 10)),
           title = list(text = "Suicides per 100K people",
                        style = list(fontSize = 10)),
           plotLines = list(
             list(color = "black", width = 1, dashStyle = "Dash", 
                  value = mean(overall_tibble$suicide_capita),
                  label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
  hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
  hc_add_theme(custom_theme)

# Create tibble for age and year.  
age_tibble <- data %>%
  select(year, age, suicides_no, population) %>%
  group_by(year, age) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Pick color for age. 
age_color <- rev(plasma(6))

# Create a line plot.
highchart() %>% 
  hc_add_series(age_tibble, hcaes(x = year, y = suicide_capita, group = age), type = "line", color = age_color) %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Age") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash",
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = 'black', fontSize = 11))))) %>% 
  hc_add_theme(custom_theme)

# Create tibble for age.  
global_age_tibble <- data %>%
  select(age, suicides_no, population) %>%
  group_by(age) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

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

### Suicides per 100K <font size='1'> (1985-2015) </font>
# Grab worldwide number of suicides per 100K people from the data
total_suicides <- round(mean(overall_tibble$suicide_capita), 2)

# Create value box
valueBox(total_suicides, icon = "fa-plus", color = 'firebrick')

### Worldwide suicides by Gender {.no-title .no-padding .colored }
# First, make a tibble of suicide by sex. We will use this for our pie chart.
pie_sex <- data %>%
  select(sex, suicides_no, population) %>%
  group_by(sex) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2))

# Create pie chart for sex. 
highchart() %>% 
  hc_add_series(pie_sex, hcaes(x = sex, y = suicide_capita, 
                               color = sex_color), type = "pie") %>%
  hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Gender: <b>{point.sex} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_title(text = "<b>Worldwide suicides by Gender</b>", style = (list(fontSize = '14px'))) %>% 
  hc_subtitle(text = "1985-2015", style = (list(fontSize = '10px'))) %>%
  hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                              style = list(fontSize = 10)), 
                            size = 130)) %>% 
  hc_add_theme(custom_theme)

### Worldwide suicides by Age {.no-title .no-padding .colored }
# First, create a tibble of suicide by Age. We will use this for our pie chart.
pie_age <- data %>%
  select(age, suicides_no, population) %>%
  group_by(age) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

# Create pie chart for Age. 
highchart() %>% 
  hc_add_series(pie_age, hcaes(x = age, y = suicide_capita, 
                               color = age_color), type = "pie") %>%
  hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%  
  hc_title(text = "<b>Worldwide suicides by Age</b>", style = (list(fontSize = '14px'))) %>% 
  hc_subtitle(text = "1985-2015", style = (list(fontSize = '10px'))) %>%
  hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                              style = list(fontSize = 10)), 
                            size = 130)) %>% 
  hc_add_theme(custom_theme)

#Continents
# Create new column in our data for continent. Use countrycode() to extract continents from country names. 
data$continent <- countrycode(sourcevar = data$country,
                              origin = "country.name",
                              destination = "continent")
#Pick up color for continent.
continent_color <- c('#058DC7', '#50B432', '#ED561B', '#FFF263', '#6AF9C4','#FF9655')

# Reclassify countries that have been coded as 'Americas', by countrycode(), into 'North America' and 'South America'. 
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
data$continent[data$country %in% south_america] <- 'South America'
data$continent[data$continent=='Americas'] <- 'North America'

### Suicides by continent
# Create a tibble for continent.
continent_tibble <- data %>%
  select(continent, sex, suicides_no, population) %>%
  group_by(continent) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

# Create histogram of suicides by continent.
highchart() %>%
  hc_add_series(continent_tibble, hcaes(x = continent, y = suicide_capita, color = suicide_capita), type = "column")  %>% 
  hc_title(text = "Suicides by Continent", style = (list(fontSize = '14px'))) %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Suicides: <b>{point.y}</b>")) %>%
  hc_xAxis(categories = c("Africa", "South <br> America", "North <br> America", "Oceania", "Asia", "Europe"), labels = list(style = list(fontSize = 8))) %>%
  hc_yAxis(labels = list(style = list(fontSize = 10)),
           title = list(text = "Suicides per 100K people",
                        style = list(fontSize = 10)),
           plotLines = list(
             list(color = "black", width = 1, dashStyle = "Dash", 
                  value = mean(overall_tibble$suicide_capita),
                  label = list(text = "Mean = 13.12", style = list(color = "black", fontSize = 6))))) %>%     
  hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
  hc_add_theme(custom_theme)

# Create a tibble of suicide by continent for pie chart.
pie_continent <- data %>%
  select(continent, suicides_no, population) %>%
  group_by(continent) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

# Create pie chart of suicides by continent
highchart() %>% 
  hc_add_series(pie_continent, hcaes(x = continent, y = suicide_capita, 
                               color = continent_color), type = "pie") %>%
  hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Continent: <b>{point.continent} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%  
  hc_title(text = "<b>Worldwide suicides by Continent</b>", style = (list(fontSize = '14px'))) %>% 
  hc_subtitle(text = "1985-2015", style = (list(fontSize = '10px'))) %>%
  hc_plotOptions(pie = list(dataLabels = list(distance = 5, 
                                              style = list(fontSize = 10)), 
                            size = 130)) %>% 
  hc_add_theme(custom_theme)

# Create a tibble for continent and year.
continent_year_tibble <- data %>%
  select(continent, year, suicides_no, population) %>%
  group_by(continent, year) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 

# Create line chart of suicides by continent and year.
highchart() %>% 
  hc_add_series(continent_year_tibble, hcaes(x = year, y = suicide_capita, group=continent), type = "line", color = continent_color) %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Continent: <b>{point.continent}</b><br>", "Total Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Continent") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash",
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = 'black', fontSize = 11))))) %>% 
  hc_add_theme(custom_theme)

### Suicides by continent and Gender {.no-title .no-padding .colored }
# Create a tibble for continent and sex.
continent_sex_tibble <- data %>%
  select(continent, sex, suicides_no, population) %>%
  group_by(continent, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

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
  hc_legend(verticalAlign = 'top', enabled = FALSE) %>% 
  hc_add_theme(custom_theme)

### Suicides by continent and Age {.no-title .no-padding .colored }
# Create a tibble for continent and sex.
continent_age_tibble <- data %>%
  select(continent, age, suicides_no, population) %>%
  group_by(continent, age) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

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

### Suicides by continent {.no-title .no-padding .colored }
# Import continent map data.
map_data <- download_map_data("custom/world-continents")

# Create a tibble for continent.
continent_tibble <- data %>%
  select(continent, suicides_no, population) %>%
  group_by(continent) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
  arrange(suicide_capita)

# Create continent map with suicide data. 
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

#Countries
### By country {.no-padding}
# Create tibble for overall suicides by country.
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
  
### By gender {.no-padding}
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

### By age {.no-padding}
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

### Suicides by country {.no-title .no-padding .colored }
# Create a tibble with suicide per capita by country for 1985-2015. 
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

# Search
### Filters {.no-title .colored }
# Create tibble for our line plot.  
country_year_tibble <- data %>%
  select(country, year, suicides_no, population) %>%
  group_by(country, year) %>%
  summarise(suicide_capita = round((sum(suicides_no)/sum(population))*100000, 2)) 

# Create shared data that will be used to link filters, data table, and line plot. 
shared_data <- SharedData$new(country_year_tibble, group = 'hello')

# Create filter for year and country. These filters will adjust the DT datatable and PLOTLY plot. 
filter_slider("year", "Year", shared_data, ~year, step = 1)
filter_select("country", "Country", shared_data, ~country, allLevels = TRUE, multiple = TRUE)

# Create datatable. 
datatable(shared_data,
          rownames = FALSE,
          colnames = c('Country', 'Year', 'Suicides /100K'),
          class = 'cell-border stripe',
          extensions = "Scroller",
          options=list(deferRender = FALSE, 
                       scrollY = 200, 
                       scrollCollapse = TRUE,
                       scroller = TRUE,
                       dom = 't'))

### Suicides by country {.no-title .colored }
# Set a random seed. We will do this so that we can reproduce the random sample of colors we will use for our next graph. 
set.seed(80085)

# Create line graph. 
plot_ly(shared_data, x = ~year, y = ~suicide_capita, 
        color = ~country, colors = sample(colours(), 120),
        type = 'scatter', mode = 'lines',
        hoverinfo = 'text', text = ~paste("Country: ", country, '<br>Year: ', year, "<br>Suicides: ", suicide_capita)) %>%
  layout(showlegend = FALSE,
         title = "Suicide by country",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Suicides per 100K people")) %>%
  layout(plot_bgcolor = 'transparent') %>% 
  layout(paper_bgcolor = 'transparent') %>% 
  add_markers() %>% 
  highlight("plotly_click")

# Correlations
###Suicides number vs. GDP per capita

# Create tipple for country's mean GDP and suicides number per 100K.
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = sum(as.numeric(suicides_no)) / sum(as.numeric(population)) * 100000, gdp_per_capita= (mean(gdp_per_capita)))
country_mean_gdp$suicide_per_100k <- round(country_mean_gdp$suicide_per_100k, 2)
country_mean_gdp$gdp_per_capita <- round(country_mean_gdp$gdp_per_capita, 2)

# Plot scatter chart
highchart() %>% 
  hc_add_series(country_mean_gdp, hcaes(x = gdp_per_capita, y = suicide_per_100k, group=continent), type = "scatter", color = continent_color) %>%
  hc_tooltip(pointFormat = paste("Continent: <b>{point.continent}</b><br>","Country: <b>{point.country}</b><br>","GDP per capita: <b>{point.x}</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_title(text = "Suicides number per 100K vs. GDP per capita") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "GDP per capita")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash", 
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = "black", fontSize = 11))))) %>%
  hc_add_theme(custom_theme)

# Build the linear regression model.
# Remove outliers.
model1 <- lm(suicide_per_100k ~ gdp_per_capita, data = country_mean_gdp)
gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% 
  inner_join(country_mean_gdp, by = c('suicide_per_100k', 'gdp_per_capita')) %>%
  select(country, continent, gdp_per_capita, suicide_per_100k)
model2 <- lm(suicide_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)
summary(model2)

# Plot chart for linear regression model.
ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = 1)) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"),
  breaks = seq(0, 70000, 10000)) +
  labs(title = 'Correlation between GDP (per capita) and Suicides per 100k', theme(legend.position = 'none')) +
  theme_bw()

hchart(gdp_suicide_no_outliers, "scatter", hcaes(x = gdp_per_capita, y = suicide_per_100k), regression = TRUE) %>% 
  hc_colors(c("#2ecc71", "#d35400")) %>% 
  hc_add_dependency("plugins/highcharts-regression.js") %>%
  hc_tooltip(pointFormat = paste("Continent: <b>{point.continent}</b><br>","Country: <b>{point.country}</b><br>","GDP per capita: <b>{point.x}</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_title(text = "Suicides number per 100K vs. GDP per capita") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "GDP per capita")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash", 
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = "black", fontSize = 11))))) %>%
  hc_add_theme(custom_theme)

### Suicides number vs. Population
# Create tipple for country's total population and total suicides number.
country_total_population <- data %>%
  group_by(country, continent) %>%
  summarize(total_suicides = sum(as.numeric(suicides_no)), total_population= sum(as.numeric(population)))
country_total_population$total_suicides <- round(country_total_population$total_suicides, 2)
country_total_population$total_population <- round(country_total_population$total_population, 2)

# Plot scatter chart.
highchart() %>% 
  hc_add_series(country_total_population, hcaes(x = total_population, y = total_suicides, group=continent), type = "scatter", color = continent_color) %>%
  hc_tooltip(pointFormat = paste("Continent: <b>{point.continent}</b><br>","Country: <b>{point.country}</b><br>","Total population: <b>{point.x}</b> <br> Total suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Total suicides number vs. Total population") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Total population")) %>%
  hc_yAxis(title = list(text = "Total suicides"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash", 
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = "black", fontSize = 11))))) %>%
  hc_add_theme(custom_theme)

# Build the linear regression model.
# Remove outliers.
model3 <- lm(total_suicides ~ total_population, data = country_total_population)
population_suicide_no_outliers <- model3 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% 
  inner_join(country_total_population, by = c('total_suicides', 'total_population')) %>%
  select(country, continent, total_population, total_suicides)
model4 <- lm(total_suicides ~ total_population, data = population_suicide_no_outliers)
summary(model4)

# Plot chart for linear regression model.
ggplot(population_suicide_no_outliers, aes(x = total_population, y = total_suicides, col = continent)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = 1)) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"),
                     breaks = seq(0, 70000, 10000)) +
  labs(title = 'Correlation between Total population and Total suicides', theme(legend.position = 'none')) +
  theme_bw()

hchart(population_suicide_no_outliers, "scatter", hcaes(x = total_population, y = total_suicides), regression = TRUE) %>% 
  hc_colors(c("#2ecc71", "#d35400")) %>% 
  hc_add_dependency("plugins/highcharts-regression.js") %>%
  hc_tooltip(pointFormat = paste("Continent: <b>{point.continent}</b><br>","Country: <b>{point.country}</b><br>","Total population: <b>{point.x}</b> <br> Total suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Total suicides vs. Total population") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Total population")) %>%
  hc_yAxis(title = list(text = "Total Suicides"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "black", width = 1, dashStyle = "Dash", 
             value = mean(overall_tibble$suicide_capita),
             label = list(text = "Mean = 13.12", 
                          style = list(color = "black", fontSize = 11))))) %>%
  hc_add_theme(custom_theme)

# Hypothesis test
### Suicides number vs. year
cor(data$year, data$suicides_no, method = 'pearson')
cor(data$year, data$suicides_no, method = 'spearman')
cor(data$year, data$suicides_no, method = 'kendall') #Strong correlation due to the approximate corr coefficients.

# Implement the Wilcox test (H0: More recent year, ,more suicides).
wilcox.test(data$year, data$suicides_no,alternative = "two.sided", exact = FALSE, correct = FALSE)  #P-value is less than 5%, so reject H0.

# Implement the T test
t.test(data$year, data$suicides_no, alternative = "two.sided", mu=0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

### Suicides number vs. gender
cor(data$gdp_per_capita, data$suicides_no, method = 'pearson')
cor(data$gdp_per_capita, data$suicides_no, method = 'spearman')
cor(data$gdp_per_capita, data$suicides_no, method = 'kendall') #Strong correlation due to the approximate coefficients.

# Implement the Wilcox test (H0: More recent year, ,more suicides).
wilcox.test(data$gdp_per_capita, data$suicides_no,alternative = "two.sided", exact = FALSE, correct = FALSE)  #P-value is less than 5%, so reject H0.

# Implement the T test
t.test(data$year, data$suicides_no, alternative = "two.sided", mu=0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# ANOVA
### Continents
# Create a tibble for continent's mean suicides_per 100K.
data$suicides_per_100k = round((data$suicides_no / data$population) * 100000, 2)
mean_suicides_per_100k <- data %>%
  select(continent, suicides_per_100k) %>%
  group_by(continent) %>%
  summarize(mean_suicides_per_100k = mean(suicides_per_100k), sd_suicides_per_100k = sd(suicides_per_100k))

# Creat boxplot for continent.
ggboxplot(data, x = "continent", y = "suicides_per_100k", 
          color = "continent",
           ylab = "Suicides per 100k", xlab = "Continent")


# Implement ANOVA.
res.aov <- aov(suicides_per_100k ~ continent, data = data)
summary(res.aov)
TukeyHSD(res.aov)

### Gender
# Creat boxplot for continent.
ggboxplot(data, x = "sex", y = "suicides_per_100k", 
          color = sex_color,
          ylab = "Suicides per 100k", xlab = "Sex")


# Implement ANOVA.
res.aov <- aov(suicides_per_100k ~ sex, data = data)
summary(res.aov)
TukeyHSD(res.aov)

### Age
# Creat boxplot for age.
ggboxplot(data, x = "age", y = "suicides_per_100k", 
          color = age_color,
          ylab = "Suicides per 100k", xlab = "Age")

# Implement ANOVA.
res.aov <- aov(suicides_per_100k ~ age, data = data)
summary(res.aov)
TukeyHSD(res.aov)

### Generation
# Creat boxplot for age.
ggboxplot(data, x = "generation", y = "suicides_per_100k", 
          color = continent_color,
          ylab = "Suicides per 100k", xlab = "Age")

# Implement ANOVA.
res.aov <- aov(suicides_per_100k ~ generation, data = data)
summary(res.aov)
TukeyHSD(res.aov)

### Graded GDP
# Group data by graded GDP
y <- quantile(data$gdp_per_capita, c(0.8, 0.6, 0.4, 0.2))
data$gdp_per_capita_grade[data$gdp_per_capita >= y[1]] <- "very high" 
data$gdp_per_capita_grade[data$gdp_per_capita < y[1]] <- "high" 
data$gdp_per_capita_grade[data$gdp_per_capita < y[2]] <- "moderate" 
data$gdp_per_capita_grade[data$gdp_per_capita < y[3]] <- "low" 
data$gdp_per_capita_grade[data$gdp_per_capita < y[4]] <- "very low" 

#Pick up color for graded GDP.
graded_gdp_color <- c('#058DC7', '#50B432', '#ED561B', '#FFF263', '#6AF9C4')

# Creat boxplot for graded GDP.
ggboxplot(data, x = "gdp_per_capita_grade", y = "suicides_per_100k", 
          color = graded_gdp_color,
          ylab = "Suicides per 100k", xlab = "graded gdp")

# Implement ANOVA.
res.aov <- aov(suicides_per_100k ~ gdp_per_capita_grade, data = data)
summary(res.aov)
TukeyHSD(res.aov)




#==============================================================================================================================

# Multivariate Linear Regression Model #

## load packages
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(corrplot)
library(broom)
library(gridExtra)
library(lm.beta)
library(MASS)
library(tidyverse)

## Import Data

data2 <- read_excel("master.xlsx")# it must be imported as excel format 

## Data Preparation

data2 <- data2 %>%
  filter(year != 2016, # filter out 2016 and countries with 0 data. 
         country != 'Dominica',
         country != 'Saint Kitts and Nevis')

### Fix the names of some of the countries in our data to match the country names 
### used by our map later on so that they'll be interpreted and displayed. 
data2 <- data2 %>%
  mutate(country = fct_recode(country, "The Bahamas" = "Bahamas"),
         country = fct_recode(country, "Cape Verde" = "Cabo Verde"),
         country = fct_recode(country, "South Korea" = "Republic of Korea"),
         country = fct_recode(country, "Russia" = "Russian Federation"),
         country = fct_recode(country, "Republic of Serbia" = "Serbia"),
         country = fct_recode(country, "United States of America" = "United States"))

### Reorder levels of age to be in chronological order. 
data2$age <- factor(data2$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))


## Before Transformation
plt1 <- data2 %>%
  ggplot(aes(x = `gdp_for_year ($)`))+
  geom_histogram(alpha = .50 , fill = "red")+
  xlab("GDP per year in Dollar")
plt2 <- data2 %>%
  ggplot(aes(x = population))+
  geom_histogram(alpha = .50 , fill = "green")+
  xlab("Population")
plt3 <- data2 %>%
  ggplot(aes(x = `gdp_per_capita ($)`))+
  geom_histogram(alpha = .50 , fill = "blue")+
  xlab("GDP per capital in Dollar")
plt4 <- data2 %>%
  ggplot(aes(x = `suicides/100k pop`))+
  geom_histogram(alpha = .50 , fill = "purple")+
  xlab("Suicides / 100k of population")

grid.arrange(plt1 , plt2 , plt3 , plt4 , nrow = 2)

plt_target <- data2 %>%
  ggplot(aes(x = suicides_no ))+
  geom_histogram(fill = "red" , alpha = .50)
plt_target

## Transforming
#Transforming variable
data2 <- data2 %>%
  mutate(gdp_for_year_log = log(`gdp_for_year ($)`),
         gdp_per_capital_log = log(`gdp_per_capita ($)`),
         population_log = log(population),
         suicides_per_100k_pop_log = log(`suicides/100k pop` + 1),
         suicides_no_log = log(suicides_no + 1))

## After Transformation
plt1_2 <- data2 %>%
  ggplot(aes(x = gdp_for_year_log))+
  geom_histogram(alpha = .50 , fill = "red")+
  xlab("GDP per year in Dollar")
plt2_2 <- data2 %>%
  ggplot(aes(x = population_log))+
  geom_histogram(alpha = .50 , fill = "green")+
  xlab("Population")
plt3_2 <- data2 %>%
  ggplot(aes(x = gdp_per_capital_log))+
  geom_histogram(alpha = .50 , fill = "blue")+
  xlab("GDP per capital in Dollar")
plt4_2 <- data2 %>%
  ggplot(aes(x = suicides_per_100k_pop_log))+
  geom_histogram(alpha = .50 , fill = "purple")+
  xlab("Suicides / 100k of population")

grid.arrange(plt1_2 , plt2_2 , plt3_2 , plt4_2 , nrow = 2)

plt_target2 <- data2 %>%
  ggplot(aes(x = suicides_no_log))+
  geom_histogram(fill = "red" , alpha = .50)
plt_target2

## Build Linear Regression Model
### one by one analysis
mod1 <- lm(data = data2, suicides_no_log ~ `HDI for year`)
summary(mod1)
mod2 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log)
summary(mod2)
mod3 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex)
summary(mod3)
mod4 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age)
summary(mod4)
mod5 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age + generation)
summary(mod5)
mod6 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age +  gdp_per_capital_log)
summary(mod6)
mod7 <- lm(data = data2 , suicides_no_log ~ `HDI for year` + gdp_for_year_log + sex + age +  gdp_per_capital_log 
           + suicides_per_100k_pop_log)
summary(mod7)
anova(mod7)
#### plot of residuals
ggplot(data = as.data.frame(mod7$residuals), aes(x = mod7$residuals))+
  geom_histogram(alpha = .60 , fill = "blue")

### Stepwise regression
step1 <- stepAIC(mod7, direction ="backward")
summary(step1)
anova(step1)

step2 <- stepAIC(mod7, direction ="both")
summary(step2)
anova(step2)

## Summary
summary(lm.beta(mod7))

