Project Title: Global Suicides Analysis
1.	File Structure:
 
 
2.	Step-by-Step Manual
Move Files
Please keep data file master.xlsx, master.csv, shiny_v1.0. R and Base_Analysis.R at the same working directory
Installing 
Please take note below packages are required to be installed before running Shiny App, if not please run below commands:

install.packages(shiny)
install.packages(shinydashboard)
install.packages(ggplot2)
install.packages(dplyr)
install.packages(tidyverse) # general
install.packages(ggalt) # dumbbell plots
install.packages(countrycode) # continent
install.packages(rworldmap) # quick country-level heat maps
install.packages(gridExtra) # plots
install.packages(broom) # significant trends within countries
install.packages(readxl)
install.packages(DT)
install.packages(highcharter)
install.packages(viridis)

Testing
After installation completes, please run app shiny_v1.0.R 
Test 1: If below screening comes out, verification 1 success.
 
Test 2: Click menu Exploratory Analysis – Continent, If below screening comes out, verification 2 success.
 



