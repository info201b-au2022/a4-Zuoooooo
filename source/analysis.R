library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# get the incarceration_trends.csv data
data <- get_data()
# clean the data that I want
new_data <- select(data, "year", "state", "county_name", "total_pop",
                   "urbanicity","division","total_jail_pop", "aapi_jail_pop", 
                   "black_jail_pop", "latinx_jail_pop", "native_jail_pop",
                   "white_jail_pop", "other_race_jail_pop")

convert <- function(x) {
  new <- as.numeric(x)
  return(new)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
data_summary <- function(new_data) {
# cleaning the data of 2008, and 2018 to observe the data changes in decade
data_2008 <- filter(new_data, year == 2008)
data_2018 <- filter(new_data, year == 2018)

# the average population in jail (2008, 2018)
avg_jail_pop_2008 <- mean(data_2008$total_jail_pop, na.rm = TRUE)
avg_jail_pop_2018 <- mean(data_2018$total_jail_pop, na.rm = TRUE)

# the average population of black people and white people in jail(2008, 2018)
black_jail_pop_2008 <- mean(data_2008$black_jail_pop, na.rm = TRUE)
white_jail_pop_2008 <- mean(data_2008$white_jail_pop, na.rm = TRUE)
#
black_jail_pop_2018 <- mean(data_2018$black_jail_pop, na.rm = TRUE)
white_jail_pop_2018 <- mean(data_2018$white_jail_pop, na.rm = TRUE)

# the ratio of incarceration (2008, 2018)  0.38 --> 0.34
total_black_jail_pop_2008 <- sum(data_2008$black_jail_pop, na.rm = TRUE)
total_jail_pop_2008 <- sum(data_2008$total_jail_pop, na.rm = TRUE)
ratio_2008 <- round(total_black_jail_pop_2008/total_jail_pop_2008, 2)
# 
total_black_jail_pop_2018 <- sum(data_2018$black_jail_pop, na.rm = TRUE)
total_jail_pop_2018 <- sum(data_2018$total_jail_pop, na.rm = TRUE)
ratio_2018 <- round(total_black_jail_pop_2018/total_jail_pop_2018, 2) 

# the area of the highest and lowest crime county name(2008, 2018)
highest_crime_region_2008 <- max(data_2008$county_name, na.rm = TRUE)
lowest_crime_region_2008 <- min(data_2008$county_name, na.rm = TRUE)
#
highest_crime_region_2018 <- max(data_2018$county_name, na.rm = TRUE)
lowest_crime_region_2018 <- min(data_2018$county_name, na.rm = TRUE)

# the data summary
summary_paragraph <- paste("According to the data, 
the average criminal population of each county in 2008 was", avg_jail_pop_2008, 
"and the population in 2018 was", avg_jail_pop_2018, ". In comparison, the average criminal population 
of each county from 2008 to 2018 decreased.
In addition, in 2008, the number of criminal populations of black people in 
each country was", black_jail_pop_2008, "and in 2018, the populations was", black_jail_pop_2018, "showing a significant decline.
The number of white criminals in each county across the country remained 
essentially unchanged between 2008 and 2018. In 2008, the black crime rate accounted for", ratio_2008, 
", of the total crime rate. In 2018, the data was", ratio_2018, ", and the data decreased slightly. 
The city with the lowest crime rate in 2008 and 2018 has been the",lowest_crime_region_2008, 
"and the city with the highest crime rate has been the",highest_crime_region_2018,".")

return(cat(summary_paragraph))
}


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# This part that I will use DPLYR and ggplot2 to replicate figure
#----------------------------------------------------------------------------#
# This function will return prison_pop_data showing the prison population by year
get_year_jail_pop <- function() {
  prison_pop_data <- data %>%
      select(year, total_jail_pop) %>%
      group_by(year) %>%
      summarize(year_total = sum(total_jail_pop, na.rm=TRUE))
    return(prison_pop_data) 
  }

# This function will return a bar chart to show the growth of the U.S. Prison Population
plot_jail_pop_for_us <- function(prison_pop_data)  {
  graphy <- ggplot(data = prison_pop_data, aes(x = year, y = year_total)) +
    geom_bar(stat="identity") + 
    labs(title = "Growth of the U.S. Prison Population(1970-2018)", 
            x = "Year", y = "Total Jail Population", 
         caption = "The chart shows the growth of the U.S. Prison Population from 1970 to 2018")
  return(graphy)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# This part that I will use DPLYR and ggplot2 to produce a line chart
#----------------------------------------------------------------------------#
# This function will return another frame showing the growth of prison population by State 
get_jail_pop_by_states <- function(states) {
  get_jail_pop_states <- data %>%
      filter(state %in% states) %>%
      group_by(year, state) %>%
      summarize(state_pop = sum(total_jail_pop, na.rm=TRUE))
  return(get_jail_pop_states)
}

## This function will return a line chart to show the growth Prison Population by State
plot_jail_pop_by_states <- function(states) {
  graphy <- ggplot(data = get_jail_pop_states, aes(x = year, y = state_pop, color = state)) +
    geom_line() +
    labs(title = "Growth of Prison Population by State (1970-2018)", 
         x = "Year", y = "Total Jail Population",
         caption = "The chart shows the growth of the U.S. Prison Population by states from 1970 to 2018")
  return(graphy)
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# The comparison between white people and the black people in jail
# This part that I will use scatter chart to reveal potential patterns of inequality
#----------------------------------------------------------------------------#
section5 <- function() {
  black_white_pop <- data %>%
    select("year", "black_jail_pop", "white_jail_pop") %>%
    group_by(year) %>%
    summarize("black_jail_pop"= sum(black_jail_pop, na.rm=TRUE),
              "white_jail_pop"= sum(white_jail_pop, na.rm=TRUE))
  return(black_white_pop)
}

## This function will return a scatter plot to reveal potential patterns of inequality 
section5_plot <- function(black_white_pop) {
  graphy <- ggplot(data = black_white_pop, aes(x = black_jail_pop, y = white_jail_pop)) +
    geom_point() +
    theme_ipsum() +
  labs(title = "The relationship between white people and the black people", 
      x = "White Jail Population", y = "Black Jail Population",
       caption = "The chart shows the relationship between white people and the black people in jail")
  return(graphy)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# A map shows potential patterns of inequality that vary geographically in U.S.
# This part that I will produce a map that reveals a potential inequality.
# This function will return a frame to show the total jail population of each state in 2018
section6 <- function() {
  data_2018 <- filter(new_data, year == 2018)
  jail_pop_2018 <- data_2018 %>%
    select("state", "total_jail_pop") %>%
    group_by(state) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_pop_2018)
}
# This function will return a map that reveals a potential inequality
section6_state <- function(data_use) {
  state <- map_data("state")
  state$region <- toupper(state$region)
  state$region <- state.abb[match(state$region, toupper(state.name))]
  names(data_use)[names(data_use)=="state"] <- "region"
  data1 <- inner_join(state, data_use,by = "region")
  graph <- ggplot(data = data1, aes(x = long, y = lat,group=group))+
    geom_polygon(aes(fill = total_jail_pop), color = "black")+
    scale_fill_continuous(name = "total jail population")+
    ggtitle("Total jail population of each state in 2018") +
    labs(x= "latitude", y= "longitude", caption = "The map shows the prison population of different state in U.S.")
  return(graph)
}



