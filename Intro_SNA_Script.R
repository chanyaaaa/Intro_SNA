#This file contains a script to the Introduction to Social Network Analysis Workshop

#Load necessary packages
library(readxl) #to import data
library(dplyr) #for data wrangling
library(ggplot2) #for data visualization
library(igraph) #we will primarily use igraph package in this workshop


#globally defined parameters
url <- "https://www.designoftradeagreements.org/media/filer_public/ab/ee/abee41ef-f5e5-44b6-91db-5e8befe48fe5/desta_list_of_treaties_02_01_dyads.xlsx"

#import the data from DESTA and take a first look at the data structure
excel_file <- tempfile()
download.file(url,excel_file, mode = "wb")
pta_data <- read_excel(path = excel_file, sheet = 2)
pta_data$regioncon <- as.factor(pta_data$regioncon)
str(pta_data) #look at the structure of the data
head(pta_data) #look at the first few rows of the data

#Look at number of ties by PTAs
pta_data_count <- pta_data %>% group_by(year, regioncon) %>% count(name)
pta_tie_count <- pta_data %>% group_by(regioncon) %>% count(name)

ggplot(pta_tie_count, aes(x = regioncon, y = n, color = regioncon, fill = regioncon)) +
  geom_col()

ggplot(pta_data_count, aes(x = year, y = n)) +
  geom_col()

ggplot(pta_data_count, aes(x = year, y = n, color = regioncon, fill = regioncon)) +
  geom_point()

ggplot(pta_data_count, aes(x = year, y = n, color = regioncon, fill = regioncon)) +
  geom_col() + facet_wrap(~regioncon)


#only intercontinential ties and creating a network
pta_intercon <- pta_data %>% filter(regioncon == "Intercontinental")
pta_intercon_net <- graph_from_edgelist(cbind(pta_intercon$country1,pta_intercon$country2))

