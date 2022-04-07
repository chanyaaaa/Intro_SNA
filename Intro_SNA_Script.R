#This file contains a script to the Introduction to Social Network Analysis Workshop

#Load necessary packages
library(readr) #to import data
library(dplyr) #for data wrangling
library(ggplot2) #for data visualization
library(igraph) #we will primarily use igraph package in this workshop

pta_data <- read_csv(params$url) 