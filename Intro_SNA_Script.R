#This file contains a script to the Introduction to Social Network Analysis Workshop
#The Graduate Institute, Geneva, May the 13th
#Module 5: Social Network Analysis
#By Chanaya Punyakumpol

#Load necessary packages
library(tidyverse) #reshaping date and data wrangling
library(ggplot2) #for data visualization
library(igraph)  #we will primarily use igraph package in this workshop


#globally defined parameters
url <- "https://www.designoftradeagreements.org/media/filer_public/87/e2/87e266f0-5874-4596-913a-251e99ab6632/desta_list_of_treaties_02_01_dyads.csv"

#import the data from DESTA and take a first look at the data structure
pta_data <- read_csv(url)
pta_data$regioncon <- as.factor(pta_data$regioncon)

#First, inspect the data
str(pta_data) #look at the structure of the data
head(pta_data) #look at the first few rows of the data

#Look at number of ties by PTAs: choosing two particular variable: year and regioncon
#this line counts, particularly the function count(), the frequency of ties between any two countries formed by year and region.
pta_data_count <- pta_data %>% group_by(year, regioncon) %>% count(name) 
summary(pta_data_count) #show the summary of the data

#notice that that pta_data has a variable called regioncon, to which states a region a particular agreement belongs:
#so, we will explore the differences between bilateral and multilateral:
pta_data_count <- pta_data_count %>% mutate(bilat = ifelse(n == 1, "bilateral", "multilateral")) #creating a new variable to see if a tie belongs to a bilateral or multilateral treaty
pta_data_count$bilat <- as.factor(pta_data_count$bilat) #make this as a factor
region_count <- pta_data_count %>% group_by(regioncon, bilat) %>% count(regioncon) #create a new data counting regioncon by bilat factor
#now we are able to plot the frequency of bilateral/multilateral by region
ggplot(region_count, aes(x= regioncon, y = n, color = bilat, fill = bilat)) +
  geom_col()

#let's see the ties formed by regioncon
ggplot(pta_data_count, aes(x = year, y = n, color = regioncon, fill = regioncon)) +
  geom_col() + facet_wrap(~regioncon)

#From now on, we will explore the inter-regional ties of PTAs
#---------------------I. create a network------------------------------------
cbind(head(pta_data$country1), head(pta_data$country2))
edge_net <- graph_from_edgelist(cbind(head(pta_data$country1), head(pta_data$country2)), directed = F) #create a graph from edgelist
get.adjacency(temp_graph) #create adjacency matrix from the graph


set.seed(123) #set.seed() fixes the configuration of the plot for the sake of reproducibility and comparison in this case
plot.igraph(edge_net)

#now, create the same netweork from a matrix
adjacency_net <- graph_from_adjacency_matrix(get.adjacency(edge_net), mode = "undirected")
set.seed(123)
plot.igraph(matrix_net)


#select only intercontinential ties and creating a network
pta_intercon <- pta_data %>% filter(regioncon == "Intercontinental") #filter for only inter-regional PTAs
pta_intercon_net <- graph_from_edgelist(cbind(pta_intercon$country1,pta_intercon$country2), directed = F) #create the full network from the edgelist

#adding the attributes to edges:
E(pta_intercon_net)$agt_name <- pta_intercon$name #add the name 
E(pta_intercon_net)$year <- pta_intercon$year
head(V(pta_intercon_net)$name) #also comes with the vertex attribute: names
summary(pta_intercon_net) #summary of the network

#-------------------II. Visualization of the network------------------
set.seed(123)
V(pta_intercon_net)$shape = "none" #this deletes the network vertex and replaces the vertex with the names only
plot.igraph(pta_intercon_net,
            vertex.size = 5,
            vertex.color = "red",
            vertex.label = V(pta_intercon_net)$name,
            vertex.label.cex = .5,
            vertex.frame.color = NA,
            layout = layout.fruchterman.reingold,
            main = "The network of inter-regional PTAs"
)

#Difficult to get any insight, I will create two separate networks: bilateral and multilateral
E(pta_intercon_net)$bilateral <- ifelse(pta_intercon$typememb == 1, 1,0) #add the edge attribute indicating whether
pta_bilat_net <- subgraph.edges(pta_intercon_net, which(E(pta_intercon_net)$bilateral == 1))
png("pta_bilat_net.png", width = 10, height = 10, units = "in", res = 300, bg = "white")
set.seed(123)
plot.igraph(pta_bilat_net,
            vertex.size = 3,
            vekrtex.label = V(pta_bilat_net)$name,
            vertex.label.cex = .5,
            vertex.frame.color = NA,
            layout = layout.fruchterman.reingold,
            main = "Inter-Regional, Bilateral PTAs"
)
dev.off()

#Now, let's try multilatertal
pta_multil_net <- subgraph.edges(pta_intercon_net, which(E(pta_intercon_net)$bilateral == 0))
set.seed(123)
plot.igraph(pta_multil_net,
            vertex.size = 3,
            vekrtex.label = V(pta_multil_net)$name,
            vertex.label.cex = .5,
            vertex.frame.color = NA,
            layout = layout.fruchterman.reingold,
            main = "Inter-Regional, multilateral PTAs"
)
#still difficult to understand, alternative to this we can plot a two-mode network for multilateral:
#creating and plotting two-mode networks for multilateral, intercontinental pta
pta_country1 <- pta_intercon %>% filter(typememb != 1) %>% 
  select(name, country1) %>% 
  rename(country = country1)

pta_two_mode <- pta_intercon %>% filter(typememb != 1) %>% 
  select(name, country2) %>%
  rename(country = country2) %>% 
  bind_rows(pta_country1) %>% 
  arrange(name) %>% 
  distinct()

pta_two_mode_net <- graph.data.frame(pta_two_mode, directed = F)
V(pta_two_mode_net)$type <- bipartite_mapping(pta_two_mode_net)$type #add the type attribute

V(pta_two_mode_net)$color <- ifelse(V(pta_two_mode_net)$type, "lightblue", "salmon")
V(pta_two_mode_net)$shape <- ifelse(V(pta_two_mode_net)$type, "none", "circle")
E(pta_two_mode_net)$color <- "lightgray"

png("pta_two_mode_net.png", width = 10, height = 10, units = "in", res = 300, bg = "white")
set.seed(123)
plot.igraph(pta_two_mode_net,
            vertex.size = 1,
            vertex.label = ifelse(V(pta_two_mode_net)$type,V(pta_two_mode_net)$name,""),
            vertex.label.cex = .5,
            layout = layout.fruchterman.reingold,
            vertex.frame.color = NA)
dev.off()

#bilateral two mode network
pta_bilat_country1 <- pta_intercon %>% filter(typememb == 1) %>% 
  select(name, country1) %>% 
  rename(country = country1)

pta_bilat_two_mode <- pta_intercon %>% filter(typememb == 1) %>% 
  select(name, country2) %>%
  rename(country = country2) %>% 
  bind_rows(pta_country1) %>% 
  arrange(name) %>% 
  distinct()

pta_bilat_two_mode_net <- graph.data.frame(pta_bilat_two_mode, directed = F)
V(pta_bilat_two_mode_net)$type <- bipartite_mapping(pta_bilat_two_mode_net)$type #add the type attribute

V(pta_bilat_two_mode_net)$color <- ifelse(V(pta_bilat_two_mode_net)$type, "lightblue", "salmon")
V(pta_bilat_two_mode_net)$shape <- ifelse(V(pta_bilat_two_mode_net)$type, "none", "circle")
E(pta_bilat_two_mode_net)$color <- "lightgray"

plot.igraph(pta_bilat_two_mode_net,
            vertex.size = 1,
            vertex.label = ifelse(V(pta_bilat_two_mode_net)$type,V(pta_bilat_two_mode_net)$name,""),
            vertex.label.cex = .5,
            vertex.frame.color = NA)
#One mode bilateral network is much better

#---------------------III. Basic descriptions of the network---------------------
#First, the degree distribution:
degree_dist_bilat <- as.data.frame(sort(degree(pta_bilat_net), decreasing = T))
colnames(degree_dist_bilat) <- c("degree")
ggplot(degree_dist_bilat, aes(x = degree))+
  geom_bar(fill = "steelblue", color = "steelblue") +
  theme_minimal()

#let's check: which one is denser?
edge_density(pta_bilat_net)
edge_density(pta_multil_net)

#which one has more transitive edges?
transitivity(pta_bilat_net)
transitivity(pta_multil_net)

#---------------------IV. Network Centralities--------------------
V(pta_bilat_net)$degree_cen <- degree(pta_bilat_net, normalized = T)
V(pta_bilat_net)$between_cen <- betweenness(pta_bilat_net, directed = F, normalized = T)
V(pta_bilat_net)$close_cen <- closeness(pta_bilat_net, normalized = T)
V(pta_bilat_net)$eigen_cen <- evcent(pta_bilat_net)$vector
V(pta_bilat_net)$pgrank_cen <- page_rank(pta_bilat_net, directed = F)$vector

#Centrality is useful for for visualization as well:
set.seed(123)
plot(pta_bilat_net,
     vertex.shape = "circle",
     vertex.size = V(pta_bilat_net)$eigen_cen*10,
     vertex.color = "salmon",
     vertex.label = V(pta_bilat_net)$name,
     vertex.label.cex = 0.5,
     vertex.frame.color = NA,
     layout = layout.fruchterman.reingold,
     main = "The network of bilateral inter-regional PTAs (simplified)")

#---------------------V. Community Detection ---------------------
#component analysis
bilat_component_list <- decompose.graph(pta_bilat_net)
V(bilat_component_list[[1]])$eigen_cen <- eigen_centrality(bilat_component_list[[1]])$vector
V(bilat_component_list[[2]])$eigen_cen <- eigen_centrality(bilat_component_list[[2]])$vector

#component 1
plot(bilat_component_list[[1]],
     vertex.size = V(bilat_component_list[[1]])$eigen_cen*10,
     vertex.color = "red",
     vertex.label = V(bilat_component_list[[1]])$name,
     vertex.label.cex = 0.5,
     vertex.frame.color = NA,
     edge.width = E(bilat_component_list[[1]])$weight,
     layout = layout.fruchterman.reingold,
     main = "The network of bilateral inter-regional PTAs - Component 1")
#component 2
plot(bilat_component_list[[2]],
     vertex.size = V(bilat_component_list[[2]])$eigen_cen*10,
     vertex.color = "red",
     vertex.label = V(bilat_component_list[[2]])$name,
     vertex.label.cex = 0.5,
     vertex.frame.color = NA,
     edge.width = E(bilat_component_list[[2]])$weight,
     layout = layout.fruchterman.reingold,
     main = "The network of bilateral inter-regional PTAs - Component 2")

#community detection:
#louvain
intercon_bilat_louvain <- cluster_louvain(pta_bilat_net)
sort(membership(intercon_bilat_louvain))
sizes(intercon_bilat_louvain)
modularity(intercon_bilat_louvain) #this index explains how good the method is
set.seed(123)
plot(pta_bilat_net,
     vertex.size = 1,
     vertex.label = V(pta_bilat_net)$name,
     vertex.label.cex = 0.5,
     vertex.frame.color = NA,
     layout = layout.fruchterman.reingold,
     mark.groups = intercon_bilat_louvain, #here create the communities in the network
     main = "Communities of Inter-Regional PTA Network")

#walktrap
intercon_bilat_walktrap <- cluster_walktrap(pta_bilat_net)
sort(membership(intercon_bilat_walktrap))
sizes(intercon_bilat_walktrap)
modularity(intercon_bilat_walktrap)
set.seed(123)
plot(pta_bilat_net,
     vertex.size = 1,
     vertex.label = V(pta_bilat_net)$name,
     vertex.label.cex = 0.5,
     vertex.frame.color = NA,
     layout = layout.fruchterman.reingold,
     mark.groups = intercon_bilat_walktrap, #here create the communities in the network
     main = "Communities of Inter-Regional PTA Network")
