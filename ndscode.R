# Load necessary libraries
library(ergm)
library(network)
library(wbstats)
library(igraph) #--Basic creation and handling of "graph" objects--#
library(network) #--Basic creation and handling of "network" objects--#
library(intergraph) #--To switch back and forth between "graph" and "network" objects--#
library(statnet) #--For basic network plots like gplots--#
library(networkD3) #--To create interactive visuals--#
library(visNetwork) #--To create interactive visuals--#
library(networkDynamic) #--To analyse networks that evolve over time--#
library(tsna) #--Another package to analyse networks that evolve over time--#
library(UserNetR) #--Contains some network data sets--#
library(sna)
library(tergm)
library(tidyverse)
library(Rglpk)
library(NetworkChange)
library(qgraph)
library(ggthemes)
library(ggplot2)




#prepare the data
Y2001<-as.matrix(X2001D)
Y2005<-as.matrix(X2005D)
Y2008<-as.matrix(X2008D)
Y2010<-as.matrix(X2010D)
Y2015<-as.matrix(X2015D)
Y2018<-as.matrix(X2018D)
#Y2021<-as.matrix(X2021)


#country subset G20 and tax- haven countries
country_names <- c("Argentina", "Aruba", "Australia", "Bahamas", "Bahrain", "Bermuda", "Brazil", "Canada", 
                   "CaymanIslands", "HongKong", "Macao", "CostaRica", "Cyprus", "France", "Germany", 
                   "Indonesia", "Ireland", "IsleofMan", "Italy", "Japan", "Korea", "Lebanon", "Malta", 
                   "Mauritius", "Panama", "Russian", "Singapore", "SouthAfrica", "Switzerland", "Turkey", 
                   "UnitedKingdom", "UnitedStates", "Vanuatu")


#subset
Y2001<- Y2001[country_names, country_names]
Y2005<- Y2005[country_names, country_names]
Y2008<- Y2008[country_names, country_names]
Y2010<- Y2010[country_names, country_names]
Y2015<- Y2015[country_names, country_names]
Y2018<- Y2018[country_names, country_names]
#Y2021<- Y2021[country_names, country_names]







# Create graph from adjacency matrix
c2001 <- graph.adjacency(Y2001, mode = "directed")
c2005 <- graph.adjacency(Y2005, mode = "directed")
c2008 <- graph.adjacency(Y2008, mode = "directed")
c2010 <- graph.adjacency(Y2010, mode = "directed")
c2015 <- graph.adjacency(Y2015, mode = "directed")
c2018 <- graph.adjacency(Y2018, mode = "directed")


# Define the labels for the vertices
vertex_labels <- colnames(Y2018) 

# Plot the graph using qgraph with full labels and custom styling
qgraph(Y2018, layout = "spring", 
       vsize = 2,  # Size of the nodes
       esize = 0.2,   # Width of the edges
       edge.color = "#7B1FA2",   # Color of the edges
       labels = vertex_labels,  # Full labels for the nodes
       label.color = "black",   # Color of the labels
       label.cex = 10,           # Size of the labels
       color = "grey80",        # Color of the nodes
       border.color = "white",  # Color of the node borders
       shape = "circle",
       asize=1)        # Shape of the nodes


#for galaxies
# Define a custom black background
par(bg = "black")

qgraph(Y2018, 
       layout = "spring", 
       vsize = 3,               # Size of the nodes
       esize = 0.2,             # Width of the edges
       edge.color = "#00BFFF",  # Shiny star blue color for the edges
       labels = vertex_labels,  # Full labels for the nodes
       label.color = "black",   # Color of the labels to contrast with black background
       label.cex = 10,          # Size of the labels
       color = "white",       # Shiny star blue color of the nodes
       border.color = "#1E90FF",# Same as node color to remove border effect
       shape = "circle",        # Shape of the nodes
       asize = 1)               # Arrow size


#####network statistic=================================================
#network density
densities <- data.frame(
  year = c(2001, 2005, 2008, 2010, 2015, 2018),
  density = c(
    edge_density(c2001),
    edge_density(c2005),
    edge_density(c2008),
    edge_density(c2010),
    edge_density(c2015),
    edge_density(c2018)
  )
)


#cluster coeffecient

transitivity <- data.frame(
  year = c(2001, 2005, 2008, 2010, 2015, 2018),
  transitivity = c(
    transitivity(c2001, type = "global"),
    transitivity(c2005, type = "global"),
    transitivity(c2008, type = "global"),
    transitivity(c2010, type = "global"),
    transitivity(c2015, type = "global"),
    transitivity(c2018, type = "global")
  )
)


# Merge the data frames on 'year'
data <- merge(densities, transitivity, by = "year")

ggplot(data, aes(x = density, y = transitivity)) +
  geom_point(color = "blue", size = 3) +  # Blue points
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +  # Red dashed trend line
  labs(title = "The Investment Universe Density and Clustering Coefficient",
       subtitle = "Data from 2001 to 2018",
       x = "Density",
       y = "Clustering Coefficient") +
  theme_economist() 


#diameter
diameter(c2001)
diameter(c2005)
diameter(c2008)
diameter(c2010)
diameter(c2015)
diameter(c2018)


densities$year <- as.factor(densities$year)

#visualisasi
ggplot(densities, aes(x = year, y = density, group = 1)) +
  geom_line(color = "steelblue", size = 1) +  # Line chart with specified color and line thickness
  geom_point(color = "steelblue", size = 3) +  # Add points to the line chart
  theme_economist() +
  labs(title = "Edge Density Over Time", x = "Year", y = "Edge Density") +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )



#centrality measure
#degree centrality
total_degree2001 <- degree(c2001, mode = "all")
total_degree2005 <- degree(c2005, mode = "all")
total_degree2008 <- degree(c2008, mode = "all")
total_degree2010 <- degree(c2010, mode = "all")
total_degree2015 <- degree(c2015, mode = "all")
total_degree2018 <- degree(c2018, mode = "all")
# Sort the degree centrality in descending order and get the top 5 nodes
top_8_degree2001 <- sort(total_degree2001, decreasing = TRUE)[1:8]
top_8_degree2005 <- sort(total_degree2005, decreasing = TRUE)[1:8]
top_8_degree2008 <- sort(total_degree2008, decreasing = TRUE)[1:8]
top_8_degree2010 <- sort(total_degree2010, decreasing = TRUE)[1:8]
top_8_degree2015 <- sort(total_degree2015, decreasing = TRUE)[1:8]
top_8_degree2018 <- sort(total_degree2018, decreasing = TRUE)[1:8]


#betweenenss

top_8_bt2001 <- sort(betweenness(c2001), decreasing = TRUE)[1:8]
top_8_bt2005 <- sort(betweenness(c2005), decreasing = TRUE)[1:8]
top_8_bt2008 <- sort(betweenness(c2008), decreasing = TRUE)[1:8]
top_8_bt2010 <- sort(betweenness(c2010), decreasing = TRUE)[1:8]
top_8_bt2015 <- sort(betweenness(c2015), decreasing = TRUE)[1:8]
top_8_bt2018 <- sort(betweenness(c2018), decreasing = TRUE)[1:8]

#closeness
top_8_cl2001 <- sort(closeness(c2001), decreasing = TRUE)[1:8]
top_8_cl2005 <- sort(closeness(c2005), decreasing = TRUE)[1:8]
top_8_cl2008 <- sort(closeness(c2008), decreasing = TRUE)[1:8]
top_8_cl2010 <- sort(closeness(c2010), decreasing = TRUE)[1:8]
top_8_cl2015 <- sort(closeness(c2015), decreasing = TRUE)[1:8]
top_8_cl2018 <- sort(closeness(c2018), decreasing = TRUE)[1:8]

#eigenvector
top_8_egn2001 <- sort(eigen_centrality(c2001)$vector, decreasing = TRUE)[1:8]
top_8_egn2005 <- sort(eigen_centrality(c2005)$vector, decreasing = TRUE)[1:8]
top_8_egn2008 <- sort(eigen_centrality(c2008)$vector, decreasing = TRUE)[1:8]
top_8_egn2010 <- sort(eigen_centrality(c2010)$vector, decreasing = TRUE)[1:8]
top_8_egn2015 <- sort(eigen_centrality(c2015)$vector, decreasing = TRUE)[1:8]
top_8_egn2018 <- sort(eigen_centrality(c2018)$vector, decreasing = TRUE)[1:8]




#attribute analysis
#add attribute to the graph
V(c2018)$region <- Node.Detail$World.Bank.Region
V(c2018)$income <- Node.Detail$World.Bank.Income.Classification
V(c2018)$coruption <- Node.Detail$Corruption.Perceptions.Index.2018
V(c2018)$govsys <- Node.Detail$System.of.Government
V(c2018)$econclas <- Node.Detail$Classification





#centrality
total_degree <- degree(c2001, mode = "all")
# In-Degree Centrality
in_degree <- degree(c2001, mode = "in")
# Out-Degree Centrality
out_degree <- degree(c2001, mode = "out")
#
blackhole <- out_degree - in_degree


# You can adjust the multiplier to better suit your visualization needs
normalized_degree <- total_degree/2

# Plot the graph
# Create a color vector for vertices based on degree (age analogy)
#vertex_colors <- ifelse(out_degree > 25, "#FFFF00",   # Bright Yellow (older)
#                        ifelse(out_degree > 20, "#FFD700",  # Gold (less old)
#                               ifelse(out_degree > 15, "#FFA500",  # Orange (younger)
#                                      "grey80")))   # Dark Orange (youngest)

vertex_colors <- ifelse(out_degree > 25 & blackhole >= 0, "#FFFF00",   # Bright Yellow (older)
                        ifelse(out_degree > 20 & blackhole >= 0, "#FFD700",  # Gold (less old)
                               ifelse(out_degree > 15 & blackhole >= 0, "#FFA500",  # Orange (younger)
                                      ifelse(blackhole < 0, "grey80", "grey80"))))  # Grey if blackhole < 0

center_node <- which.max(V(c2001)$total_degree)
# Plot the graph
plot(c2001, 
     layout = layout_as_star(c2001),  # You can choose any layout function you prefer
     edge.arrow.size = 0.005,         # Change the size of the arrowhead
     edge.arrow.width = 2,            # Change the width of the arrowhead
     edge.color = "#00BFFF",          # Change the color of the arrows
     edge.arrow.mode = 1,             # Arrow mode
     vertex.color = vertex_colors,    # Node color based on degree
     vertex.frame.color = "white",   # Node frame color
     vertex.size = normalized_degree, # Set node size based on normalized degree
     vertex.label = V(c2001)$media, 
     vertex.label.color = "black",    # Label color
     vertex.label.cex = 0.8,          # Adjust the size of the labels
     edge.curved = 0.2                # Add curvature to the edges
)


#the best visualization==========================================

# Assign total_degree attribute
V(c2001)$total_degree <- degree(c2001, mode = "in") + degree(c2001, mode = "out")

# Identify center nodes with total_degree greater than 10
center_nodes <- which(V(c2001)$total_degree > 20)

# Create the star layout with the first identified center node
star_layout <- layout_as_star(c2001, center = center_nodes[1])

# Adjust the layout to place all center nodes in a small circle around the origin
center_radius <- 0.5  # Adjust the radius of the circle
center_angle <- 2 * pi / length(center_nodes)
for (i in seq_along(center_nodes)) {
  star_layout[center_nodes[i], ] <- c(center_radius * cos(center_angle * i), center_radius * sin(center_angle * i))
}

# Spread the remaining nodes around the center nodes
remaining_nodes <- setdiff(1:vcount(c2001), center_nodes)
angle <- 2 * pi / length(remaining_nodes)
for (i in seq_along(remaining_nodes)) {
  star_layout[remaining_nodes[i], ] <- c(cos(angle * i), sin(angle * i))
}

# Scale the layout to make the area smaller if necessary
scale_factor <- 0.5  # Adjust this factor to control the layout size
star_layout <- star_layout * scale_factor

# Assuming you have already defined vertex_colors and normalized_degree
plot(c2001, 
     layout = star_layout,
     edge.arrow.size = 0.005,         # Change the size of the arrowhead
     edge.arrow.width = 2,            # Change the width of the arrowhead
     edge.color = "#00BFFF",          # Change the color of the arrows
     edge.arrow.mode = 1,             # Arrow mode
     vertex.color = vertex_colors,    # Node color based on degree
     vertex.frame.color = "white",    # Node frame color
     vertex.size = normalized_degree, # Set node size based on normalized degree
     vertex.label = V(c2001)$media, 
     vertex.label.color = "black",    # Label color
     vertex.label.cex = 2,          # Adjust the size of the labels
     edge.curved = 0.2                # Add curvature to the edges
)


#the best visualization for 2008=========================
total_degree <- degree(c2008, mode = "all")
# In-Degree Centrality
in_degree <- degree(c2008, mode = "in")
# Out-Degree Centrality
out_degree <- degree(c2008, mode = "out")
#
blackhole <- out_degree - in_degree


# You can adjust the multiplier to better suit your visualization needs
normalized_degree <- total_degree/2

# Plot the graph
# Create a color vector for vertices based on degree (age analogy)
#vertex_colors <- ifelse(out_degree > 25, "#FFFF00",   # Bright Yellow (older)
#                        ifelse(out_degree > 20, "#FFD700",  # Gold (less old)
#                               ifelse(out_degree > 15, "#FFA500",  # Orange (younger)
#                                      "grey80")))   # Dark Orange (youngest)

vertex_colors <- ifelse(out_degree > 25 & blackhole >= 0, "#FFFF00",   # Bright Yellow (older)
                        ifelse(out_degree > 20 & blackhole >= 0, "#FFD700",  # Gold (less old)
                               ifelse(out_degree > 15 & blackhole >= 0, "#FFA500",  # Orange (younger)
                                      ifelse(blackhole < 0, "grey80", "grey80"))))  # Grey if blackhole < 0

# Assign total_degree attribute
V(c2008)$total_degree <- degree(c2008, mode = "in") + degree(c2008, mode = "out")

# Identify center nodes with total_degree greater than 10
center_nodes <- which(V(c2008)$total_degree > 20)

# Create the star layout with the first identified center node
star_layout <- layout_as_star(c2008, center = center_nodes[1])

# Adjust the layout to place all center nodes in a small circle around the origin
center_radius <- 0.5  # Adjust the radius of the circle
center_angle <- 2 * pi / length(center_nodes)
for (i in seq_along(center_nodes)) {
  star_layout[center_nodes[i], ] <- c(center_radius * cos(center_angle * i), center_radius * sin(center_angle * i))
}

# Spread the remaining nodes around the center nodes
remaining_nodes <- setdiff(1:vcount(c2008), center_nodes)
angle <- 2 * pi / length(remaining_nodes)
for (i in seq_along(remaining_nodes)) {
  star_layout[remaining_nodes[i], ] <- c(cos(angle * i), sin(angle * i))
}

# Scale the layout to make the area smaller if necessary
scale_factor <- 0.5  # Adjust this factor to control the layout size
star_layout <- star_layout * scale_factor

# Assuming you have already defined vertex_colors and normalized_degree
plot(c2008, 
     layout = star_layout,
     edge.arrow.size = 0.005,         # Change the size of the arrowhead
     edge.arrow.width = 2,            # Change the width of the arrowhead
     edge.color = "#00BFFF",          # Change the color of the arrows
     edge.arrow.mode = 1,             # Arrow mode
     vertex.color = vertex_colors,    # Node color based on degree
     vertex.frame.color = "white",    # Node frame color
     vertex.size = normalized_degree, # Set node size based on normalized degree
     vertex.label = V(c2008)$media, 
     vertex.label.color = "black",    # Label color
     vertex.label.cex = 2,          # Adjust the size of the labels
     edge.curved = 0.2                # Add curvature to the edges
)




#black hole visualization
# Filter nodes with negative degree difference
blackhole <- V(c2001)[blackhole < 0]


# Create a subgraph with these nodes
subgraph <- induced_subgraph(c2001, blackhole)
total_degree <- degree(subgraph, mode = "all")
#add variable to the network
V(subgraph)$total_degree <- degree(subgraph, mode = "all")
normalized_degree <- total_degree*3
center_node <- which.max(V(subgraph)$total_degree)
                                      
# Plot the graph
plot(subgraph, 
     layout = layout_as_star(subgraph,center = center_node),
     edge.arrow.size = 0.005,         # Change the size of the arrowhead
     edge.arrow.width = 2,            # Change the width of the arrowhead
     edge.color = "#00BFFF",          # Change the color of the arrows
     edge.arrow.mode = 1,             # Arrow mode
     vertex.color = "black",    # Node color based on degree
     vertex.frame.color = "white",   # Node frame color
     vertex.size = normalized_degree, # Set node size based on normalized degree
     vertex.label = V(subgraph)$media, 
     vertex.label.color = "white",    # Label color
     vertex.label.cex = 3,          # Adjust the size of the labels
     edge.curved = 0.2                # Add curvature to the edges
)


#bikin buat semua nya, universe black
#lalu buat visualisasi control siapa yang paling besar koneksi dan yang paling kecil
#bikin perbedaan warna G20 dan tax haven countries
#baru cek lagi, berdasarkan parameter lain, diameter , density, yang paling banyak in, yang paling banyak out
#gambarkan step one by one, per countries




# Convert igraph object to network object
initial_network <- asNetwork(c2021)

# Fit the simpler p* model (ERGMs) with only edges
fit_simple <- ergm(initial_network ~ triangles)




