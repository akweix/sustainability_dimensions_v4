library(plotly)
library(readxl)
# Load the dataset
my_esg_data <- read_excel("Data/cleandata.xlsx")


# Separate Asset Types
my_esg_data$marker_size <- ifelse(my_esg_data$Asset_Type == "SRI" | my_esg_data$Asset_Type == "Market", 6, 5)
my_esg_data$marker_color <- ifelse(my_esg_data$Asset_Type == "SRI", "orange",
                                   ifelse(my_esg_data$Asset_Type == "Market", "black", "blue"))

# Filtering out 'SRI' and 'Market' asset types
non_special_assets <- my_esg_data[my_esg_data$Asset_Type != "SRI" & my_esg_data$Asset_Type != "Market", ]

# Custom color scale
custom_colorscale <- list(
  list(0, "red"),   # Low values are red
  list(0.5, "yellow"), # Mid values are yellow
  list(1, "green")  # High values are green
)


# Create a 3D scatter plot for the majority of the points
p_3d <- plot_ly(data = non_special_assets,
                x = ~Economic,
                y = ~Environmental,
                z = ~Social,
                text = ~paste(`Company Name`, Statement, sep=": "),  # Concatenate Company Name with Statement
                hoverinfo = 'text+xyz', 
                type = 'scatter3d',
                mode = 'markers',
                showlegend = FALSE,
                marker = list(size = 4,
                              color = ~Social,
                              colorscale = custom_colorscale,
                              opacity = 1,
                              line = list(color = 'rgba(0, 0, 0, 0)', width = 0)))

# Create separate traces for the orange and black points
orange_points <- my_esg_data[my_esg_data$Asset_Type == "SRI", ]
black_points <- my_esg_data[my_esg_data$Asset_Type == "Market", ]

p_3d <- add_trace(p_3d,
                  data = orange_points,
                  x = ~Economic,
                  y = ~Environmental,
                  z = ~Social,
                  type = 'scatter3d',
                  mode = 'markers',
                  showlegend = FALSE,
                  marker = list(size = 8,
                                color = 'orange'))

p_3d <- add_trace(p_3d,
                  data = black_points,
                  x = ~Economic,
                  y = ~Environmental,
                  z = ~Social,
                  type = 'scatter3d',
                  mode = 'markers',
                  showlegend = FALSE,
                  marker = list(size = 8,
                                color = 'black'))

# Add the title to the plot using layout()
p_3d <- layout(p_3d,
               title = "Sustainability Visualization of Swiss Stocks & SRI portfolios",
               margin = list(l = 50, r = 50, b = 50, t = 30, pad = 4), 
               height = 600, 
               width = 1000) 


# Define vertices for the green cuboid (high scores)
green_cuboid_vertices <- data.frame(
  x = c(0, 0, 3.289814904, 3.289814904, 0, 0, 3.289814904, 3.289814904),
  y = c(1.05, 1.05, 1.05, 1.05, 1.967616857, 1.967616857, 1.967616857, 1.967616857),
  z = c(0.83, 1.868187822, 0.83, 1.868187822, 0.83, 1.868187822, 0.83, 1.868187822)
)

# Define vertices for the red cuboid (low scores)
red_cuboid_vertices <- data.frame(
  x = c(-3.108606407, -3.108606407, -1, -1, -3.108606407, -3.108606407, -1, -1),
  y = c(-0.3, -0.3, -0.3, -0.3, -1.312494934, -1.312494934, -1.312494934, -1.312494934),
  z = c(-0.68, -1.77268577, -0.68, -1.77268577, -0.68, -1.77268577, -0.68, -1.77268577)
)

# Add the green cuboid with the correct attributes
p_3d <- add_trace(p_3d, 
                  type = "mesh3d", 
                  x = green_cuboid_vertices$x, 
                  y = green_cuboid_vertices$y, 
                  z = green_cuboid_vertices$z,
                  showlegend = FALSE,
                  facecolor = rep('green', 8), # Set facecolor for each vertex
                  opacity = 0.2, # Set opacity to make it semi-transparent
                  hoverinfo = 'none') # Make the cuboid non-interactive

# Add the red cuboid with facecolor for correct color display
p_3d <- add_trace(p_3d, 
                  type = "mesh3d", 
                  x = red_cuboid_vertices$x, 
                  y = red_cuboid_vertices$y, 
                  z = red_cuboid_vertices$z,
                  showlegend = FALSE,
                  facecolor = rep('red', 8), # Set facecolor for each vertex
                  opacity = 0.2, # Set opacity to make it semi-transparent
                  hoverinfo = 'none') # Make the cuboid non-interactive

# Function to generate edge traces for the cuboids
add_cuboid_edges <- function(plot, vertices, color) {
  for(i in seq(1, 4)) {
    # Add vertical lines (edges)
    plot <- add_trace(plot,
                      type = 'scatter3d',
                      mode = 'lines',
                      x = c(vertices$x[i], vertices$x[i+4]),
                      y = c(vertices$y[i], vertices$y[i+4]),
                      z = c(vertices$z[i], vertices$z[i+4]),
                      line = list(color = color, width = 2),
                      hoverinfo = 'none')  # Make lines non-interactive
  }
  # Add horizontal lines (top and bottom faces)
  for(i in c(1, 5)) {
    j <- i + 2 # Opposite corner index
    plot <- add_trace(plot,
                      type = 'scatter3d',
                      mode = 'lines',
                      x = c(vertices$x[i], vertices$x[i+1], vertices$x[j+1], vertices$x[j], vertices$x[i]),
                      y = c(vertices$y[i], vertices$y[i+1], vertices$y[j+1], vertices$y[j], vertices$y[i]),
                      z = c(vertices$z[i], vertices$z[i+1], vertices$z[j+1], vertices$z[j], vertices$z[i]),
                      line = list(color = color, width = 2),
                      hoverinfo = 'none')  # Make lines non-interactive
  }
  return(plot)
}


# Add edges for the green cuboid
p_3d <- add_cuboid_edges(p_3d, green_cuboid_vertices, 'green')

# Add edges for the red cuboid
p_3d <- add_cuboid_edges(p_3d, red_cuboid_vertices, 'red')

# Add invisible points for text labels with colored text
label_points <- data.frame(
  x = c(mean(green_cuboid_vertices$x), mean(red_cuboid_vertices$x)),
  y = c(mean(green_cuboid_vertices$y), mean(red_cuboid_vertices$y)),
  z = c(mean(green_cuboid_vertices$z), mean(red_cuboid_vertices$z)),
  text = c("Sustainable: 3 SRIs and 11% of Stocks", "At Risk: 6% of Stocks"),
  color = c("green", "red") # Assign colors to each label
)

p_3d <- add_trace(p_3d,
                  data = label_points,
                  x = ~x,
                  y = ~y,
                  z = ~z,
                  text = ~text,
                  type = 'scatter3d',
                  mode = 'text',
                  hoverinfo = 'text',
                  showlegend = FALSE,
                  textfont = list(color = ~color, size = 12), # Set the color and size of the text
                  marker = list(size = 1, color = 'rgba(0,0,0,0)')) 

# Final Visualization
p_3d
