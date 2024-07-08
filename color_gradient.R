library(ggplot2)
library(gridExtra)

# Define gradient colors for each main color using named colors and their hex codes
yellow_gradient <- data.frame(Name = c("lightyellow", "lightgoldenrodyellow", "yellow", "gold", "orange"),
                              Hex = c("#FFFFE0", "#FAFAD2", "#FFFF00", "#FFD700", "#FFA500"))
red_gradient <- data.frame(Name = c("mistyrose", "lightcoral", "red", "firebrick", "darkred"),
                           Hex = c("#FFE4E1", "#F08080", "#FF0000", "#B22222", "#8B0000"))
pink_gradient <- data.frame(Name = c("seashell", "lightpink", "hotpink", "deeppink", "palevioletred"),
                            Hex = c("#FFF5EE", "#FFB6C1", "#FF69B4", "#FF1493", "#DB7093"))
violet_gradient <- data.frame(Name = c("lavender", "thistle", "orchid", "mediumorchid", "darkorchid"),
                              Hex = c("#E6E6FA", "#D8BFD8", "#DA70D6", "#BA55D3", "#9932CC"))
blue_gradient <- data.frame(Name = c("azure", "lightblue", "skyblue", "steelblue", "blue"),
                            Hex = c("#F0FFFF", "#ADD8E6", "#87CEEB", "#4682B4", "#0000FF"))
green_gradient <- data.frame(Name = c("honeydew", "palegreen", "lime", "green", "darkgreen"),
                             Hex = c("#F0FFF0", "#98FB98", "#00FF00", "#008000", "#006400"))
brown_gradient <- data.frame(Name = c("seashell", "burlywood", "chocolate", "brown", "saddlebrown"),
                             Hex = c("#FFF5EE", "#DEB887", "#D2691E", "#A52A2A", "#8B4513"))

# Combine all gradients into a list
color_gradients <- list(
  "Yellow" = yellow_gradient,
  "Red" = red_gradient,
  "Pink" = pink_gradient,
  "Violet" = violet_gradient,
  "Blue" = blue_gradient,
  "Green" = green_gradient,
  "Brown" = brown_gradient
)

# Function to create a gradient plot for each color
create_gradient_plot <- function(color_name, gradient_df) {
  ggplot(gradient_df, aes(x = factor(1:nrow(gradient_df)), y = 1, fill = Hex)) +
    geom_tile(color = "gray", size = 0.5) +
    geom_text(aes(label = paste(Name, "\n", Hex)), vjust = 0.5, size = 2.5) +  # Add text labels
    scale_fill_identity() +
    theme_void() +
    labs(title = color_name) +
    theme(
      plot.title = element_text(hjust = 0.1, size = 14, face = "bold"),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1), "lines")  # Add margin to avoid text cut-off
    )
}

# Generate plots for each color gradient
gradient_plots <- lapply(names(color_gradients), function(name) create_gradient_plot(name, color_gradients[[name]]))

# Arrange the gradient plots in a grid
grid.arrange(grobs = gradient_plots, ncol = 1)
