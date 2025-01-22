# Plotting 16 different wang and engel figures to create the conceptual metaanalysis figure
# Code given by VVM and followed by Christophe on 22 January 2025


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load library 

# Setwd 
setwd("/Users/christophe_rouleau-desrochers/github/asevdiversitydup/figures/metaAnalysisFigure")

# Wang&Engel function
wang <- function(x, Tmin, Topt, Tmax){
  if((x <= Tmax) & (Tmax > (Tmin + 1)) & (Topt > (Tmin + 0.5)) & (Topt < (Tmax - 0.5)) & (x > Tmin)){
    alpha <-  log(2) / log((Tmax - Tmin) / (Topt - Tmin))
    
    num <- 2 * (x - Tmin)^alpha * (Topt - Tmin)^alpha - (x - Tmin)^(2 * alpha)
    den <- (Topt - Tmin)^(2*alpha)
    
    return(num/den)
  }else{
    return(0)
  }
}

# Parameters
Tmin <- -10
Tmax <- 30
Topt <- 20

x <- seq(-40,30,1) 
y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
plot(y~x)
lines(y~x)

dev.off()




# Wang & Engel function
wang <- function(x, Tmin, Topt, Tmax) {
  if ((x <= Tmax) & (Tmax > (Tmin + 1)) & (Topt > (Tmin + 0.5)) & (Topt < (Tmax - 0.5)) & (x > Tmin)) {
    alpha <- log(2) / log((Tmax - Tmin) / (Topt - Tmin))
    
    num <- 2 * (x - Tmin)^alpha * (Topt - Tmin)^alpha - (x - Tmin)^(2 * alpha)
    den <- (Topt - Tmin)^(2 * alpha)
    
    return(num / den)
  } else {
    return(0)
  }
}

# Parameters
Tmin <- -10
Tmax <- 30
Topt <- 20

# Continuous curve (full line)
x <- seq(-40, 30, 0.1)  # Fine resolution for a smooth curve
y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)

# Plot the continuous line
plot(x, y, type = "l", lwd = 2, col = "black", xlab = "x", ylab = "y", main = "Wang & Engel Function")

# Add customizable dots
# Define the specific points to plot (flexible)
x_dots <- seq(-10, 0, length.out = 5)  # Example: 10 equally spaced dots within a range
y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)

# Add dots to the curve
points(x_dots, y_dots, pch = 1, col = "black", cex = 1.5)  # Customize dot appearance

# Clean up device
dev.off()





# Generate and save 5 different figures
set.seed(123)  # Set seed for reproducibility
for (i in 1:5) {
  # Vary parameters for the curve
  Tmin <- sample(seq(-15, -5, 1), 0.001)   # Random Tmin
  Tmax <- 40   # Random Tmax
  Topt <- sample(seq(Tmin + 8, Tmax - 8, 1), 1)  # random optimal temperature
  
  # Create a unique filename for each figure
  filename <- paste0("figure_", i, ".png")
  
  # Open a PNG device
  png(filename, width = 800, height = 600)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.1) 
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 2, col = rainbow(5)[i], xlab = "x", ylab = "y",
    main = paste("Curve Shape - Tmin:", Tmin, "Topt:", Topt, "Tmax:", Tmax)
  )
  
  # Define dots with varying numbers and positions
  n_dots <- sample(3:5, 1)  # Randomly choose between 3 to 5 dots
  x_dots <- sort(runif(n_dots, min = Tmin + 1, max = Tmax - 1))  # Random positions
  y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Add dots to the curve
  points(x_dots, y_dots, pch = 16, col = "red", cex = 1.5)
  
  # Close the PNG device
  dev.off()
} 

# Define the continuous curve
dev.new()
x <- seq(-40, 40, 1)  # Fine resolution for a smooth curve
y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)

# Plot the continuous line
plot(
  x, y, type = "l", lwd = 2, col = rainbow(5)[i], xlab = "x", ylab = "y",
  main = paste("", Tmin, "", Topt, "", Tmax)
)

# Define dots with varying numbers and positions
n_dots <- sample(3:5, 1)  # Randomly choose between 3 to 5 dots
x_dots <- sort(runif(n_dots, min = Tmin + 1, max = Tmax - 1))  # Random positions
y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)

# Add dots to the curve
points(x_dots, y_dots, pch = 16, col = "red", cex = 1.5)
dev.off()
