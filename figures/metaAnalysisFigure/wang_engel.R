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

# === === === === === === === === === === === === === === ===
# Set parameters for the 6 figures of rootstock 1
# === === === === === === === === === === === === === === ===
parameters <- list(
  list(Tmin = -40, Topt = 10, Tmax = 40), # figure 1
  list(Tmin = -20, Topt = 22, Tmax = 40), # figure 2
  list(Tmin = -35, Topt = 20, Tmax = 40), # figure 3
  list(Tmin = -15, Topt = 30, Tmax = 40), # figure 4
  list(Tmin = -40, Topt = 20, Tmax = 40), # figure 5
  list(Tmin = -40, Topt = 15, Tmax = 40) # figure 6
)

# Generate and save 5 different figures
for (i in 1:6) {
  # Extract parameters for this figure
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Create a unique filename for each figure
  filename <- paste0("rootstock1_", i, ".pdf")
  
  # Open a pdf device
  pdf(filename)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.01)  # Extend range to fit all curves
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 8, col = "#c701ff", xlab = "", ylab = "",
    main = "", bty = "l", xaxt = "n", yaxt = "n",
  )
  # keep ticks, remove their labels
  axis(1, labels = FALSE, tick = TRUE) 
  axis(2, labels = FALSE, tick = TRUE) 
  # Define dots with varying numbers and positions
  n_dots <- 2 + (i %% 4)  # Alternate between 3, 4, and 5 dots
  x_dots <- sort(seq(Tmin + 10, Tmax - 10, length.out = n_dots))  # Equally spaced dots
  y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Add dots to the curve
  points(x_dots, y_dots, pch = 19, col = "black", cex = 4)
  
  # Close the pdf device
  dev.off()
}


# === === === === === === === === === === === === === === ===
# Set parameters for the 3 figures of rootstock 2
# === === === === === === === === === === === === === === ===
parameters <- list(
  list(Tmin = -40, Topt = 20, Tmax = 40), # figure 1
  list(Tmin = -20, Topt = 30, Tmax = 40), # figure 2
  list(Tmin = -35, Topt = 20, Tmax = 40), # figure 2
  list(Tmin = -35, Topt = 15, Tmax = 40) # figure 3
)

for (i in 1:4) {
  # Extract parameters for this figure
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Create a unique filename for each figure
  filename <- paste0("rootstock2_", i, ".pdf")
  
  # Open a pdf device
  pdf(filename)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.01)  # Extend range to fit all curves
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 8, col = "#c83034", xlab = "", ylab = "",
    main = "", bty = "l", xaxt = "n", yaxt = "n",
  )
  # keep ticks, remove their labels
  axis(1, labels = FALSE, tick = TRUE) 
  axis(2, labels = FALSE, tick = TRUE) 
  # Define dots with varying numbers and positions
  n_dots <- 3 + (i %% 3)  # Alternate between 3, 4, and 5 dots
  x_dots <- sort(seq(Tmin + 10, Tmax - 10, length.out = n_dots))  # Equally spaced dots
  y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Add dots to the curve
  points(x_dots, y_dots, pch = 19, col = "black", cex = 4)
  
  # Close the pdf device
  dev.off()
}


# === === === === === === === === === === === === === === ===
# Set parameters for the 2 ND figures of Rootstock 2
# === === === === === === === === === === === === === === ===
parameters <- list(
  list(Tmin = -40, Topt = 20, Tmax = 40), # figure 1
  list(Tmin = -30, Topt = 15, Tmax = 40)# figure 2
)


# Generate and save 2 different figures
for (i in 1:2) {
  # Extract parameters for this figure
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Create a unique filename for each figure
  filename <- paste0("rootstock2ND_", i, ".pdf")
  
  # Open a pdf device
  pdf(filename)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.01)  # Extend range to fit all curves
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 8, col = "#c83034", lty = 2, xlab = "", ylab = "",
    main = "", bty = "l", xaxt = "n", yaxt = "n",
  )
  # keep ticks, remove their labels
  axis(1, labels = FALSE, tick = TRUE) 
  axis(2, labels = FALSE, tick = TRUE) 
  # Close the pdf device
  dev.off()
}


# === === === === === === === === === === === === === === ===
# Set parameters for the 2 figures of rootstock 3
# === === === === === === === === === === === === === === ===
parameters <- list(
  list(Tmin = -10, Topt = 30, Tmax = 40), # figure 1
  list(Tmin = -40, Topt = 20, Tmax = 40), # figure 2
  list(Tmin = -20, Topt = 25, Tmax = 40) # figure 3
)

for (i in 1:3) {
  # Extract parameters for this figure
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Create a unique filename for each figure
  filename <- paste0("rootstock3_", i, ".pdf")
  
  # Open a pdf device
  pdf(filename)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.01)  # Extend range to fit all curves
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 8, col = "#003a7d", xlab = "", ylab = "",
    main = "", bty = "l", xaxt = "n", yaxt = "n",
  )
  # keep ticks, remove their labels
  axis(1, labels = FALSE, tick = TRUE) 
  axis(2, labels = FALSE, tick = TRUE) 
  # Define dots with varying numbers and positions
  n_dots <- 2 + (i %% 2)  # Alternate between 3, 4, and 5 dots
  x_dots <- sort(seq(Tmin + 10, Tmax - 10, length.out = n_dots))  # Equally spaced dots
  y_dots <- sapply(x_dots, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Add dots to the curve
  points(x_dots, y_dots, pch = 19, col = "black", cex = 4)
  
  # Close the pdf device
  dev.off()
}

# === === === === === === === === === === === === === === ===
# Set parameters for the 3 ND figures of Rootstock 3
# === === === === === === === === === === === === === === ===
parameters <- list(
  list(Tmin = -40, Topt = 20, Tmax = 40), # figure 1
  list(Tmin = -30, Topt = 15, Tmax = 40), # figure 2
  list(Tmin = -30, Topt = 25, Tmax = 40) # figure 3
)

for (i in 1:3) {
  # Extract parameters for this figure
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Create a unique filename for each figure
  filename <- paste0("rootstock3ND_", i, ".pdf")
  
  # Open a pdf device
  pdf(filename)
  
  # Define the continuous curve
  x <- seq(-40, 40, 0.01)  # Extend range to fit all curves
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Plot the continuous line
  plot(
    x, y, type = "l", lwd = 8, col = "#003a7d", lty = 2, xlab = "", ylab = "",
    main = "", bty = "l", xaxt = "n", yaxt = "n",
  )
  # keep ticks, remove their labels
  axis(1, labels = FALSE, tick = TRUE) 
  axis(2, labels = FALSE, tick = TRUE) 
  # Close the pdf device
  dev.off()
}


# === === === === === === === === === === === === === === ===
# Universal relationship
# === === === === === === === === === === === === === === ===\
# Adjust the spacing of axis labels and titles using mgp
par(mgp = c(1, -5, 0)) # Adjust the first value for axis title position

parameters <- list(
  list(Tmin = -40, Topt = 20, Tmax = 40), # UNIVERSAL
  list(Tmin = -40, Topt = 30, Tmax = 40), # curve rootstock 1
  list(Tmin = -40, Topt = 25, Tmax = 40), # curve rootstock 2
  list(Tmin = -40, Topt = 10, Tmax = 40) # curve rootstock 3
)
colors <- c("black", "#c701ff", "#c83034", "#003a7d")
line_styles <- c(1, 2, 2, 2) # Specify line styles for each curve
line_width <- c(3,2,2,2)
# Open a single PDF device
pdf("universalcurves.pdf", width = 6, height = 8)

# Define the continuous x range
x <- seq(-40, 40, 0.01)  # Extend range to fit all curves

# Set up the plot for multiple curves
plot(NULL, xlim = range(x), ylim = c(0, 1), xlab = "Environment", ylab = "Process (e.g. rate)", 
     main = "", bty = "l", xaxt = "n", yaxt = "n", mgp = c(2, 0.5, 0))  # Adjust label position

# Add x and y axes with ticks but no tick labels
axis(1, labels = FALSE, tick = TRUE) # x-axis with ticks but no tick labels
axis(2, labels = FALSE, tick = TRUE) # y-axis with ticks but no tick labels

# Loop through each parameter set to add curves to the same plot
for (i in 1:4) {
  # Extract parameters for this curve
  Tmin <- parameters[[i]]$Tmin
  Topt <- parameters[[i]]$Topt
  Tmax <- parameters[[i]]$Tmax
  
  # Calculate the curve
  y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
  
  # Add the curve to the plot with specified color and line style
  lines(x, y, lwd = line_width[i], lty = line_styles[i], col = colors[i])
}

# Close the PDF device
dev.off()

