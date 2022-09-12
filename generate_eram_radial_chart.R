
# Working script to run the ERAM visuals code. Call this code from command line, e.g.:
# "C:\Program Files\R\R-4.2.1\bin\Rscript.exe" generate_eram_radial_chart.R
# C:\Users\taner\OneDrive - Stichting Deltares\_WS\ERAMVisuals
# Install requred R packages (if not already installed)
if(!require(scales)) {install.packages(scales)}
if(!require(ggplot2)) {install.packages(ggplot2)}
if(!require(dplyr)) {install.packages(dplyr)}
if(!require(readr)) {install.packages(readr)}

# R function to create the Radial plot
source("ERAMRadialPlot.R")

# Generate radial plot based on the data
p <- ERAMRadialPlot(data.to.plot = readr::read_csv("sample_data.csv"))

# Save plot to pdf
ggplot2::ggsave(filename = "radialplot.pdf", plot = p, width = 8, height = 8)
