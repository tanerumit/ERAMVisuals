
# Check if packages are installed, if not install them
if(!require(scales)) {install.packages(scales)}
if(!require(ggplot2)) {install.packages(ggplot2)}
if(!require(dplyr)) {install.packages(dplyr)}
if(!require(ggsci)) {install.packages(ggsci)}


# Function to create the radial plot
ERAMRadialPlot <- function(data, empty_bar = 2,
  ymin = -60, ymax = 140, label_size = 2) {

  require(scales)
  require(ggplot2)
  require(dplyr)
  require(ggsci)

  # Scale values between 20 and 80
  data <- data %>%
    mutate(group = factor(group, levels = c("P", "I", "C", "R", "E"))) %>%
    arrange(group) %>%
    mutate(value = scales::rescale(value, to=c(20,80)))

  # Set a number of 'empty bar' to add at the end of each group
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data))

  # Data frame to store the labels
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)

  # Data frame to store base lines
  base_data <- data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))

  # Data frame to store grids
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +

    # Set main bars
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.8) +

    # Set segments
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80),
      colour = "grey50", alpha=1, size=0.5 , inherit.aes = F) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60),
      colour = "grey50", alpha=1, size=0.5 , inherit.aes = F) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40),
      colour = "grey50", alpha=1, size=0.5 , inherit.aes = F) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20),
      colour = "grey50", alpha=1, size=0.5 , inherit.aes = F) +

    # Set yaxis scales
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80),
      label = c("1", "2", "3", "4") , color="grey50", size=5 ,
      angle=0, fontface="bold", hjust=1) +

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(ymin,ymax) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm")
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=stringr::str_wrap(individual, 20),
      lineheight = .75, hjust=hjust),
      color="black", fontface="bold",alpha=0.6, size=label_size,
      angle=label_data$angle, inherit.aes = FALSE) +

    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5),
      colour = "black", alpha=0.8, size=1 , inherit.aes = FALSE )  +

    geom_text(data=base_data, aes(x = title, y = -20, label=group),
      colour = "black", alpha=1, size=7, fontface="bold", inherit.aes = F) +

    # Fill color
    scale_fill_locuszoom()

    return(p)
}

# Create plots & save to png/pdf
p <- ERAMRadialPlot(data = readr::read_csv("./sample_data.csv"))
ggsave(filename = "radialplot.png", plot = p, width = 8, height = 8)
ggsave(filename = "radialplot.pdf", plot = p)
