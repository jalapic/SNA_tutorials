# import data
april18 <- read.csv("Summer2026/mousetropolis/data/rawdata20260418.csv", sep = ";")
april19 <- read.csv("Summer2026/mousetropolis/data/rawdata20260419.csv", sep = ";")
april20 <- read.csv("Summer2026/mousetropolis/data/rawdata20260420.csv", sep = ";")
april21 <- read.csv("Summer2026/mousetropolis/data/rawdata20260421.csv", sep = ";")
april22 <- read.csv("Summer2026/mousetropolis/data/rawdata20260422.csv", sep = ";")
april23 <- read.csv("Summer2026/mousetropolis/data/rawdata20260423.csv", sep = ";")
april24 <- read.csv("Summer2026/mousetropolis/data/rawdata20260424.csv", sep = ";")
april25 <- read.csv("Summer2026/mousetropolis/data/rawdata20260425.csv", sep = ";")
april26 <- read.csv("Summer2026/mousetropolis/data/rawdata20260426.csv", sep = ";")
april27 <- read.csv("Summer2026/mousetropolis/data/rawdata20260427.csv", sep = ";")
april28 <- read.csv("Summer2026/mousetropolis/data/rawdata20260428.csv", sep = ";")
april29 <- read.csv("Summer2026/mousetropolis/data/rawdata20260429.csv", sep = ";")
april30 <- read.csv("Summer2026/mousetropolis/data/rawdata20260430.csv", sep = ";")
may1 <- read.csv("Summer2026/mousetropolis/data/rawdata20260501.csv", sep = ";")
may2 <- read.csv("Summer2026/mousetropolis/data/rawdata20260502.csv", sep = ";")

track_movements <- function(df, mouse_id) {
  df$datetimestamp <- as.POSIXct(gsub("(\\d{2}):(\\d{3})$", "\\1.\\2", df$datetimestamp),
                                 format = "%d.%m.%Y %H:%M:%OS")
  
  # filter for this mouse and sort by time
  mouse <- df[df$data == mouse_id, ]
  mouse <- mouse[order(mouse$datetimestamp), ]
  
  # find rows where location changes
  location <- paste(mouse$deviceid, mouse$antennaID, sep = "-")
  transitions <- which(diff(as.numeric(as.factor(location))) != 0)
  
  cat("Movements for mouse", mouse_id, ":\n")
  for (i in transitions) {
    t_from <- format(mouse$datetimestamp[i],   "%H:%M:%S")
    t_to   <- format(mouse$datetimestamp[i+1], "%H:%M:%S")
    from   <- paste("device", mouse$deviceid[i],   "antenna", mouse$antennaID[i])
    to     <- paste("device", mouse$deviceid[i+1], "antenna", mouse$antennaID[i+1])
    mins   <- round(as.numeric(difftime(mouse$datetimestamp[i+1], mouse$datetimestamp[i], units = "mins")), 1)
    cat(t_from, "->", t_to, ":", from, "->", to, "(", mins, "mins )\n")
  }
}

all_data <- rbind(april18, april19, april20, april21, april22, april23, april24,
                  april25, april26, april27, april28, april29, april30, may1, may2)

track_movements(all_data, 900133000459667)
track_movements(all_data, 900263000641476)
track_movements(all_data, 900263000641471)
track_movements(all_data, 900263000641480)
track_movements(all_data, 900263000641477)
track_movements(all_data, 900263000641474)
track_movements(all_data, 900263000641478)
track_movements(all_data, 900263000641463)
track_movements(all_data, 900263000641479)
track_movements(all_data, 900263002586587)
track_movements(all_data, 900263002586586)
track_movements(all_data, 900263002586597)
track_movements(all_data, 900263002586594)
track_movements(all_data, 900263002586588)
track_movements(all_data, 900263002586592)
track_movements(all_data, 900263002586583)
track_movements(all_data, 900263002586584)
track_movements(all_data, 900263002586595)
track_movements(all_data, 900263002586581)


# visual representation of individual mouse movements

# import libraries
library(dplyr)
library(igraph)

# load data
all_true <- readRDS("Summer2026/mousetropolis/data/all_true.RDS")

# node positions matching the physical layout
node_coords <- data.frame(
  name = c("FC1", "A", "B", "FC2", "C", "D", "E", "F", "G", "H", "I", "J", "FC3", "K", "L", "FC4"),
  x    = c(1,      2,   3,   4,     1,   2,   3,   4,   1,   2,   3,   4,   1,     2,   3,   4),
  y    = c(4,      4,   4,   4,     3,   3,   3,   3,   2,   2,   2,   2,   1,     1,   1,   1)
)

# plot individual mouse movements
plot_mouse_movements <- function(mouse_id) {
  mouse_moves <- all_true[all_true$mouse_id == mouse_id, ]
  mouse_moves <- mouse_moves[order(as.POSIXct(mouse_moves$datetimestamp, format = "%d.%m.%Y %H:%M:%OS")), ]
  
  edge_counts <- mouse_moves %>%
    group_by(box_from, box_to) %>%
    summarise(count = n(), .groups = "drop")
  
  g <- graph_from_data_frame(edge_counts, directed = TRUE, vertices = node_coords)
  layout_mat <- as.matrix(node_coords[, c("x", "y")])
  
  # count transitions per box
  box_counts <- table(c(all_true$box_from, all_true$box_to))
  box_counts <- box_counts[V(g)$name]
  box_counts[is.na(box_counts)] <- 0
  
  # create color gradient from light to dark
  color_palette <- colorRampPalette(c("lightyellow", "darkorange", "darkred"))
  node_colors <- color_palette(100)[as.integer(cut(box_counts, breaks = 100))]
  
  plot(g,
       layout = layout_mat,
       edge.width = log(E(g)$count + 1),
       edge.arrow.size = 0.3,
       edge.label = E(g)$count,
       edge.label.cex = 1,
       edge.label.color = "black",
       edge.color = rgb(0.5, 0.5, 0.5, 0.3),
       vertex.color = node_colors,
       vertex.label = V(g)$name,
       vertex.label.cex = 0.8,
       vertex.size = 20,
       margin = 0.2,
       main = paste("Movements for mouse", mouse_id))
}

plot_mouse_movements("900263000641463")

# "900133000459667" "900263000641463" "900263000641471" "900263000641474" "900263000641476"
# "900263000641477" "900263000641478" "900263000641479" "900263000641480" "900263002586583"
# "900263002586584" "900263002586586" "900263002586587" "900263002586588" "900263002586592"
# "900263002586594" "900263002586597"


## LOTS OF GRAPHS INCOMING ##
# individual mouse plotting activity
library(ggplot2)
library(tidyr)

# prep data once
activity <- all_true %>%
  mutate(
    dt   = as.POSIXct(start_datetimestamp, format = "%d.%m.%Y %H:%M:%OS"),
    hour = as.integer(format(dt, "%H")),
    date = as.Date(dt)
  ) %>%
  filter(!(date == as.Date("2026-05-02") & hour >= 18)) %>%
  group_by(mouse_id, date, hour) %>%
  summarise(transitions = n(), .groups = "drop") %>%
  complete(mouse_id, date, hour = 0:23, fill = list(transitions = 0))

mouse_ids <- unique(activity$mouse_id)

for (mid in mouse_ids) {
  mouse_data <- activity %>% filter(mouse_id == mid)
  
  p <- ggplot(mouse_data, aes(x = hour, y = transitions)) +
    geom_line(color = "steelblue") +
    geom_point(size = 1.5, color = "steelblue") +
    facet_wrap(~date, ncol = 3) +
    scale_x_continuous(breaks = seq(0, 23, by = 3), labels = paste0(seq(0, 23, by = 3), "h")) +
    labs(
      title = paste("24hr Activity — Mouse", mid),
      x = "Hour of Day",
      y = "Transitions per Hour"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      strip.text = element_text(size = 8)
    )
  
  print(p)
}

# stacked mice graphs with all mice colored

dates <- unique(activity$date)

for (d in dates) {
  day_data <- activity %>% filter(date == d)
  
  p <- ggplot(day_data, aes(x = hour, y = transitions, color = factor(mouse_id), group = factor(mouse_id))) +
    geom_line(alpha = 0.6) +
    geom_point(size = 1.5, alpha = 0.6) +
    scale_x_continuous(breaks = seq(0, 23, by = 3), labels = paste0(seq(0, 23, by = 3), "h")) +
    labs(
      title = paste("24hr Activity —", format(as.Date(d, origin = "1970-01-01"), "%B %d, %Y")),
      x = "Hour of Day",
      y = "Transitions per Hour",
      color = "Mouse ID"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  
  print(p)
}


# stacked mice with most movement colored
dates <- unique(activity$date)

for (d in dates) {
  day_data <- activity %>% 
    filter(date == d) %>%
    group_by(mouse_id) %>%
    mutate(total = sum(transitions)) %>%
    ungroup() %>%
    mutate(top_mouse = mouse_id == mouse_id[which.max(total)])
  
  p <- ggplot(day_data, aes(x = hour, y = transitions, group = factor(mouse_id), color = top_mouse)) +
    geom_line(alpha = 0.6) +
    geom_point(size = 1.5, alpha = 0.6) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray70"),
                       labels = c("TRUE" = paste("Most active:", day_data$mouse_id[which.max(day_data$total)]),
                                  "FALSE" = "Other mice")) +
    scale_x_continuous(breaks = seq(0, 23, by = 3), labels = paste0(seq(0, 23, by = 3), "h")) +
    labs(
      title = paste("24hr Activity —", format(as.Date(d, origin = "1970-01-01"), "%B %d, %Y")),
      x = "Hour of Day",
      y = "Transitions per Hour",
      color = ""
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  
  print(p)
}

