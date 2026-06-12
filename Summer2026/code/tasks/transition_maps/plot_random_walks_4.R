library(ggplot2)
library(reshape2)  # For melting matrix into long format

# Convert matrix into long format for ggplot
TM4_long <- melt(TM4)
colnames(TM4_long) <- c("Row", "Column", "Value")

# Create the tile plot
ggplot(TM4_long, aes(x = Column, y = Row, fill = Value)) +
  geom_tile(color = "black") +  # Add tile borders
  scale_fill_gradientn(colours= c(
    "#FFFFFF",  # 0 (White)
    "#F1E7F4",  # 1 (Very Very Light Purple)
    "#EAD8EC",  # 2 (Very Light Purple)
    "#D4B9DA",  # 3 (Light Purple)
    "#C994C7",  # 4 (Medium-Light Purple)
    "#E67DBD",  # 5 (Light Medium Purple)
    "#DF65B0",  # 6 (Medium Purple)
    "#DD1C77",  # 7 (Dark Pinkish Purple)
    "#C51067",  # 8 (Deep Pink Purple)
    "#980043",  # 9 (Deep Dark Purple)
    "#7A002F",  # 10 (Very Deep Purple)
    "#67001F"   # 11 (Almost Black Purple)
  )
  ) +
  labs(title = "4x4 Transition Matrix Heatmap",
       x = "Column",
       y = "Row",
       fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_y_reverse(breaks=1:4) +
  scale_x_continuous(breaks=1:4)
