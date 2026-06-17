# list from example_import_data_mouse
classes

# need ggplot2
library(tidyverse)

# turn list into dataframe and remove NA datapoints
df_classes <- tibble(classes)

View(df_classes)

df_classes <- df_classes %>% filter(!is.na(classes))

# graph timeline of network states
ggplot(df_classes, aes(x = 1:nrow(df_classes), y = classes)) +
  geom_point(size = .5) +
  ggtitle('Cohort 7 Isomorph Timeline') +
  xlab('Nth Interaction') +
  ylab('Isomorph Number') +
  theme_bw() +
  scale_y_continuous(minor_breaks = seq(1, 56, by = 1))
