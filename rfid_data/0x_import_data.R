### Loading in an example RFID dataset

library(tidyverse)

c7 <- read_csv("rfid_data/cohort7.csv")

head(c7)

#make sure data is ordered by time (value 1 column)
c7 <- c7 %>% arrange(value1)


# Data are now in time ordered sequence.
head(c7)


# the winner loser columns are #vector1 (winner) and #vector2 (loser)

table(c7$vector1)
table(c7$vector2)

# this just shows that there is a hierarchy
mat <- compete::get_wl_matrix(as.matrix(c7[,4:5]))
mat
isi.res <- compete::isi13(mat)
isi.res
compete::get_di_matrix(mat)[isi.res$best_order,isi.res$best_order]


head(c7)
