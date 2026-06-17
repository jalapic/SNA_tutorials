# code to make sure the timing is consistent

# import data

# steps
# 1. download files
# 2. make sure clock doesn't restart each day
# 3. put into one big thing
# 4. order data by milisecond time
# 5. then we have our new csv
# 6. look for our gaps (missing mice, jumping mice, missing antennas)
# 7. identify transitions for each cage for each mouse
# 8. determine which mice are following each other (within 250 miliseconds of each other)

# import library
library(here)

# import data
april18 <- read.csv("data/rawdata20260418.csv")
