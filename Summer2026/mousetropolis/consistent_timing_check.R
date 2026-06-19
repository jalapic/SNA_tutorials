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

# import libraries


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

# make sure clock doesn't start each day

