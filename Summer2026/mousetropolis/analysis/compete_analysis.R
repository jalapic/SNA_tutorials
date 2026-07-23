# load in all following pairs
following_pairs <- readRDS('Summer2026/mousetropolis/data/following_pairs.RDS')

# run packages if not already
library(compete)
library(dplyr)
library(splitstackshape)
library(data.table)

# clean table so compete functions can be applied
following_pairs_renamed <- following_pairs %>%
  rename(Recipient = leader, Actor = follower, time_recipient_readable = time_leader_readable) %>%
  select(Recipient, Actor, time_recipient_readable)

# adds a column ‘score’ with a 1 indicating a clear win for the Actor vs the Recipient, 
# and a 0.5 indicating a tie
df1 <- expandrows(following_pairs_renamed)

# preliminary raw sociomatrix
xtabs(~ Actor + Recipient, df1)

# Create sociomatrices
wldf <- df1[score==1][, c(2,1), with = FALSE] #data.table indexing
head(wldf)

# raw frequency sociomatrix of wins and losses
wlmat <- get_wl_matrix(wldf)
wlmat

# heirarchy measures
rshps(wlmat)

# directional consistency
dc_test(wlmat)

# modified h
devries(wlmat)

# plotting modified h
devries(wlmat, plot=T)

#triangular Transitivity
ttri_test(wlmat)

# David's score
ds(wlmat)

# plot david's score steepness
plot(1:12, rev(sort(ds(wlmat))), "l",
     xlab = "Rank",
     ylab = "David's Score",
     main = "David's Scores by Rank")
abline(h = 0, col = "red", lty = 3)
