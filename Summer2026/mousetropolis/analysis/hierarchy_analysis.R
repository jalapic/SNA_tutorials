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

# hierarchy measures
compete::org_matrix(wlmat, method='ds')

wlmat2<- wlmat
rownames(wlmat2)<-colnames(wlmat2)<-LETTERS[1:16]
wlmat2

wlmat2b <- compete::get_di_matrix(wlmat2)
wlmat2b
org_matrix(wlmat2b, method='ds')

isi98(wlmat2b)

rshps(wlmat)

# directional consistency
dc_test(wlmat)

# modified h
devries(wlmat)

# plotting modified h
devries(wlmat, plot=T)

# triangular transitivity
ttri_test(wlmat)

# David's score
ds(wlmat)

# plot david's score steepness
plot(1:16, rev(sort(ds(wlmat))), "l",
     xlab = "Rank",
     ylab = "David's Score",
     main = "David's Scores by Rank")
abline(h = 0, col = "red", lty = 3)

# i&si
isi.out <-  isi98(wlmat)

# wins made by each animal
despotism(wlmat)

# Color Sociomatrix
matrixplot(wlmat, mylevs=isi.out$best_order)

# quickly visualizing the inconsistencies in the hierarchy
matrixplot0(wlmat, mylevs=isi.out$best_order)

# checking for inconsistencies
contests(df1,"K","C")

# network certainty
library(Perc)
obsmat <- as.conflictmat(wldf)
DominanceProbability.obs <- conductance(obsmat, maxLength = 2)
s.rank.obs <- simRankOrder(DominanceProbability.obs$p.hat, num = 10, kmax = 10)
dfobs <- merge(individualDomProb(DominanceProbability.obs$p.hat), s.rank.obs$BestSimulatedRankOrder)
plot(dfobs$ranking, dfobs$Mean,
     xlab="Rank", ylab="Dominance Certainty")

# GLIKO
library(PlayerRatings)
df1 <- df1[order(df1$Timestamp),] #ensure in date order
df1$event <- 1:nrow(df1)
glick.df <- df1[, c(11,2,4,10), with = FALSE] #need event, actor, recipient, score
gl <- glicko(glick.df, history=T, cval=2)
gl