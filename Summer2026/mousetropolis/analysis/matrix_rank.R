library(compete)
library(splitstackshape)
library(data.table)

pair_data <- readRDS('Summer2026/mousetropolis/data/following_pairs.RDS')

datetime_pairs <- pair_data %>% 
  select(Timestamp = time_leader_readable,
         Actor = follower,
         Recipient = leader)

pairs_exp <- expandrows(datetime_pairs)

# create sociomatrix/raw aggregated matrix
wldf <- pairs_exp[score==1][, c(2,3), with = FALSE] #data.table indexing
head(wldf)

wlmat <- get_wl_matrix(wldf)
wlmat

# organize matrix by David's Scores method
org_matrix(wlmat, method="ds")

# binarize matrix
bimat <- get_di_matrix(wlmat)
org_matrix(bimat, method="ds")

# view David's Scores
ds(wlmat)

plot(1:16, rev(sort(ds(wlmat))), "l",
     xlab = "Rank",
     ylab = "David's Score",
     main = "David's Scores by Rank")
abline(h = 0, col = "red", lty = 3)

# use I&SI ranking method
isi.out <-  isi98(wlmat)

isi.out

# colored sociomatrix
matrixplot(wlmat, mylevs=isi.out$best_order)

matrixplot0(wlmat, mylevs=isi.out$best_order)
