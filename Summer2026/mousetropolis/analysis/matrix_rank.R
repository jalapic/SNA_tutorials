library(compete)
library(splitstackshape)
library(data.table)

pair_data <- readRDS('Summer2026/mousetropolis/data/following_pairs.RDS')

ids <- read.csv("Summer2026/mousetropolis/data/mousemetRD1_ids.csv", 
                colClasses = c("character", "character", "numeric"))

pairs_temp <- pair_data %>%
  left_join(ids, by = c("leader" = "TagID")) %>%
  rename(recipient_temp = NumID) %>%
  left_join(ids, by = c("follower" = "TagID")) %>%
  rename(actor_temp = NumID)

datetime_pairs <- pairs_temp %>% 
  select(Timestamp = time_leader_readable,
         Actor = actor_temp,
         Recipient = recipient_temp)

pairs_exp <- expandrows(datetime_pairs)

# create sociomatrix/raw aggregated matrix
wldf <- pairs_exp[score==1][, c(2,3), with = FALSE] #data.table indexing
head(wldf)

wlmat <- get_wl_matrix(wldf)
wlmat

# rename winner-loser matrix row-column labels
id_map <- setNames(ids$TempID, ids$NumID)

rownames(wlmat) <- id_map[as.character(rownames(wlmat))]
colnames(wlmat) <- id_map[as.character(colnames(wlmat))]

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
