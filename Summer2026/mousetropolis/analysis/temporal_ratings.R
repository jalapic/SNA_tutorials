library(tidyverse)
library(compete)
library(PlayerRatings)

pairs <- readRDS('Summer2026/mousetropolis/data/following_pairs.RDS')

ids <- read.csv("Summer2026/mousetropolis/data/mousemetRD1_ids.csv", 
                colClasses = c("character", "character"))

pairs_temp <- pairs %>%
  left_join(ids, by = c("leader" = "TagID")) %>%
  rename(recipient_temp = TempID) %>%
  left_join(ids, by = c("follower" = "TagID")) %>%
  rename(actor_temp = TempID)

clean_pairs <- pairs_temp %>% 
  select(datetime = time_leader_readable,
         actor = actor_temp,
         recipient = recipient_temp)

clean_pairs$score <- 1

clean_pairs <- clean_pairs[order(clean_pairs$datetime),] #ensure in date order
clean_pairs$event <- 1:nrow(clean_pairs)
glick.df <- clean_pairs[, c(5,2,3,4)] #need event, actor, recipient, score
gl <- glicko(glick.df, history=T, cval=2)
gl

plot(gl,npl=16)

# run compete_extra_copy.R in order to run this
plotglicko(glick.df, cval=2, ylim1=1500, ylim2=3000, 
           thetitle="Glicko Ratings over Time", linewd=.5)

ggplot(gl$ratings, aes(x=1:16, y=Rating)) + 
  geom_point(size=2) + 
  scale_x_continuous(breaks=1:16, labels=gl$ratings$Player) +
  theme_classic() +
  geom_errorbar(aes(ymin=Rating-Deviation, ymax=Rating+Deviation),
                width=.2,                    
                position=position_dodge(.9),
                size=.5) +
  geom_hline(yintercept=2200, color='red', linetype='dotted') +
  ylab("Glicko Rating (Mean ± SD)") +
  xlab("Mouse ID") +
  ggtitle("Final Glicko Ratings")
