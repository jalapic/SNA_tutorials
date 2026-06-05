
#--------------------------- RANDOM WALKS ----------------------- #

# this is in brief - look at other R file for more code,
# also other file has better plotting code

# the only benefit to the below is that it slightly might have a better
# way of randomizing than I came up with
#
# but my file has more info 

Nperms <- 10000  #this should probably be much,much,much bigger

reps6  # this is the list of 56 isomorphs

# Random-walk transitions for n=6
idx6 <- build_canon_index(reps6)
path <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)
TM <- create_transition_matrix(path, num_states = length(reps6))

print(TM[1:10, 1:10])
