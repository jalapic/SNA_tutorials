
#--------------------------- RANDOM WALKS ----------------------- #

# this is in brief - look at other R file for more code,
# also other file has better plotting code

# the only benefit to the below is that it slightly might have a better
# way of randomizing than I came up with
#
# but my file has more info 

Nperms <- 10000  #this should probably be much,much,much bigger

reps4  # list of 4 isomorphs
reps5  # list of 12 isomorphs
reps6  # list of 56 isomorphs

# ------------------Random-walk transitions for n=4-----------------
idx4 <- build_canon_index(reps4)
path4 <- simulate_tournament_transitions(N = Nperms, reps = reps4, 
                                         index_map = idx4)

# bounds error: adjusted with AI
states4 <- sort(unique(path4))
path_idx4 <- match(path4, states4)
TM4 <- create_transition_matrix(path_idx4, num_states = length(states4))
# TM4 <- create_transition_matrix(path, num_states = length(reps4))

print(TM4[1:4, 1:4])

# ------------------Random-walk transitions for n=5-----------------
idx5 <- build_canon_index(reps5)
path5 <- simulate_tournament_transitions(N = Nperms, reps = reps5, 
                                         index_map = idx5)

# bounds error: adjusted with AI
states5 <- sort(unique(path5))
path_idx5 <- match(path5, states5)
TM5 <- create_transition_matrix(path_idx5, num_states = length(states5))
#TM5 <- create_transition_matrix(path, num_states = length(reps5))

print(TM5[1:10, 1:10])

# ------------------Random-walk transitions for n=6-----------------
idx6 <- build_canon_index(reps6)
path6 <- simulate_tournament_transitions(N = Nperms, reps = reps6, index_map = idx6)
TM6 <- create_transition_matrix(path, num_states = length(reps6))

print(TM6[1:10, 1:10])
