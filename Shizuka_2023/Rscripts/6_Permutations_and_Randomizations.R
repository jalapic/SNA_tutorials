# Testing against the Null Hypothesis: Permutations and Randomizations
# there's not really coding in this one

# packages
library(asnipe)
library(igraph)
library(assortnet)

# Node-label Permutations - randomizes node values (sex, size, etc.) while keeping the same edges
# helpful for understanding how node attributes affect connectivity
# (ex: testing correlations between node attribute and network position or using as null model for assortativity)

# Edge permutations uses:
# Not necessarily testing for the roles of particular node types
# Testing whether structure of network is non-random… but what is random?
# Must be careful about exactly how we permute edges–do we want to preserve any aspect of the connectivity of nodes?

# Unconstrained edge permutations
# generate "random graphs" - can be made with a ‘Erdös-Renyí random graph’ or by ‘rewiring’ the network randomly

# Edge rewiring while keeping the degree distribution constant
# sees if the distribution of node degrees being non-random causes the clustering coefficient to be larger than expected by the random graph
# rewire() uses "switching" to do this
# switching example: 1 and 2 are connected, 3 and 4 are connected
# after switching: 1 and 4 are connected, 2 and 3 are connected
# switches as many times as the number of edges that are in the network

# Group membership permutation
# permutates group membership data (ex: group by individual matrix) so the group (ex: flock) of each individual is randomized
# "swap algorithm"/"flipping procedure":
# 1. Identify a 2x2 sub-matrix within the bipartite matrix
# 2. Swap the row/columns

# Using Randomizations for Hypothesis Testing
# randomization can be used to generate null models



