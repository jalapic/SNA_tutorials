# Intro to Network Regression (MRQAP)
# predict the effect of some relational variable on network relations–e.g., how the similarity between two nodes affects the probability of an edge, or how network relations predict the transmission of pathogens
# MRQAP allows you to determine the influence of one matrix on another, controlling for the effects of one or more covariate matrices

# load packages
library(sna) 
library(asnipe)

#generate 3 random adjacency matrices using the rgraph() function within sna
set.seed(2)
m1=rgraph(10, m=1, tprob=0.5, mode="graph")
m2=rgraph(10, m=1, tprob=0.5, mode="graph") 
m3=rgraph(10, m=1, tprob=0.5, mode="graph")
#test the effect of m2 on m1, controlling for m3. sna package function.

netlm(m1, m2+m3, mode="graph", nullhyp="qap", test.statistic="t-value")

#test the effect of m2 on m1 controlling for m3, and effect of m3 on m1, controlling for m2. asnipe package function.
mrqap.dsp(m1~m2+m3, directed="undirected")
