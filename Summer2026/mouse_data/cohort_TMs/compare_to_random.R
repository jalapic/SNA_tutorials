# turn TM counts to decimals for each cohort (no 6th cohort)
for (i in seq_along(TM_list)) {
  TM_list[[i]] <- TM_list[[i]] / rowSums(TM_list[[i]])
}

# cohort TMs do not include all possible network states (<56x56) - fill them in
full_TM_list <- vector("list", length(TM_list))

for (i in seq_along(full_TM_list)) {
  full_TM_list[[i]] <- matrix(0, nrow = 56, ncol = 56)
  rownames(full_TM_list[[i]]) <- colnames(full_TM_list[[i]]) <- 
    as.character(1:56)
  full_TM_list[[i]][rownames(TM_list[[i]]), colnames(TM_list[[i]])] <- 
    TM_list[[i]]
}

# compare each cohort TM to the random walk TM
for (i in seq_along(full_TM_list)) {
  # figure out how to compare cohort TMs to random walk TM (TM6) here
}