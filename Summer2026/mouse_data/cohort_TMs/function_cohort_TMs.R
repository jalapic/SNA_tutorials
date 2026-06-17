#-----------------function for making transition matrices-------------------
make_TM <- function(df) {
  x <- df[[1]]
  states <- sort(unique(x))
  m <- table(
    factor(x[-length(x)], levels = states),
    factor(x[-1], levels = states)
  )
  as.matrix(m)
}

#-------------loop to make transition matrices for all cohorts--------------
TM_list <- vector("list", length(c_dfs))

for (i in seq_along(c_dfs)) {
  TM_list[[i]] <- make_TM(c_dfs[[i]])
}

for (i in seq_along(TM_list)) {
  print(TM_list[[i]])
}
