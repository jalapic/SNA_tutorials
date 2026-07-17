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

# -------using function_isomorph_transitions.R------------
c1 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort1.csv")
c2 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort2.csv")
c3 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort3.csv")
c4 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort4.csv")
c5 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort5.csv")
c7 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort7.csv")
c8 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort8.csv")
c9 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort9.csv")
c10 <- readr::read_csv("Summer2026/cohort_mouse_data/cohort10.csv")

c_list <- list(c1, c2, c3, c4, c5, c7, c8, c9, c10)

c_dfs <- vector("list", length(c_list))

for (i in seq_along(c_list)) {
  c_dfs[[i]] <- get_class_seq(c_list[[i]])
}

#-------------loop to make transition matrices for all cohorts--------------
TM_list <- vector("list", length(c_dfs))

for (i in seq_along(c_dfs)) {
  TM_list[[i]] <- make_TM(c_dfs[[i]])
}

for (i in seq_along(TM_list)) {
  print(TM_list[[i]])
}

# turn TM counts to decimals for each cohort (no 6th cohort)
for (i in seq_along(TM_list)) {
  TM_list[[i]] <- TM_list[[i]] / rowSums(TM_list[[i]])
}

# cohort TMs do not include all possible network states (<56x56) - fill them in
full_TMs <- vector("list", length(TM_list))

for (i in seq_along(full_TMs)) {
  full_TMs[[i]] <- matrix(0, nrow = 56, ncol = 56)
  rownames(full_TMs[[i]]) <- colnames(full_TMs[[i]]) <- 
    as.character(1:56)
  full_TMs[[i]][rownames(TM_list[[i]]), colnames(TM_list[[i]])] <- 
    TM_list[[i]]
}

# for (i in seq_along(full_TMs)) {
#  print(full_TMs[[i]])
# }