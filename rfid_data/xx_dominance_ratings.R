
### Calculating Dominance.


c10x <- read_csv("rfid_data/cohort10.csv",
                col_types = cols(vector1 = col_character(), 
                                 vector2 = col_character())) %>% 
  arrange(value1)


head(c10x)

# https://github.com/jalapic/compete
library(compete) #get from GitHub


# organize winner loser column into a WL matrix

mat <- get_wl_matrix(as.data.frame(c10x[,4:5]))
mat

# organize winner loser matrix by David Scores
ds(mat)
mat1 <- org_matrix(mat, method="ds")

#make binary
get_di_matrix(mat1)


## best method for rating over whole period (aggregated data) is called ISI
besto <- isi13(mat)$best_order
mat2 <- mat[besto,besto]
get_di_matrix(mat2)

besto 
ds(mat)




### Glicko ratings change over time.

library(PlayerRatings)
c10x.gl <- data.frame(
  time = 1:nrow(c10x),
  as.data.frame(c10x[,4:5]),
  score = 1
)
  


c10x.gl.out <- glicko(c10x.gl, history = T, cval=2)
c10x.gl.out
plot(c10x.gl.out,npl=6)





