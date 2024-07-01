### Writing a Function in R.


library(babynames)
babynames

# get_person <- function() {
#   
# }



get_person <- function(df) {
  return(df1)
}


get_person()
get_person(df = babynames)



get_person <- function(df, pname="Michelle") {
  df1 <- df %>% filter(name==pname)
  return(df1)
}

get_person(df=babynames)
get_person(df=babynames, pname="Seymour")


get_person <- function(df, pname=NULL) {
  df1 <- df %>% filter(name==pname)
  return(df1)
}


get_person(df=babynames, pname="Seymour")
get_person(df=babynames, pname="Michelle")



plot_person <- function(df, pname=NULL) {
  p <- df %>% 
    filter(name==pname) %>%
    ggplot(., aes(x=year, y=n, color=sex)) +
    geom_line() +
    theme_classic() +
    ggtitle(pname)
  return(p)
}

plot_person(df = babynames, pname="Michelle")
plot_person(df = babynames, pname="Skylar")
plot_person(df = babynames, pname="Avery")
plot_person(df = babynames, pname="Jamie")




####  return lots of things.

g <- igraph::erdos.renyi.game(n=10,p=.3, directed=T)

get_tc_info <- function(g) {
  
  out <- triad_census(g)
  out1 <- out[[9]]/ sum(out)
  out2 <- out[[10]]/ sum(out)
  
  return(list(out1,out2))
}




get_tc_info <- function(g) {
  
  out <- triad_census(g)
  out1 <- out[[9]]/ sum(out)
  out2 <- out[[10]]/ sum(out)
  
  return(list('proportion of transitive =' = out1,
              'proportion of intransitive =' = out2))
}


get_tc_info(g)
