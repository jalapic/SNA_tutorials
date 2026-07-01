# code to make sure the timing is consistent

# import data
april18 <- read.csv("Summer2026/mousetropolis/data/rawdata20260418.csv", sep = ";")
april19 <- read.csv("Summer2026/mousetropolis/data/rawdata20260419.csv", sep = ";")
april20 <- read.csv("Summer2026/mousetropolis/data/rawdata20260420.csv", sep = ";")
april21 <- read.csv("Summer2026/mousetropolis/data/rawdata20260421.csv", sep = ";")
april22 <- read.csv("Summer2026/mousetropolis/data/rawdata20260422.csv", sep = ";")
april23 <- read.csv("Summer2026/mousetropolis/data/rawdata20260423.csv", sep = ";")
april24 <- read.csv("Summer2026/mousetropolis/data/rawdata20260424.csv", sep = ";")
april25 <- read.csv("Summer2026/mousetropolis/data/rawdata20260425.csv", sep = ";")
april26 <- read.csv("Summer2026/mousetropolis/data/rawdata20260426.csv", sep = ";")
april27 <- read.csv("Summer2026/mousetropolis/data/rawdata20260427.csv", sep = ";")
april28 <- read.csv("Summer2026/mousetropolis/data/rawdata20260428.csv", sep = ";")
april29 <- read.csv("Summer2026/mousetropolis/data/rawdata20260429.csv", sep = ";")
april30 <- read.csv("Summer2026/mousetropolis/data/rawdata20260430.csv", sep = ";")
may1 <- read.csv("Summer2026/mousetropolis/data/rawdata20260501.csv", sep = ";")
may2 <- read.csv("Summer2026/mousetropolis/data/rawdata20260502.csv", sep = ";")

# save all days as one csv
all_data <- rbind(april18, april19, april20, april21, april22, april23, april24,
                  april25, april26, april27, april28, april29, april30, may1, may2)

write.csv(all_data, "Summer2026/mousetropolis/data/all_data.csv", row.names = FALSE)


# making sure clock is tracking for entire time
check_gaps <- function(df, name, threshold = 9000000) {
  df$datetimestamp <- as.POSIXct(gsub("(\\d{2}):(\\d{3})$", "\\1.\\2", df$datetimestamp),
                                  format = "%d.%m.%Y %H:%M:%OS")
  df <- df[order(df$datetimestamp), ]
  gaps <- diff(as.numeric(df$datetimestamp) * 1000)
  large_gaps <- which(gaps > threshold)

  if (length(large_gaps) == 0) {
    cat(name, ": no large gaps found\n")
  } else {
    cat(name, ":", length(large_gaps), "gaps found\n")
    for(i in large_gaps) {
      cat("Gap of", gaps[i], "ms between rows", i, "and", i+1, "\n")
      print(df[c(i, i+1), ])
    }
  }
}

check_gaps(april18, "april18")
check_gaps(april19, "april19")
check_gaps(april20, "april20")
check_gaps(april21, "april21")
check_gaps(april22, "april22")
check_gaps(april23, "april23")
check_gaps(april24, "april24")
check_gaps(april25, "april25")
check_gaps(april26, "april26")
check_gaps(april27, "april27")
check_gaps(april28, "april28")
check_gaps(april29, "april29")
check_gaps(april30, "april30")
check_gaps(may1, "may1")
check_gaps(may2, "may2")
check_gaps(all_data, "all_data")

# simplified version for quick reference
summarize_gaps <- function(df, name, threshold = 18000) {
  df$datetimestamp <- as.POSIXct(gsub("(\\d{2}):(\\d{3})$", "\\1.\\2", df$datetimestamp),
                                 format = "%d.%m.%Y %H:%M:%OS")
  df <- df[order(df$datetimestamp), ]
  gaps <- diff(as.numeric(df$datetimestamp) * 1000)
  large_gaps <- which(gaps > threshold)
  
  if (length(large_gaps) == 0) {
    cat(name, ": no large gaps found\n")
  } else {
    cat(name, ":", length(large_gaps), "gaps found\n")
    for (i in large_gaps) {
      t1 <- format(df$datetimestamp[i],   "%H:%M")
      t2 <- format(df$datetimestamp[i+1], "%H:%M")
      mins <- round(gaps[i] / 60000, 1)
      cat(t1, "->", t2, "--", mins, "minute gap\n")
    }
  }
}

summarize_gaps(april18, "april18")
summarize_gaps(april19, "april19")
summarize_gaps(april20, "april20")
summarize_gaps(april21, "april21")
summarize_gaps(april22, "april22")
summarize_gaps(april23, "april23")
summarize_gaps(april24, "april24")
summarize_gaps(april25, "april25")
summarize_gaps(april26, "april26")
summarize_gaps(april27, "april27")
summarize_gaps(april28, "april28")
summarize_gaps(april29, "april29")
summarize_gaps(april30, "april30")
summarize_gaps(may1, "may1")
summarize_gaps(may2, "may2")
summarize_gaps(all_data, "all_data")

