# one-time(ish) preprocessing: replace the long RFID tag numbers in the raw
# antenna-read exports with their short letter labels (A-P) from
# mousemetRD1_ids.csv, and write the result to data/cleaned/.
# tags not present in mousemetRD1_ids.csv (stray/unknown reads) are left
# as their original long ID so no rows are dropped.
#
# run this whenever new rawdataYYYYMMDD.csv files are added; every other
# script should read from data/cleaned/ instead of data/ so the long IDs
# never need to be looked up again downstream.

library(dplyr)
library(readr)

raw_dir     <- "Summer2026/mousetropolis/data"
cleaned_dir <- file.path(raw_dir, "cleaned")
dir.create(cleaned_dir, showWarnings = FALSE)

ids <- read_csv(file.path(raw_dir, "mousemetRD1_ids.csv"), col_types = "ccc") %>%
  select(TagID, TempID)

raw_files <- list.files(raw_dir, pattern = "^rawdata\\d{8}\\.csv$", full.names = TRUE)

for (f in raw_files) {
  df <- read_delim(f, delim = ";", col_types = cols(.default = "c"))

  df <- df %>%
    left_join(ids, by = c("data" = "TagID")) %>%
    mutate(data = coalesce(TempID, data)) %>%
    select(-TempID)

  out_path <- file.path(cleaned_dir, basename(f))
  write_delim(df, out_path, delim = ";")
  cat("wrote", out_path, "\n")
}
