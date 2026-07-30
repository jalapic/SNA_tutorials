library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

df <- readRDS("Summer2026/mousetropolis/data/all_true_crossings.rds")
id_key <- read.csv("Summer2026/mousetropolis/data/mousemetRD1_ids.csv", stringsAsFactors = FALSE) %>%
  mutate(TagID = as.character(TagID))

df2 <- df %>%
  mutate(
    mouse_id = as.character(mouse_id),
    start_datetimestamp = as.POSIXct(start_datetimestamp, tz = "UTC"),
    end_datetimestamp = as.POSIXct(end_datetimestamp, tz = "UTC")
  ) %>%
  left_join(id_key, by = c("mouse_id" = "TagID")) %>%
  filter(!is.na(TempID)) %>%
  arrange(TempID, start_datetimestamp)

sample_times <- seq(
  floor_date(min(df2$start_datetimestamp, na.rm = TRUE), unit = "10 minutes"),
  ceiling_date(max(df2$end_datetimestamp, na.rm = TRUE), unit = "10 minutes"),
  by = "10 mins"
)

mouse_list <- split(df2, df2$TempID)

make_mouse_time <- function(dat, sample_times) {
  dat <- dat %>% arrange(start_datetimestamp)
  
  grid <- tibble(sampled_time = sample_times)
  
  out <- grid %>%
    cross_join(dat %>% select(start_datetimestamp, to_box)) %>%
    filter(sampled_time >= start_datetimestamp) %>%
    group_by(sampled_time) %>%
    slice_max(start_datetimestamp, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(sampled_time, box = to_box)
  
  grid %>%
    left_join(out, by = "sampled_time") %>%
    arrange(sampled_time) %>%
    tidyr::fill(box, .direction = "down")
}

long_boxes <- purrr::imap_dfr(mouse_list, function(dat, tempid) {
  make_mouse_time(dat, sample_times) %>%
    mutate(TempID = tempid)
})

wide_boxes <- long_boxes %>%
  pivot_wider(names_from = TempID, values_from = box) %>%
  arrange(sampled_time)

saveRDS(wide_boxes, "Summer2026/mousetropolis/data/mice-location_ten-min-interval.rds")

View(wide_boxes)
