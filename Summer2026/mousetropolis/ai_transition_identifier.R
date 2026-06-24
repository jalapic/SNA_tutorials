library(dplyr)
library(readr)
library(igraph)

# uses mousetropolis labeled image
build_layout <- function() {
  tibble::tribble(
    ~device_id, ~antenna_id, ~box_from, ~box_to,
    
    1, 1, "FC1", "A",
    1, 2, "A",   "FC1",
    
    2, 1, "A",   "B",
    2, 2, "B",   "A",
    
    3, 1, "B",   "FC2",
    3, 2, "FC2", "B",
    
    4, 1, "A",   "D",
    4, 2, "D",   "A",
    
    5, 1, "B",   "E",
    5, 2, "E",   "B",
    
    7, 1, "C",   "D",
    7, 2, "D",   "C",
    
    8, 1, "E",   "F",
    8, 2, "F",   "E",
    
    9, 1, "C",   "G",
    9, 2, "G",   "C",
    
    16, 1, "D",  "H",
    16, 2, "H",  "D",
    
    17, 1, "E",  "I",
    17, 2, "I",  "E",
    
    18, 1, "F",  "J",
    18, 2, "J",  "F",
    
    19, 1, "G",  "H",
    19, 2, "H",  "G",
    
    20, 1, "I",  "J",
    20, 2, "J",  "I",
    
    21, 1, "H",  "K",
    21, 2, "K",  "H",
    
    22, 1, "I",  "L",
    22, 2, "L",  "I",
    
    24, 1, "FC3", "K",
    24, 2, "K",   "FC3",
    
    25, 1, "K",   "L",
    25, 2, "L",   "K",
    
    32, 1, "L",   "FC4",
    32, 2, "FC4", "L"
  )
}

process_mouse_csv <- function(file,
                              layout = build_layout(),
                              max_cross_ms = 500000) {
  df <- readr::read_delim(file, delim = ";", show_col_types = FALSE)
  
  ev <- df %>%
    transmute(
      cantimestamp = as.numeric(cantimestamp),
      datetimestamp = datetimestamp,
      device_id = as.integer(deviceid),
      antenna_id = as.integer(antennaID),
      mouse_id = as.character(data)
    ) %>%
    arrange(mouse_id, cantimestamp) %>%
    left_join(layout, by = c("device_id", "antenna_id")) %>%
    group_by(mouse_id) %>%
    mutate(
      next_cantimestamp = lead(cantimestamp),
      next_device_id = lead(device_id),
      next_antenna_id = lead(antenna_id),
      next_box_from = lead(box_from),
      next_box_to = lead(box_to),
      dt_ms = next_cantimestamp - cantimestamp,
      true_transition = if_else(
        device_id == next_device_id &
          antenna_id != next_antenna_id &
          !is.na(dt_ms) &
          dt_ms >= 0 &
          dt_ms <= max_cross_ms,
        TRUE,
        NA
      )
    ) %>%
    ungroup()
  
  g <- graph_from_data_frame(
    layout %>% distinct(box_from, box_to) %>% rename(from = box_from, to = box_to),
    directed = TRUE
  )
  
  vertex_names <- V(g)$name
  
  reachable <- function(box_from, box_to) {
    if (is.na(box_from) || is.na(box_to)) return(FALSE)
    if (!(box_from %in% vertex_names)) return(FALSE)
    if (!(box_to %in% vertex_names)) return(FALSE)
    
    is.finite(distances(g, v = box_from, to = box_to, mode = "out")[1, 1])
  }
  
  ev %>%
    mutate(
      suspicious_jump = case_when(
        is.na(next_box_from) ~ FALSE,
        TRUE ~ !mapply(reachable, box_to, next_box_from)
      )
    ) %>%
    select(
      cantimestamp, datetimestamp, mouse_id,
      device_id, antenna_id, box_from, box_to,
      next_box_from, true_transition, suspicious_jump
    )
}