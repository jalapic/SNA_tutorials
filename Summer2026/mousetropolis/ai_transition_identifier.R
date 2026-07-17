library(dplyr)
library(readr)
library(igraph)
library(lubridate)

# uses the mousetropolis_labeled.jpeg image for box labels
build_layout <- function() {
  tibble::tribble(
    ~device_id, ~box_1, ~box_2,
    1, "FC1", "A",
    2, "A",   "B",
    3, "B",   "FC2",
    4, "A",   "D",
    5, "B",   "E",
    7, "C",   "D",
    8, "E",   "F",
    9, "C",   "G",
    16, "D",   "H",
    17, "E",   "I",
    18, "F",   "J",
    19, "G",   "H",
    20, "I",   "J",
    21, "H",   "K",
    22, "I",   "L",
    24, "FC3", "K",
    25, "K",   "L",
    32, "L",   "FC4"
  )
}

# function that outputs a table with each mouse's supposed transitions
check_mouse_transitions <- function(file,
                              layout = build_layout() #,
                              # what is a permissible crossing time?
                              # max_cross_ms = NA
                              ) {
  df <- readr::read_delim(file, delim = ";", show_col_types = FALSE)
  
  events <- df %>%
    # ensure all data is of correct type
    dplyr::transmute(
      datetimestamp = as.POSIXct(gsub("(\\d{2}):(\\d{3})$", "\\1.\\2", datetimestamp),
                                  format = "%d.%m.%Y %H:%M:%OS", tz = "America/Chicago"),
      device_id = as.integer(deviceid),
      antenna_id = as.integer(antennaID),
      mouse_id = as.character(data)
    ) %>%
    # order by mouse and datetimestamp
    dplyr::arrange(mouse_id, datetimestamp) %>%
    dplyr::left_join(layout, by = "device_id") %>%
    dplyr::group_by(mouse_id) %>%
    dplyr::mutate(
      # look to the next row
      next_datetimestamp = dplyr::lead(datetimestamp),
      next_device_id     = dplyr::lead(device_id),
      next_antenna_id    = dplyr::lead(antenna_id),
      
      next2_device_id  = dplyr::lead(device_id, 2),
      next2_antenna_id = dplyr::lead(antenna_id, 2),
      
      # determine crossing time
      #dt_ms = next_datetimestamp - datetimestamp,
      
      # determine if current and next row make up a true transition
      candidate_transition =
        !is.na(next_datetimestamp) &
        device_id == next_device_id &
        antenna_id != next_antenna_id #&
        #dt_ms >= 0 &
        #dt_ms <= max_cross_ms
        ,
      
      verify_transition =
        candidate_transition &
        (
          (next2_device_id == next_device_id & next2_antenna_id == next_antenna_id) |
            (!is.na(next2_device_id) & next2_device_id != next_device_id)
        ),
      
      from_box = dplyr::case_when(
        verify_transition & antenna_id == 1L & next_antenna_id == 2L ~ box_1,
        verify_transition & antenna_id == 2L & next_antenna_id == 1L ~ box_2,
        TRUE ~ NA_character_
      ),
      
      to_box = dplyr::case_when(
        verify_transition & antenna_id == 1L & next_antenna_id == 2L ~ box_2,
        verify_transition & antenna_id == 2L & next_antenna_id == 1L ~ box_1,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup()
  
  # make output table
  crossings <- events %>%
    dplyr::filter(verify_transition, !is.na(from_box), !is.na(to_box)) %>%
    dplyr::transmute(
      mouse_id,
      #cross_dt_ms = dt_ms,
      start_datetimestamp = datetimestamp,
      end_datetimestamp   = next_datetimestamp,
      device_id,
      from_antenna = antenna_id,
      to_antenna   = next_antenna_id,
      from_box,
      to_box
    ) %>%
    dplyr::arrange(mouse_id, start_datetimestamp)
  
  crossings %>%
    dplyr::group_by(mouse_id) %>%
    dplyr::mutate(
      # the below column is not entirely necessary
      next_from_box = dplyr::lead(from_box),
      # checks if mouse could realistically make next transition from
      # where it currently is - are devices connected by same box?
      suspicious_jump = dplyr::case_when(
        is.na(next_from_box) ~ FALSE,
        TRUE ~ next_from_box != to_box
      )
    ) %>%
    dplyr::ungroup()
}