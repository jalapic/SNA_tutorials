track_movements <- function(df, mouse_id) {
  df$datetimestamp <- as.POSIXct(gsub("(\\d{2}):(\\d{3})$", "\\1.\\2", df$datetimestamp),
                                 format = "%d.%m.%Y %H:%M:%OS")
  
  # filter for this mouse and sort by time
  mouse <- df[df$data == mouse_id, ]
  mouse <- mouse[order(mouse$datetimestamp), ]
  
  # find rows where location changes
  location <- paste(mouse$deviceid, mouse$antennaID, sep = "-")
  transitions <- which(diff(as.numeric(as.factor(location))) != 0)
  
  cat("Movements for mouse", mouse_id, ":\n")
  for (i in transitions) {
    t_from <- format(mouse$datetimestamp[i],   "%H:%M:%S")
    t_to   <- format(mouse$datetimestamp[i+1], "%H:%M:%S")
    from   <- paste("device", mouse$deviceid[i],   "antenna", mouse$antennaID[i])
    to     <- paste("device", mouse$deviceid[i+1], "antenna", mouse$antennaID[i+1])
    mins   <- round(as.numeric(difftime(mouse$datetimestamp[i+1], mouse$datetimestamp[i], units = "mins")), 1)
    cat(t_from, "->", t_to, ":", from, "->", to, "(", mins, "mins )\n")
  }
}

all_data <- rbind(april18, april19, april20, april21, april22, april23, april24,
                  april25, april26, april27, april28, april29, april30, may1, may2)

track_movements(all_data, 900133000459667)
track_movements(all_data, 900263000641476)
track_movements(all_data, 900263000641471)
track_movements(all_data, 900263000641480)
track_movements(all_data, 900263000641477)
track_movements(all_data, 900263000641474)
track_movements(all_data, 900263000641478)
track_movements(all_data, 900263000641463)
track_movements(all_data, 900263000641479)
track_movements(all_data, 900263002586587)
track_movements(all_data, 900263002586586)
track_movements(all_data, 900263002586597)
track_movements(all_data, 900263002586594)
track_movements(all_data, 900263002586588)
track_movements(all_data, 900263002586592)
track_movements(all_data, 900263002586583)
track_movements(all_data, 900263002586584)
track_movements(all_data, 900263002586595)
track_movements(all_data, 900263002586581)
