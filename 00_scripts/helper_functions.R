dl_birth_datetime <- function() {
  file_info("01_data/cran_logs.csv") %>%
    pull(birth_time)
}

max_cran_datetime <- function() {
  read.csv("01_data/cran_logs.csv") %>%
    as_tibble() %>%
    select(date, time) %>%
    mutate(
      date_time = paste0(date, " ", time) %>%
        lubridate::ymd_hms()
    ) %>%
    pull(date_time) %>%
    max()
}

hours_since_cran_log_update <- function(){
    round(as.numeric(difftime(dl_birth_datetime(), max_cran_datetime(), units = "hours")), 2)
}

update_log_message <- function(){
  if(hours_since_cran_log_update() > 36){
    msg <- "Consider updating the cran log file from the package-downloads project."
  } else {
    msg <- "Happy analyzing!"
  }
  
  return(msg)
}