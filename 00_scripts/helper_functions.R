dl_birth_datetime <- function() {
  file_info("cran_logs.csv") %>%
    pull(birth_time)
}

max_cran_datetime <- function() {
  read.csv("cran_logs.csv") %>%
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
