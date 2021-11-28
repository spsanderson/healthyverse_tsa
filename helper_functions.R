downloads_file_date <- fs::file_info("old_downloads.RDS") %>%
    dplyr::pull(birth_time)

max_cran_date <- readr::read_rds("old_downloads.RDS") %>%
    max(date)