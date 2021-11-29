downloads_processed_tbl <- function(){
    data <- load_cran_data()
    
    data_tbl <- data %>%
        mutate(
            date_time = paste0(date, " ", time) %>%
                lubridate::ymd_hms()
        ) %>%
        mutate(date = as.Date(date)) %>%
        mutate(time = hms(time)) %>%
        select(date, time, date_time, everything())
    
    return(data_tbl)
}

