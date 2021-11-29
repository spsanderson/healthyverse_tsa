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

ts_downloads_tbl <- function(.data, .by_time = "day", ...){
    
    if(!is.data.frame(.data)){
        stop(call. = FALSE, ".data must be a tibble/data.frame")
    }
    
    grp_var_expr <- rlang::enquos(...)
    
    data_tbl <- as_tibble(.data) %>%
        group_by(date, ...) %>%
        summarise_by_time(
            .date_var = date,
            .by       = .by_time,
            value     = n()
        )
    
    return(data_tbl)
        
}
