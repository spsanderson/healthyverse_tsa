dl_birth_datetime <- function(){
    
    file_info("cran_logs.csv") %>%
        pull(birth_time)
    
} 

max_cran_datetime <- read.csv("cran_logs.csv") %>% 
    as_tibble() %>%
    select(date, time) %>%
    mutate(
        date_time = paste0(date, " ", time) %>%
            lubridate::ymd_hms()
    ) %>%
    pull(date_time) %>%
    max()
