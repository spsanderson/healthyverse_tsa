get_cran_data <- function() {
  url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
  f_name <- "old_downloads.RDS"
  f_url <- paste0(url, f_name)
  data <- readRDS(url(f_url, method = "libcurl"))
  data <- as_tibble(data)
  write_csv(data, "01_data/cran_logs.csv")
}

csv_to_rds <- function(){
  data <- read.csv("01_data/cran_logs.csv")
  saveRDS(data, "01_data/cran_logs.rds")
}

load_cran_data <- function() {
  data <- readRDS("01_data/cran_logs.rds") %>%
    as_tibble()
  
  return(data)
}
