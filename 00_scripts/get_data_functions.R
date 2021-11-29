get_cran_data <- function() {
  url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
  f_name <- "old_downloads.RDS"
  f_url <- paste0(url, f_name)
  data <- readRDS(url(f_url, method = "libcurl"))
  data <- as_tibble(data)
  write_csv(data, "01_data/cran_logs.csv")
}

load_cran_data <- function() {
  data <- read_csv("cran_logs.csv")
}