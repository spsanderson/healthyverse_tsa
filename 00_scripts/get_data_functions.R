get_cran_data <- function() {
  url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
  f_name <- "old_downloads.RDS"
  f_url <- paste0(url, f_name)
  data <- readRDS(url(f_url, method = "libcurl"))
  data <- as_tibble(data)
  write_csv(data, "01_data/cran_logs.csv")
}

get_package_release_data <- function(){
  url <- "https://raw.githubusercontent.com/spsanderson/package-downloads/master/"
  f_name <- "pkg_release_tbl.rds"
  f_url <- paste0(url, f_name)
  data <- readRDS(url(f_url, method = "libcurl"))
  data <- as_tibble(data)
  write_csv(data, "01_data/pkg_release_tbl.csv")
}

csv_to_rds <- function(){
  data <- read.csv("01_data/cran_logs.csv")
  pkg_data <- read.csv("01_data/pkg_release_tbl.csv")
  saveRDS(data, "01_data/cran_logs.rds")
  saveRDS(pkg_data, "01_data/pkg_release_tbl.rds")
}

load_cran_data <- function() {
  data <- readRDS("01_data/cran_logs.rds") %>%
    as_tibble()
  
  return(data)
}
