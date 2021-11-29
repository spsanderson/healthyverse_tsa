csv_to_rds <- function(){
    data <- read.csv("01_data/cran_logs.csv")
    saveRDS(data, "01_data/cran_logs.rds")
}