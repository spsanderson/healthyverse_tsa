get_cran_data <- function(){
    
    url    <- "https://github.com/spsanderson/package-downloads/blob/master/"
    f_name <- "old_downloads.rds"
    f_url  <- paste0(url, f_name)
    utils::download.file(f_url, f_name, method = "curl")

}

load_cran_data <- function(){
    
    
}