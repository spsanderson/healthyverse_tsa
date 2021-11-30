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
        ) %>%
        ungroup()
    
    return(data_tbl)
        
}

liv <- function(x, limit_lower = "auto", limit_upper = "auto", offset = 0, silent = FALSE) {
    
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    
    x <- x + offset
    if (any(x <= 0)) rlang::abort("x <= 0: Try using an offset to avoid values less than or equal to zero.")
    
    max_x   <- max(x)
    min_x   <- min(x)
    range_x <- abs(max_x - min_x)
    
    # Convert character strings to numeric
    limit_lower <- auto_limit_lower(limit_lower, min_x, range_x)
    limit_upper <- auto_limit_upper(limit_upper, max_x, range_x)
    
    # Checks
    if (any(is.na(x))) rlang::abort("Missing values detected. Try replacing missing values.")
    if (limit_upper <= max_x) rlang::abort("limit_upper <= max(x): This results in NaN. Try increasing limit_upper to a value greater than or equal to max(x).")
    if (limit_lower >= min_x) rlang::abort("limit_lower >= min(x): This results in NaN. Try decreasing limit_lower to a value less than or equal to min(x).")
    
    # Message
    if (!silent) message("log_interval_vec(): \n Using limit_lower: ", limit_lower, "\n Using limit_upper: ", limit_upper, "\n Using offset: ", offset)
    
    scaled <- (x - limit_lower) / (limit_upper - x)
    ls     <- log(scaled)
    
    out_list <- list(
        limit_lower = limit_lower,
        limit_upper = limit_upper,
        offset      = offset,
        log_scaled  = ls
    )
    
    return(out_list)
}

liiv <- function(x, limit_lower, limit_upper, offset = 0) {
    
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    
    if (rlang::is_missing(limit_lower)) {
        rlang::abort("log_interval_inv_vec(limit_lower): Is missing. Please provide a value.")
    }
    if (rlang::is_missing(limit_upper)) {
        rlang::abort("log_interval_inv_vec(limit_upper): Is missing. Please provide a value.")
    }
    
    a <- limit_lower
    b <- limit_upper
    
    v <- (b-a)*(exp(x)) / (1 + exp(x)) + a - offset
    
    out_list <- list(
        limit_lower = a,
        limit_upper = b,
        rescaled_v  = v
    )
}

auto_limit_lower <- function(limit_lower, min_x, range_x) {
    if (limit_lower == "auto") {
        limit_lower <- 0
    }
    return(limit_lower)
}

auto_limit_upper <- function(limit_upper, max_x, range_x) {
    if (limit_upper == "auto") {
        limit_upper <- max_x + (0.1 * range_x)
    }
    return(limit_upper)
}

standard_vec <- function(x, mean = NULL, sd = NULL, silent = FALSE) {
    
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    
    m <- mean
    s <- sd
    
    if (is.null(mean)) {
        m <- mean(x, na.rm = T)
    }
    if (is.null(sd)) {
        s <- stats::sd(x, na.rm = T)
    }
    
    if (!silent) {
        if (is.null(mean)) {
            if (is.null(sd)) {
                message(stringr::str_glue("Standardization Parameters
                                  mean: {m}
                                  standard deviation: {s}"))
            }
        }
    }
    
    sv <- (x - m) / s
    
    out_list <- list(
        mean            = m,
        sd              = s,
        standard_scaled = sv
    )
    
    return(out_list)
    
}

standard_inv_vec <- function(x, mean, sd) {
    
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    
    if (rlang::is_missing(mean)) {
        rlang::abort("`mean` is missing with no default.")
    }
    if (rlang::is_missing(sd)) {
        rlang::abort("`sd` is missing with no default.")
    }
    
    si <- (x * sd) + mean
    
    out_list <- list(
        mean                   = mean,
        sd                     = sd,
        standard_inverse_value = si
    )
    
    return(out_list)
    
}
