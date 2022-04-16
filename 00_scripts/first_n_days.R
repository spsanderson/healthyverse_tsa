first_30_days_tbl <- downloads_tbl %>%
    group_by(package) %>%
    select(date_time, package) %>%
    summarise_by_time(
        .date_var = date_time,
        .by = "day",
        value = n()
    ) %>%
    mutate(cum_sum = cumsum(value)) %>%
    mutate(rec_no = row_number()) %>%
    ungroup() %>%
    mutate(package = as.factor(package)) %>%
    filter(rec_no <= 90)

plot_time_series(
    .data = first_30_days_tbl,
    .date_var = rec_no,
    .value = value,
    .color_var = package,
    .smooth = FALSE
)

plot_time_series(
    .data = first_30_days_tbl,
    .date_var = rec_no,
    .value = cum_sum,
    .color_var = package,
    .smooth = FALSE
)
