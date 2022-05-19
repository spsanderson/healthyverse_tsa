Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 37,293
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,~
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M~
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:~
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4~
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N~
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA~
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N~
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR~
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0~
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", ~
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638~

The last day in the data set is 2022-05-17 22:24:37, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4061.77
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 37293         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     25030 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25030 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25030 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3000 |          0.92 |   2 |   2 |     0 |      108 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-17 | 2021-10-25 |      541 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1453965.17 | 1828628.90 | 357 | 16873 | 271482 | 3241683 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8689.54 |   16630.54 |   1 |   190 |   2659 |    8742 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-17 22:24:37 | 2021-10-25 08:12:41 |    21694 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 9M 55S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Now that we have our full data set and saved our parameters we can
create the full data set.

``` r
horizon         <- 4*7
lag_period      <- 4*7
rolling_periods <- c(7, 14, 28)

data_prepared_full_tbl <- data_transformed_tbl %>%
  group_by(package) %>%
  
  # Add future windows
  bind_rows(
    future_frame(., .date_var = date, .length_out = horizon)
  ) %>%
  
  # Add autocorolated lags
  tk_augment_lags(value_trans, .lags = lag_period) %>%
  
  # Add rolling features
  tk_augment_slidify(
    .value     = value_trans_lag28
    , .f       = median
    , .period  = rolling_periods
    , .align   = "center"
    , .partial = TRUE
  ) %>%
  
  # Format columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .)) %>%
  select(date, package, everything()) %>%
  ungroup()

data_prepared_full_tbl %>% 
  group_by(package) %>% 
  pivot_longer(-c(date, package)) %>% 
  plot_time_series(
    .date_var = date
    , .value = value
    , .color_var = name
    , .smooth = FALSE
    , .interactive = FALSE
    , .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-data_prepared_full_tbl-1.png)<!-- -->

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(value_trans))

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(value_trans))

nested_data_tbl <- data_prepared_tbl %>%
  nest_timeseries(
    .id_var = package
    , .length_future = horizon
  ) %>%
  split_nested_timeseries(
    .length_test = horizon
  )
```

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  ) %>%
  step_mutate(yr = lubridate::year(date)) %>%
  step_harmonic(yr, frequency = 365/12, cycle_size = 1) %>%
  step_hai_fourier(value_trans, scale_type = "sincos", period = 365/12, order = 1) %>%
  step_lag(value_trans, lag = 1) %>%
  step_impute_knn(contains("lag_"))

recipe_base
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          5
    ## 
    ## Operations:
    ## 
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
    ## Lagging value_trans
    ## K-nearest neighbor imputation for contains("lag_")

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wflw_glm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_glm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [482|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [474|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [423|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [399|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [213|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [89 x 6]> <tibble>     <split [61|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0768216 | 11.367133 | 0.0501109 | 10.987719 | 0.0940098 | 0.9929138 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.0815802 | 11.895946 | 0.0532149 | 11.306671 | 0.0982853 | 0.9913896 |
| healthyR.data |         4 | EARTH       | Test  | 0.0790435 | 12.931128 | 0.0515602 | 10.873071 | 0.1142193 | 0.9885658 |
| healthyR.data |         5 | NNAR        | Test  | 0.0676358 |  5.181998 | 0.0441190 |  4.677000 | 0.2297582 | 0.9641452 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0457205 | 15.670715 | 0.0349383 | 12.894597 | 0.0520252 | 0.9978446 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.0465122 | 16.333149 | 0.0355433 | 13.253508 | 0.0531457 | 0.9978585 |
| healthyR      |         4 | EARTH       | Test  | 0.0170752 |  3.009351 | 0.0130484 |  2.945411 | 0.0287229 | 0.9995692 |
| healthyR      |         5 | NNAR        | Test  | 0.0400378 |  3.389680 | 0.0305958 |  3.390105 | 0.0892513 | 0.9939330 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0478146 | 17.488508 | 0.0360715 | 11.733779 | 0.0586039 | 0.9973142 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.0482184 | 18.560231 | 0.0363761 | 12.508681 | 0.0584167 | 0.9974077 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0169177 |  3.704625 | 0.0127628 |  3.583436 | 0.0187387 | 0.9997594 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0187172 |  5.721941 | 0.0141203 |  5.174817 | 0.0299635 | 0.9992258 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0406284 | 10.550398 | 0.0345789 |  9.579179 | 0.0486600 | 0.9977954 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.0395003 | 10.426137 | 0.0336187 |  9.291932 | 0.0476391 | 0.9978235 |
| healthyverse  |         4 | EARTH       | Test  | 0.0090854 |  2.678500 | 0.0077326 |  2.705725 | 0.0129160 | 0.9998473 |
| healthyverse  |         5 | NNAR        | Test  | 0.0075577 |  1.611333 | 0.0064324 |  1.590808 | 0.0147626 | 0.9997966 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0438006 |  7.737936 | 0.0331360 |  7.381359 | 0.0611321 | 0.9963700 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.0440842 |  7.935210 | 0.0333506 |  7.550859 | 0.0624085 | 0.9963929 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0162121 |  2.857201 | 0.0122647 |  2.808082 | 0.0193752 | 0.9997234 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0185579 |  1.731310 | 0.0140394 |  1.709345 | 0.0494273 | 0.9987196 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.1226356 | 54.426556 | 0.0824110 | 36.270431 | 0.1416803 | 0.9948989 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.1561323 | 68.557649 | 0.1049208 | 42.456505 | 0.1771358 | 0.9931595 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0135525 |  1.414239 | 0.0091073 |  1.413215 | 0.0190109 | 0.9996901 |
| TidyDensity   |         5 | NNAR        | Test  | 0.3369911 | 83.302703 | 0.2264577 | 62.433093 | 0.4249707 | 0.9347106 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 10
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         1 REGRESSION  Test  0.0768  11.4  0.0501  11.0  0.0940 0.993
    ## 2 healthyR          4 EARTH       Test  0.0171   3.01 0.0130   2.95 0.0287 1.00 
    ## 3 healthyR~         4 EARTH       Test  0.0169   3.70 0.0128   3.58 0.0187 1.00 
    ## 4 healthyv~         4 EARTH       Test  0.00909  2.68 0.00773  2.71 0.0129 1.00 
    ## 5 healthyR~         4 EARTH       Test  0.0162   2.86 0.0123   2.81 0.0194 1.00 
    ## 6 TidyDens~         4 EARTH       Test  0.0136   1.41 0.00911  1.41 0.0190 1.00

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = FALSE
    )
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [482|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [474|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [423|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [399|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [213|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [89 x 6]> <tibble>     <split [61|28]>  <mdl_time_tbl>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
