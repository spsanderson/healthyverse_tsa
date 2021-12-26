Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
26 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,768
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

The last day in the data set is 2021-12-24 15:35:44, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -598.95
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25768          |
| Number of columns                                | 11             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 6              |
| Date                                             | 1              |
| numeric                                          | 2              |
| POSIXct                                          | 1              |
| Timespan                                         | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| r\_version     |      17265 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17265 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17265 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2184 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-24 | 2021-08-04 |       397 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1531905.96 | 1879017.21 | 357 | 21941 | 238655 | 3246423 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8107.52 |   15331.85 |   1 |   209 |   2831 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-24 15:35:44 | 2021-08-04 01:18:17 |     15017 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     34 |        60 |

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
  step_rm(yr) %>%
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
    ## Variable mutation
    ## Harmonic numeric variables for yr
    ## Delete terms yr
    ## Fourier transformation on value_trans
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

# Boosted Auto ARIMA ------------------------------------------------------

model_spec_arima_boosted <- arima_boost(
  min_n = 2
  , learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

wflw_arima_boosted <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_boosted)

# ETS ---------------------------------------------------------------------

model_spec_ets <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "ets") 

wflw_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_ets)

model_spec_croston <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "croston")

wflw_croston <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_croston)

model_spec_theta <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "theta")

wflw_theta <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_theta)


# STLM ETS ----------------------------------------------------------------

model_spec_stlm_ets <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_ets")

wflw_stlm_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_ets)

model_spec_stlm_tbats <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("tbats")

wflw_stlm_tbats <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_tbats)

model_spec_stlm_arima <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_arima")

wflw_stlm_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_arima)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)


# Prophet -----------------------------------------------------------------

model_spec_prophet <- prophet_reg(
  seasonality_yearly = "auto",
  seasonality_weekly = "auto",
  seasonality_daily = "auto"
) %>%
  set_engine(engine = "prophet")

wflw_prophet <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet)

model_spec_prophet_boost <- prophet_boost(
  learn_rate = 0.1
  , trees = 10
  , seasonality_yearly = FALSE
  , seasonality_weekly = FALSE
  , seasonality_daily  = FALSE
) %>% 
  set_engine("prophet_xgboost") 

wflw_prophet_boost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet_boost)

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

# XGBoost -----------------------------------------------------------------

model_spec_xgboost <- boost_tree(
  mode  = "regression",
  mtry  = 10,
  trees = 100,
  min_n = 5,
  tree_depth = 3,
  learn_rate = 0.3,
  loss_reduction = 0.01
) %>%
  set_engine("xgboost")

wflw_xgboost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_xgboost)
```

### Nested Modeltime Tables

``` r
parallel_start(n_cores)
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
  control = control_nested_fit(
    verbose = TRUE,
    allow_par = TRUE,
    cores = n_cores
  ),
  
  # Add workflows
  wflw_arima_boosted,
  wflw_auto_arima,
  wflw_croston,
  wflw_ets,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar,
  wflw_prophet,
  wflw_prophet_boost,
  wflw_stlm_arima,
  wflw_stlm_ets,
  wflw_stlm_tbats,
  wflw_theta,
  wflw_xgboost
)
parallel_stop()

nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [368 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [358 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [309 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [283 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [98 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8344096 |  87.209151 | 0.7127215 |  82.490681 | 1.1707748 | 0.2669175 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2895806 |  21.938139 | 0.2473489 |  21.951881 | 0.6982860 | 0.7355882 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.1683975 |  89.260086 | 0.9980015 | 140.363426 | 1.5705225 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2472811 |  15.139203 | 0.2112183 |  15.492683 | 0.7007614 | 0.7351764 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2322361 |   8.090650 | 0.1983674 |   8.642378 | 0.6666601 | 0.7500089 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2473988 |   9.055932 | 0.2113188 |   8.801045 | 0.6579906 | 0.7589942 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2429402 |  14.521507 | 0.2075105 |  14.751576 | 0.6919855 | 0.7431890 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5940837 |  41.706389 | 0.5074441 |  54.796816 | 0.9979535 | 0.7020046 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0638749 | 135.332832 | 0.9087222 |  88.313085 | 1.4076658 | 0.3671044 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4464233 | 139.577341 | 1.2354808 | 112.223422 | 1.8644008 | 0.0319087 |
| healthyR.data |         13 | BATS                       | Test  | 1.1907719 |  97.979184 | 1.0171129 | 153.560691 | 1.5738660 | 0.0237371 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.1874496 |  90.572511 | 1.0142752 | 147.130545 | 1.5862611 | 0.1193496 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7094159 | 126.118624 | 0.7675952 |  97.258950 | 0.8564626 | 0.5598165 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0979498 |   9.566760 | 0.1059826 |   9.238306 | 0.2192647 | 0.9642210 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8856528 | 130.159759 | 0.9582853 | 123.692507 | 1.1313670 | 0.0057824 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1046013 |  12.069518 | 0.1131797 |  11.107306 | 0.2184849 | 0.9642302 |
| healthyR      |          7 | EARTH                      | Test  | 0.0428612 |   2.509463 | 0.0463763 |   2.434048 | 0.1037121 | 0.9929778 |
| healthyR      |          8 | NNAR                       | Test  | 0.0868708 |   7.305090 | 0.0939951 |   7.212170 | 0.1887841 | 0.9856825 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1062165 |  13.659519 | 0.1149273 |  12.082391 | 0.2135152 | 0.9662171 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3244646 |  50.968575 | 0.3510740 |  50.467984 | 0.4485793 | 0.9821062 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8635728 | 210.864190 | 0.9343946 |  90.512082 | 1.1905616 | 0.5472613 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1882570 | 216.192075 | 1.2857062 | 134.523641 | 1.4449079 | 0.0902893 |
| healthyR      |         13 | TBATS                      | Test  | 0.8717406 | 137.823690 | 0.9432323 | 123.652070 | 1.1286227 | 0.0175148 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8789854 | 124.469168 | 0.9510712 | 117.511003 | 1.1264428 | 0.1065119 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9473363 |  88.252422 | 0.6631747 |  99.513611 | 1.1473765 | 0.5702313 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1371016 |   8.012525 | 0.0959768 |   8.176585 | 0.3321708 | 0.9493279 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1457592 |  97.419252 | 0.8020790 | 124.152439 | 1.3761227 | 0.0724982 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1363177 |   7.830992 | 0.0954280 |   7.981967 | 0.3292544 | 0.9494862 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1263663 |   5.812725 | 0.0884617 |   5.617368 | 0.2208314 | 0.9759540 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2044650 |  11.654252 | 0.1431340 |  10.261635 | 0.3821657 | 0.9425144 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1374253 |   8.461231 | 0.0962034 |   8.550384 | 0.3233117 | 0.9517050 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5647519 |  38.295682 | 0.3953498 |  48.849248 | 0.7224456 | 0.9575805 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0297995 | 154.846724 | 0.7209024 |  86.420394 | 1.2795695 | 0.5261834 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5561179 | 195.264994 | 1.0893471 | 143.386879 | 1.7965599 | 0.0528566 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3791396 | 101.367324 | 0.9654549 | 164.127796 | 1.6478918 | 0.0468839 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2221017 |  96.980549 | 0.8555218 | 132.990105 | 1.4867843 | 0.0445595 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8018227 | 121.486283 | 0.8416989 | 132.868120 | 0.9774876 | 0.5305992 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1042130 |  14.575969 | 0.1093957 |  15.402259 | 0.1952363 | 0.9733430 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9328102 | 128.496833 | 0.9792008 | 154.747790 | 1.1611436 | 0.1210069 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1054437 |  13.421802 | 0.1106876 |  13.561322 | 0.1930213 | 0.9735200 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0686681 |   6.156379 | 0.0720831 |   6.021939 | 0.1283234 | 0.9913214 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0633731 |   4.174768 | 0.0665247 |   3.829717 | 0.1641641 | 0.9837175 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1049209 |  14.838605 | 0.1101388 |  15.662519 | 0.1865329 | 0.9750058 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4161013 |  46.882004 | 0.4367949 |  59.502585 | 0.5520332 | 0.9860103 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8571827 | 270.122911 | 0.8998121 |  91.308068 | 1.0907527 | 0.6118827 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2512023 | 400.443122 | 1.3134271 | 139.912797 | 1.4368445 | 0.0846983 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9890321 | 126.774350 | 1.0382187 | 149.260261 | 1.2516107 | 0.1361405 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9841145 |  99.707249 | 1.0330564 | 198.232566 | 1.2629543 | 0.1116020 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7230310 |  83.079586 | 0.7439689 |  95.044453 | 0.9673144 | 0.8926579 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1272089 |  13.011852 | 0.1308927 |  12.931762 | 0.2026833 | 0.9701840 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1253019 | 109.025109 | 1.1578890 | 178.450108 | 1.3729547 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1353374 |  15.021332 | 0.1392566 |  14.823270 | 0.2005577 | 0.9712305 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0630480 |   5.013870 | 0.0648738 |   5.065848 | 0.1304636 | 0.9874536 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3151872 |  35.130116 | 0.3243146 |  38.643428 | 0.4460215 | 0.8650750 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1383387 |  16.124832 | 0.1423448 |  15.988467 | 0.1974486 | 0.9725276 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4762960 |  74.776608 | 0.4900888 |  68.042358 | 0.6381183 | 0.8411002 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9479534 | 184.872357 | 0.9754048 |  88.581713 | 1.2858800 | 0.4455401 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4456123 | 226.263002 | 1.4874751 | 146.458574 | 1.7082715 | 0.0706831 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.1669377 | 136.010755 | 1.2007305 | 167.051872 | 1.3989191 | 0.1684381 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9704768 |  94.752340 | 0.9985804 | 144.705752 | 1.2132795 | 0.0265122 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = .2,
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
    ##   # A tibble: 5 x 10
    ##   package      .model_id .model_desc .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>            <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.da~         8 NNAR        Test  0.247   9.06 0.211   8.80 0.658 0.759
    ## 2 healthyR             7 EARTH       Test  0.0429  2.51 0.0464  2.43 0.104 0.993
    ## 3 healthyR.ts          7 EARTH       Test  0.126   5.81 0.0885  5.62 0.221 0.976
    ## 4 healthyverse         7 EARTH       Test  0.0687  6.16 0.0721  6.02 0.128 0.991
    ## 5 healthyR.ai          7 EARTH       Test  0.0630  5.01 0.0649  5.07 0.130 0.987

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
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
parallel_start(n_cores)
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = TRUE, 
      cores = n_cores
    )
  )
parallel_stop()

nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [368 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [358 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [309 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [283 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [98 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
