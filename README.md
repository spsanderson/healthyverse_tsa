Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
06 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,200
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

The last day in the data set is 2022-01-04 17:38:55, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -865.01
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26200          |
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
| r\_version     |      17544 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17544 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17544 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2221 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-04 | 2021-08-04 |       408 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530622.83 | 1878566.81 | 357 | 18908 | 238645.5 | 3246521 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8015.56 |   15232.73 |   1 |   202 |   2806.0 |    8154 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-04 17:38:55 | 2021-08-04 08:11:22 |     15274 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   29.5 |        60 |

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
    ## 1 healthyR.data <tibble [379 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [369 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [319 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [294 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [108 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0933958 | 335.284183 | 0.9017841 | 107.133173 | 1.3646524 | 0.5106568 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1951398 |  17.631209 | 0.1609426 |  19.178528 | 0.7000711 | 0.7419112 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.1516471 | 343.343618 | 0.9498272 | 110.113908 | 1.4965173 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1961301 |  17.372680 | 0.1617593 |  19.485683 | 0.7006509 | 0.7414179 |
| healthyR.data |          7 | EARTH                      | Test  | 0.1501462 |   4.416194 | 0.1238339 |   5.301622 | 0.6523397 | 0.7874609 |
| healthyR.data |          8 | NNAR                       | Test  | 0.1630138 |   5.734149 | 0.1344465 |   6.868551 | 0.7397716 | 0.7200321 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1936199 |  15.366031 | 0.1596891 |  18.410845 | 0.6940109 | 0.7469404 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4333765 |  87.379804 | 0.3574296 |  68.294616 | 0.8795065 | 0.7445276 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1070182 | 388.716413 | 0.9130193 | 115.007827 | 1.2308716 | 0.4993249 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4175687 | 285.347849 | 1.1691475 | 122.238683 | 1.6744659 | 0.1235916 |
| healthyR.data |         13 | BATS                       | Test  | 1.3315940 | 421.476543 | 1.0982393 | 114.678281 | 1.6262266 | 0.2222917 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.1466105 | 340.962007 | 0.9456732 | 109.961359 | 1.4925283 | 0.1796002 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 1.0493218 | 283.440182 | 1.0443567 | 137.352682 | 1.1704012 | 0.6690966 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0761074 |   9.440879 | 0.0757473 |   9.310817 | 0.2111401 | 0.9646665 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.2582660 | 338.624783 | 1.2523122 | 138.843045 | 1.4513232 | 0.0343602 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0775466 |  10.554176 | 0.0771796 |  10.342722 | 0.2105285 | 0.9646792 |
| healthyR      |          7 | EARTH                      | Test  | 0.0134139 |   1.476637 | 0.0133504 |   1.475190 | 0.0285031 | 0.9996432 |
| healthyR      |          8 | NNAR                       | Test  | 0.1295384 |  18.631853 | 0.1289255 |  16.220993 | 0.3668206 | 0.9046596 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0788545 |  10.776388 | 0.0784814 |  10.454260 | 0.2063708 | 0.9659011 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4203154 |  99.356189 | 0.4183266 | 105.036704 | 0.5265205 | 0.9880472 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1255673 | 285.062544 | 1.1202414 | 120.002275 | 1.3206889 | 0.5451740 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4808316 | 394.976221 | 1.4738247 | 134.795841 | 1.7926982 | 0.1687501 |
| healthyR      |         13 | TBATS                      | Test  | 1.2895101 | 351.942473 | 1.2834085 | 139.953723 | 1.4649623 | 0.0916412 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.2190490 | 320.331888 | 1.2132807 | 138.378433 | 1.4098700 | 0.3136170 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9158777 | 237.472820 | 0.5660847 | 106.311804 | 1.1036983 | 0.4875985 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1057739 |  10.231772 | 0.0653766 |   9.489555 | 0.3093666 | 0.9549141 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2431042 | 397.810634 | 0.7683364 | 114.813373 | 1.4567100 | 0.1744547 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1036874 |   8.403472 | 0.0640870 |   8.211499 | 0.3066134 | 0.9549075 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0820307 |   3.760102 | 0.0507014 |   3.735667 | 0.1852757 | 0.9806737 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0899824 |   4.534933 | 0.0556162 |   4.446687 | 0.2456387 | 0.9661562 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1123769 |  14.143829 | 0.0694578 |  16.817423 | 0.3022232 | 0.9557869 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4331692 |  63.316552 | 0.2677327 |  66.902733 | 0.6038818 | 0.9654319 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1974526 | 353.750747 | 0.7401201 | 111.105453 | 1.3621107 | 0.5903221 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5218051 | 567.703331 | 0.9405956 | 127.736201 | 1.8218357 | 0.1481340 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3989733 | 470.716144 | 0.8646758 | 116.874452 | 1.6245888 | 0.0331819 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2001928 | 350.478760 | 0.7418138 | 113.071692 | 1.4679872 | 0.3302563 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7919374 | 286.681144 | 0.7549900 | 140.019324 | 0.9405815 | 0.7217730 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1038410 |  21.632313 | 0.0989963 |  22.673447 | 0.1957474 | 0.9747016 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3237377 | 518.788361 | 1.2619794 | 148.333988 | 1.4998624 | 0.2361218 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1110323 |  25.106039 | 0.1058522 |  25.880284 | 0.1993196 | 0.9745412 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0266048 |   3.708328 | 0.0253636 |   3.636850 | 0.0710044 | 0.9962168 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0161001 |   3.339823 | 0.0153490 |   3.313277 | 0.0435420 | 0.9991965 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0979884 |  22.257978 | 0.0934168 |  18.504255 | 0.1899494 | 0.9758100 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4436322 | 131.871717 | 0.4229348 | 101.037796 | 0.5590028 | 0.9868643 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8303501 | 324.315700 | 0.7916105 | 106.147099 | 0.9868289 | 0.7134902 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5295670 | 818.897916 | 1.4582059 | 144.470466 | 1.7557397 | 0.2055396 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8588934 | 275.355713 | 0.8188222 | 149.644636 | 1.0204030 | 0.5201422 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.2584479 | 435.079901 | 1.1997356 | 146.337940 | 1.4939957 | 0.3027926 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1167174 | 220.043152 | 0.7963846 | 110.305971 | 1.3686708 | 0.4090352 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1488037 |  19.924990 | 0.1061190 |  21.882189 | 0.2107926 | 0.9737958 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9583774 | 134.419433 | 0.6834649 | 120.027644 | 1.1944987 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1535545 |  20.497602 | 0.1095070 |  22.028777 | 0.2150629 | 0.9729104 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0466332 |   3.977232 | 0.0332564 |   3.945229 | 0.1001814 | 0.9930430 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2386882 |  28.549166 | 0.1702200 |  33.102382 | 0.3265742 | 0.9662791 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1613105 |  21.965461 | 0.1150383 |  23.747152 | 0.2173308 | 0.9718150 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8080367 | 161.930499 | 0.5762497 | 106.199006 | 0.9494746 | 0.8581685 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8730621 | 169.721738 | 0.6226224 |  76.047629 | 1.1118490 | 0.6842953 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1623774 | 236.251863 | 0.8289470 | 117.944162 | 1.3169268 | 0.2227110 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9070268 | 156.409623 | 0.6468443 | 114.054410 | 1.1245562 | 0.1959978 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9990503 | 157.886485 | 0.7124706 | 115.036638 | 1.2561696 | 0.1937150 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ##   # A tibble: 5 x 10
    ##   package     .model_id .model_desc .type    mae  mape   mase smape   rmse   rsq
    ##   <chr>           <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR.d~         7 EARTH       Test  0.150   4.42 0.124   5.30 0.652  0.787
    ## 2 healthyR            7 EARTH       Test  0.0134  1.48 0.0134  1.48 0.0285 1.00 
    ## 3 healthyR.ts         7 EARTH       Test  0.0820  3.76 0.0507  3.74 0.185  0.981
    ## 4 healthyver~         8 NNAR        Test  0.0161  3.34 0.0153  3.31 0.0435 0.999
    ## 5 healthyR.ai         7 EARTH       Test  0.0466  3.98 0.0333  3.95 0.100  0.993

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
    ## 1 healthyR.data <tibble [379 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [369 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [319 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [294 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [108 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
