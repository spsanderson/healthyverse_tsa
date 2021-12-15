Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 24,993
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

The last day in the data set is 2021-12-13 23:09:08, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -342.51
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 24993          |
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
| r\_version     |      16770 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16770 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16770 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2110 |           0.92 |   2 |   2 |     0 |        98 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-13 | 2021-07-28 |       386 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532829.79 | 1879360.78 | 357 | 26285 | 238439 | 3246189 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8129.53 |   15323.82 |   1 |   231 |   2989 |    8310 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-13 23:09:08 | 2021-07-28 21:13:12 |     14582 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 45M 48S |        60 |

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
    ## 1 healthyR.data <tibble [357 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [347 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [298 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [272 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [87 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9821799 |  65.696219 | 0.7208793 |  94.251110 | 1.4526864 | 0.3086830 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2527830 |   9.407418 | 0.1855323 |  10.733316 | 0.7763625 | 0.7423724 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4432327 | 102.926846 | 1.0592729 | 196.114698 | 1.8918697 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2616084 |  11.390889 | 0.1920097 |  12.391657 | 0.7674836 | 0.7418598 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2466411 |   8.369078 | 0.1810243 |   8.745561 | 0.6527079 | 0.7782783 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2217371 |   7.360063 | 0.1627458 |   7.637528 | 0.6444284 | 0.7834651 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2487991 |   9.131470 | 0.1826082 |  10.448381 | 0.7717903 | 0.7479554 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7449067 |  49.491371 | 0.5467306 |  68.989090 | 1.1628847 | 0.7313771 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7868014 |  87.585330 | 0.5774796 |  72.434713 | 1.0979308 | 0.5319671 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5373168 | 127.157329 | 1.1283267 | 129.405667 | 1.9721202 | 0.0929887 |
| healthyR.data |         13 | BATS                       | Test  | 1.3920247 |  93.441054 | 1.0216884 | 176.393814 | 1.8732001 | 0.0219005 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4778417 | 106.339340 | 1.0846745 | 192.542523 | 1.9258395 | 0.1141896 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7848701 |  79.976644 | 0.7741044 |  95.523064 | 1.0118608 | 0.7095075 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1196501 |   8.036563 | 0.1180089 |   8.101628 | 0.2586033 | 0.9631575 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3074305 | 124.109122 | 1.2894969 | 164.789179 | 1.6257866 | 0.0168582 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1223055 |   8.620244 | 0.1206279 |   8.625375 | 0.2564429 | 0.9631368 |
| healthyR      |          7 | EARTH                      | Test  | 0.0792178 |   3.824511 | 0.0781312 |   3.621010 | 0.1704276 | 0.9912538 |
| healthyR      |          8 | NNAR                       | Test  | 0.1484895 |   7.174434 | 0.1464527 |   6.574972 | 0.3266857 | 0.9810757 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1154213 |   7.745281 | 0.1138381 |   7.836908 | 0.2549914 | 0.9654687 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3940583 |  32.009347 | 0.3886531 |  41.685767 | 0.5764835 | 0.9699969 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7209894 | 122.759436 | 0.7110998 |  75.498101 | 0.9478895 | 0.6999186 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4114527 | 170.477660 | 1.3920923 | 143.171026 | 1.6480456 | 0.2236185 |
| healthyR      |         13 | TBATS                      | Test  | 1.2832470 | 127.987875 | 1.2656451 | 168.884366 | 1.5845000 | 0.0264922 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3338515 | 118.780948 | 1.3155556 | 172.657901 | 1.6435729 | 0.1313849 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0455662 |  99.713847 | 0.8981102 | 119.055027 | 1.3395872 | 0.5518971 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1607870 |  10.182917 | 0.1381112 |  10.233615 | 0.3491503 | 0.9485754 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6283251 | 145.377293 | 1.3986826 | 169.791211 | 1.9285649 | 0.1864165 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1612879 |  10.405596 | 0.1385415 |  10.405055 | 0.3473217 | 0.9486307 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1430441 |   6.959687 | 0.1228706 |   6.646028 | 0.2427411 | 0.9743213 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2039895 |   9.605184 | 0.1752209 |   8.787299 | 0.3722855 | 0.9543569 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1546600 |   9.895933 | 0.1328484 |   9.939028 | 0.3456932 | 0.9508419 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6328368 |  44.741135 | 0.5435879 |  59.065064 | 0.8806863 | 0.9001871 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0106370 | 212.013307 | 0.8681071 |  89.761107 | 1.2185700 | 0.6407592 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.8285286 | 259.870943 | 1.5706515 | 142.307365 | 2.1595290 | 0.1448752 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5100512 | 126.118254 | 1.2970889 | 169.879158 | 1.8221176 | 0.2727189 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6788876 | 152.332035 | 1.4421143 | 181.080231 | 1.9971779 | 0.2376143 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8448418 |  82.490305 | 0.7682621 | 115.355603 | 1.0843576 | 0.6453916 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1158839 |  11.761981 | 0.1053797 |  12.590890 | 0.2159357 | 0.9733929 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3643120 | 173.840949 | 1.2406455 | 170.527287 | 1.6339851 | 0.2437206 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1138545 |  12.599464 | 0.1035343 |  15.842506 | 0.2159928 | 0.9734896 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1178451 |   9.708191 | 0.1071631 |   9.514803 | 0.1896264 | 0.9888331 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0348703 |   5.650246 | 0.0317095 |   7.898382 | 0.0611132 | 0.9975080 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1103120 |  14.305270 | 0.1003129 |  15.530062 | 0.2095755 | 0.9748133 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5850963 |  78.958511 | 0.5320609 |  83.826515 | 0.7495393 | 0.9781054 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8137411 | 231.550647 | 0.7399805 |  92.702073 | 1.0055602 | 0.6701211 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5496238 | 244.553482 | 1.4091599 | 154.224743 | 1.7949640 | 0.1423699 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3540851 | 178.061954 | 1.2313456 | 170.633768 | 1.6269767 | 0.2448118 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4305914 | 196.719047 | 1.3009171 | 169.790375 | 1.7310912 | 0.1159228 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9090134 |  86.391022 | 0.8540567 | 127.622399 | 1.1997620 | 0.5290597 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1253012 |  12.568089 | 0.1177258 |  12.434498 | 0.2247055 | 0.9712584 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2729976 | 127.717370 | 1.1960352 | 177.766511 | 1.5713551 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1324935 |  14.205349 | 0.1244833 |  14.020712 | 0.2244611 | 0.9705701 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1056106 |   5.301133 | 0.0992257 |   5.634894 | 0.2632374 | 0.9725788 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3733719 |  27.973299 | 0.3507988 |  35.443509 | 0.6092379 | 0.9072566 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1358911 |  16.090080 | 0.1276754 |  17.449924 | 0.2250037 | 0.9696245 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5227073 |  37.328225 | 0.4911056 |  48.896908 | 0.8345560 | 0.8147540 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0369711 | 215.169608 | 0.9742783 |  97.397042 | 1.4253695 | 0.4857779 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4559566 | 211.369866 | 1.3679329 | 134.861981 | 1.7635052 | 0.1044764 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.3375438 | 138.885847 | 1.2566792 | 171.425908 | 1.6456932 | 0.0040743 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.2755494 | 128.215108 | 1.1984327 | 177.634545 | 1.5739310 | 0.1530023 |
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
    ##   package     .model_id .model_desc .type    mae  mape   mase smape   rmse   rsq
    ##   <chr>           <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR.d~         8 NNAR        Test  0.222   7.36 0.163   7.64 0.644  0.783
    ## 2 healthyR            7 EARTH       Test  0.0792  3.82 0.0781  3.62 0.170  0.991
    ## 3 healthyR.ts         7 EARTH       Test  0.143   6.96 0.123   6.65 0.243  0.974
    ## 4 healthyver~         8 NNAR        Test  0.0349  5.65 0.0317  7.90 0.0611 0.998
    ## 5 healthyR.ai         6 LM          Test  0.132  14.2  0.124  14.0  0.224  0.971

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
    ## 1 healthyR.data <tibble [357 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [347 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [298 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [272 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [87 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
