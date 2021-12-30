Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
30 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,958
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

The last day in the data set is 2021-12-28 20:23:12, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -699.75
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25958          |
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
| r\_version     |      17405 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17405 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17405 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2200 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-28 | 2021-08-04 |       401 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530188.26 | 1878169.66 | 357 | 20633.5 | 238637 | 3246414 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8069.89 |   15291.61 |   1 |   204.0 |   2817 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-28 20:23:12 | 2021-08-04 04:20:49 |     15125 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     43 |        60 |

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
    ## 1 healthyR.data <tibble [372 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [362 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [313 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [287 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [102 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8135629 |  97.124574 | 0.6383070 |  83.176295 | 1.1674314 | 0.5439641 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2279756 |  12.175636 | 0.1788656 |  12.828439 | 0.7059908 | 0.7644649 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0200131 |  92.807483 | 0.8002841 | 107.606961 | 1.4611651 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2301773 |  12.661602 | 0.1805930 |  13.337361 | 0.7064029 | 0.7641894 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2117376 |   7.074769 | 0.1661256 |   7.802985 | 0.6691604 | 0.7784450 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2277775 |   7.349625 | 0.1787102 |   7.629194 | 0.7167786 | 0.7445094 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2230348 |  11.413193 | 0.1749892 |  11.962276 | 0.6986445 | 0.7710099 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5131801 |  37.650596 | 0.4026320 |  49.040821 | 0.9444102 | 0.7403521 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0231838 | 142.196018 | 0.8027718 |  96.940275 | 1.2016135 | 0.5567452 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2633710 | 142.635872 | 0.9912184 | 115.177688 | 1.5252780 | 0.1214214 |
| healthyR.data |         13 | BATS                       | Test  | 1.0502721 |  91.283504 | 0.8240248 | 117.018322 | 1.4842627 | 0.0039122 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0237750 |  92.093941 | 0.8032356 | 109.062685 | 1.4639491 | 0.2440075 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7377947 | 147.367823 | 0.7415955 | 108.073054 | 0.8722749 | 0.5905216 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0934525 |   9.474912 | 0.0939340 |   9.244364 | 0.2206812 | 0.9671099 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8683084 | 152.884056 | 0.8727816 | 119.639755 | 1.1252420 | 0.0222528 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0977334 |  11.427347 | 0.0982369 |  10.868420 | 0.2197088 | 0.9671386 |
| healthyR      |          7 | EARTH                      | Test  | 0.0374212 |   2.368892 | 0.0376140 |   2.287825 | 0.1042924 | 0.9938473 |
| healthyR      |          8 | NNAR                       | Test  | 0.1246160 |   8.580467 | 0.1252580 |   7.785193 | 0.2583792 | 0.9803021 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0978094 |  12.063702 | 0.0983132 |  11.271451 | 0.2146715 | 0.9688613 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3364619 |  60.692099 | 0.3381952 |  65.083415 | 0.4610831 | 0.9832150 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0339090 | 225.650178 | 1.0392353 | 109.916438 | 1.2762378 | 0.5713182 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2297920 | 237.649106 | 1.2361274 | 130.961612 | 1.4811287 | 0.1238958 |
| healthyR      |         13 | TBATS                      | Test  | 0.8540735 | 122.990704 | 0.8584734 | 135.081883 | 1.1210247 | 0.0595935 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8879607 | 141.270830 | 0.8925352 | 122.736351 | 1.1415046 | 0.3498925 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9318627 | 102.301719 | 0.5881873 | 101.430752 | 1.1396669 | 0.5690752 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1280028 |   7.335275 | 0.0807947 |   7.558267 | 0.3344676 | 0.9516424 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1355141 | 104.402569 | 0.7167310 | 127.875580 | 1.3718378 | 0.1052240 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1277852 |   7.349386 | 0.0806574 |   7.562130 | 0.3339430 | 0.9517284 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1250218 |   5.644808 | 0.0789131 |   5.443993 | 0.2229592 | 0.9765532 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1558751 |   9.292473 | 0.0983876 |   8.799272 | 0.2798441 | 0.9647636 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1298005 |   7.976680 | 0.0819295 |   8.101559 | 0.3298113 | 0.9536983 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5297081 |  38.523901 | 0.3343492 |  48.163378 | 0.6962790 | 0.9594578 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1968867 | 183.313158 | 0.7554692 | 104.424209 | 1.3823128 | 0.5659235 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5032813 | 212.327935 | 0.9488639 | 135.524000 | 1.7538674 | 0.0876508 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3443365 | 106.322234 | 0.8485387 | 164.022596 | 1.6083959 | 0.0733381 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2003821 | 102.399384 | 0.7576754 | 134.098750 | 1.4806191 | 0.2915823 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7845981 | 145.373997 | 0.7487565 | 135.303178 | 0.9635417 | 0.5718953 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1050130 |  15.445993 | 0.1002158 |  15.201833 | 0.1981032 | 0.9751856 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8925163 | 148.657079 | 0.8517448 | 150.625037 | 1.1297992 | 0.1704807 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1074326 |  16.550703 | 0.1025249 |  16.379334 | 0.1982342 | 0.9751512 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0666202 |   6.412924 | 0.0635769 |   6.269588 | 0.1238271 | 0.9916112 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0521596 |   4.535786 | 0.0497769 |   4.370805 | 0.1362527 | 0.9910820 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1026291 |  17.384794 | 0.0979409 |  18.213822 | 0.1903529 | 0.9767129 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3728363 |  42.284525 | 0.3558046 |  53.947551 | 0.5211803 | 0.9855003 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8693966 | 256.186738 | 0.8296813 | 101.886627 | 1.0383666 | 0.6993518 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2041753 | 387.689002 | 1.1491668 | 135.597783 | 1.3643693 | 0.1514182 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0251969 | 162.874403 | 0.9783644 | 145.136247 | 1.3296882 | 0.2232321 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9410609 | 105.379757 | 0.8980719 | 173.121742 | 1.2376241 | 0.3507929 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7450638 |  92.289074 | 0.6211370 | 100.672785 | 0.9667789 | 0.8794083 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1341034 |  15.372241 | 0.1117980 |  18.602327 | 0.2061531 | 0.9691423 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1264767 | 101.990309 | 0.9391093 | 174.226630 | 1.3811886 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1349920 |  15.676432 | 0.1125387 |  19.062024 | 0.2048582 | 0.9698250 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0623899 |   5.306436 | 0.0520126 |   5.289410 | 0.1261026 | 0.9881957 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2882705 |  30.572072 | 0.2403224 |  33.494783 | 0.4156713 | 0.8960059 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1388604 |  16.377811 | 0.1157637 |  19.645730 | 0.2022617 | 0.9708068 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4877035 |  86.466285 | 0.4065835 |  74.239752 | 0.6619767 | 0.8526837 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9690503 | 158.974100 | 0.8078678 |  80.636929 | 1.2204120 | 0.5940885 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3610286 | 182.989347 | 1.1346481 | 140.458776 | 1.5903590 | 0.1406085 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.1324058 | 102.216329 | 0.9440523 | 172.277418 | 1.3893480 |        NA |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9933932 | 104.394458 | 0.8281616 | 145.840367 | 1.2189091 | 0.1435756 |
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
    ##   package      .model_id .model_desc .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>            <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.da~         7 EARTH       Test  0.212   7.07 0.166   7.80 0.669 0.778
    ## 2 healthyR             7 EARTH       Test  0.0374  2.37 0.0376  2.29 0.104 0.994
    ## 3 healthyR.ts          7 EARTH       Test  0.125   5.64 0.0789  5.44 0.223 0.977
    ## 4 healthyverse         7 EARTH       Test  0.0666  6.41 0.0636  6.27 0.124 0.992
    ## 5 healthyR.ai          7 EARTH       Test  0.0624  5.31 0.0520  5.29 0.126 0.988

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
    ## 1 healthyR.data <tibble [372 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [362 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [313 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [287 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [102 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
