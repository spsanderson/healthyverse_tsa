Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
29 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,884
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

The last day in the data set is 2021-12-27 13:56:59, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -669.31
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25884          |
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
| r\_version     |      17352 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17352 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17352 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2190 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-27 | 2021-08-04 |       400 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530829.61 | 1878311.64 | 357 | 21941 | 238655 | 3246412 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8084.21 |   15309.31 |   1 |   202 |   2823 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-27 13:56:59 | 2021-08-04 02:43:24 |     15091 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     39 |        60 |

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
    ## 1 healthyR.data <tibble [371 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [361 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [312 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [286 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [101 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8127696 |  92.762833 | 0.6481947 |  77.980010 | 1.1608429 | 0.4129929 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2410514 |  14.252221 | 0.1922418 |  14.740152 | 0.7028611 | 0.7573435 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0431172 |  87.341346 | 0.8319000 | 111.727149 | 1.4772874 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2354868 |  13.179435 | 0.1878039 |  13.832803 | 0.7034361 | 0.7574856 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2007155 |   6.367458 | 0.1600733 |   6.945857 | 0.6528306 | 0.7811812 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2140678 |   6.471041 | 0.1707220 |   6.731092 | 0.6763599 | 0.7644578 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2297044 |  12.248501 | 0.1831924 |  12.761298 | 0.6950619 | 0.7647145 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5146756 |  36.400424 | 0.4104607 |  46.640716 | 0.9424006 | 0.7321482 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0391441 | 142.698298 | 0.8287314 |  95.885065 | 1.2037846 | 0.5561498 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2089285 | 129.849135 | 0.9641367 | 111.161239 | 1.4986302 | 0.1355470 |
| healthyR.data |         13 | BATS                       | Test  | 1.0381169 |  85.037068 | 0.8279122 | 113.066579 | 1.4835941 | 0.0009203 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0522432 |  87.435923 | 0.8391781 | 114.437140 | 1.4827970 | 0.2058670 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7177161 | 128.112817 | 0.7358476 | 102.437160 | 0.8583671 | 0.5833599 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0952683 |   9.214803 | 0.0976751 |   8.937899 | 0.2199476 | 0.9664315 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8626067 | 128.303975 | 0.8843985 | 118.755771 | 1.1202048 | 0.0292410 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1002455 |  11.111437 | 0.1027780 |  10.410429 | 0.2190879 | 0.9664597 |
| healthyR      |          7 | EARTH                      | Test  | 0.0375960 |   2.345963 | 0.0385458 |   2.266418 | 0.1037981 | 0.9937474 |
| healthyR      |          8 | NNAR                       | Test  | 0.0794070 |   5.533264 | 0.0814131 |   5.078695 | 0.1932410 | 0.9791421 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0993289 |  11.415472 | 0.1018383 |  10.597208 | 0.2142193 | 0.9682003 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3345041 |  57.396408 | 0.3429546 |  59.852652 | 0.4618591 | 0.9830150 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0096839 | 213.172679 | 1.0351913 | 100.333307 | 1.2697465 | 0.5762499 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1910038 | 210.827311 | 1.2210918 | 124.454284 | 1.4628936 | 0.1313151 |
| healthyR      |         13 | TBATS                      | Test  | 0.8400364 | 132.512297 | 0.8612580 | 115.529008 | 1.1082562 | 0.0422745 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8918204 | 118.672534 | 0.9143503 | 122.991334 | 1.1531235 | 0.3324728 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9105693 |  90.914848 | 0.5875828 |  95.562512 | 1.1272048 | 0.5603132 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1292396 |   7.086624 | 0.0833973 |   7.326825 | 0.3327594 | 0.9498771 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1694724 |  93.459368 | 0.7546508 | 130.414897 | 1.4120048 | 0.1158223 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1287329 |   7.005305 | 0.0830703 |   7.242871 | 0.3315287 | 0.9499583 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1277339 |   5.983984 | 0.0824256 |   5.793806 | 0.2296228 | 0.9734604 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2140617 |  12.542590 | 0.1381322 |  11.220944 | 0.3624325 | 0.9472453 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1305678 |   7.749170 | 0.0842543 |   7.886069 | 0.3275951 | 0.9520386 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5705181 |  39.708178 | 0.3681506 |  51.500103 | 0.7279365 | 0.9571170 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1827510 | 170.648712 | 0.7632194 |  99.466672 | 1.3736505 | 0.5827750 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4389787 | 185.897917 | 0.9285609 | 129.321893 | 1.7201995 | 0.1095186 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3569760 | 103.788743 | 0.8756453 | 161.677601 | 1.6183344 | 0.0889458 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2750339 |  97.866084 | 0.8227687 | 150.927551 | 1.5438872 | 0.2564620 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7863194 | 139.120623 | 0.7736972 | 131.537639 | 0.9658815 | 0.4907773 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1050866 |  14.888436 | 0.1033997 |  14.815858 | 0.1971691 | 0.9746937 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8970951 | 130.176883 | 0.8826946 | 148.877198 | 1.1425693 | 0.1840805 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1063286 |  14.866835 | 0.1046218 |  14.669780 | 0.1959170 | 0.9747229 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0669303 |   6.425446 | 0.0658559 |   6.288064 | 0.1232826 | 0.9914763 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0525750 |   3.759535 | 0.0517311 |   3.538254 | 0.1315558 | 0.9909278 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1026567 |  16.196020 | 0.1010088 |  16.769664 | 0.1888407 | 0.9763736 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4027056 |  45.100741 | 0.3962412 |  56.936124 | 0.5473577 | 0.9864819 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8662267 | 250.864328 | 0.8523217 |  96.976903 | 1.0343500 | 0.7091807 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1639485 | 366.454791 | 1.1452644 | 129.086852 | 1.3368387 | 0.1699266 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9367568 | 120.400517 | 0.9217197 | 147.979608 | 1.2020714 | 0.2516844 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9744598 |  99.936374 | 0.9588174 | 197.763220 | 1.2684252 | 0.3350084 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7314889 |  81.048675 | 0.6381235 |  94.417569 | 0.9604668 | 0.8716826 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1345887 |  12.377088 | 0.1174102 |  12.270446 | 0.2051390 | 0.9684418 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1629490 | 105.837069 | 1.0145131 | 179.033897 | 1.3979514 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1379935 |  13.434273 | 0.1203804 |  13.341724 | 0.2040434 | 0.9694823 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0581470 |   4.653700 | 0.0507252 |   4.669017 | 0.1195721 | 0.9891776 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2978831 |  29.665650 | 0.2598621 |  32.680822 | 0.4388693 | 0.8771806 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1418011 |  14.279969 | 0.1237020 |  14.076554 | 0.2018799 | 0.9706577 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4301156 |  65.181816 | 0.3752168 |  63.136513 | 0.6202803 | 0.8645173 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9103021 | 135.153865 | 0.7941134 |  71.758056 | 1.1907954 | 0.6117953 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3547882 | 169.351498 | 1.1818664 | 138.633763 | 1.5854495 | 0.1565234 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.1413311 | 116.389889 | 0.9956545 | 151.730792 | 1.3862120 | 0.1210839 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0158671 |  96.444363 | 0.8862044 | 150.708047 | 1.2370173 | 0.1156888 |
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
    ## 1 healthyR.da~         7 EARTH       Test  0.201   6.37 0.160   6.95 0.653 0.781
    ## 2 healthyR             7 EARTH       Test  0.0376  2.35 0.0385  2.27 0.104 0.994
    ## 3 healthyR.ts          7 EARTH       Test  0.128   5.98 0.0824  5.79 0.230 0.973
    ## 4 healthyverse         7 EARTH       Test  0.0669  6.43 0.0659  6.29 0.123 0.991
    ## 5 healthyR.ai          7 EARTH       Test  0.0581  4.65 0.0507  4.67 0.120 0.989

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
    ## 1 healthyR.data <tibble [371 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [361 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [312 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [286 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [101 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
