Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
13 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 24,836
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

The last day in the data set is 2021-12-11 23:16:47, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -294.64
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 24836          |
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
| r\_version     |      16660 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16660 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16660 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2080 |           0.92 |   2 |   2 |     0 |        98 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-11 | 2021-07-28 |       384 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532635.75 | 1878979.75 | 357 | 27186.25 | 238433 | 3245981 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8129.94 |   15283.19 |   1 |   221.00 |   2953 |    8369 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-11 23:16:47 | 2021-07-28 04:57:50 |     14502 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |      4 |        60 |

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
    ## 1 healthyR.data <tibble [355 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [345 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [296 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [270 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [85 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9539296 |  57.567730 | 0.7088053 |  88.962467 | 1.4575465 | 0.4155969 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2541406 |   9.523525 | 0.1888359 |  10.846551 | 0.7774458 | 0.7473263 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4427926 | 102.637585 | 1.0720487 | 196.968043 | 1.8896680 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2626111 |  11.542428 | 0.1951298 |  12.545023 | 0.7686725 | 0.7467686 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2442356 |   8.109690 | 0.1814762 |   8.474300 | 0.6540786 | 0.7831659 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2440196 |   9.281809 | 0.1813157 |   9.501993 | 0.6562000 | 0.7813927 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2493440 |   9.009910 | 0.1852719 |  10.344648 | 0.7731809 | 0.7531380 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7263989 |  47.241797 | 0.5397415 |  65.337488 | 1.1523135 | 0.7367867 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8851050 | 101.038423 | 0.6576661 |  80.148092 | 1.2465750 | 0.4372107 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6726175 | 147.876224 | 1.2428172 | 135.890822 | 2.1402675 | 0.0395912 |
| healthyR.data |         13 | TBATS                      | Test  | 1.3595302 |  88.840027 | 1.0101817 | 159.424066 | 1.8600389 | 0.0801192 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4785397 | 106.369285 | 1.0986102 | 193.948724 | 1.9241819 | 0.3072711 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7909597 |  76.747592 | 0.8184014 | 100.849276 | 1.0229837 | 0.7698802 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1204576 |   8.210677 | 0.1246368 |   8.263721 | 0.2590662 | 0.9630017 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3834211 | 132.017134 | 1.4314178 | 165.957880 | 1.7069661 | 0.0201388 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1229756 |   8.725321 | 0.1272422 |   8.721910 | 0.2569505 | 0.9629555 |
| healthyR      |          7 | EARTH                      | Test  | 0.0793219 |   3.828837 | 0.0820739 |   3.624663 | 0.1707017 | 0.9912367 |
| healthyR      |          8 | NNAR                       | Test  | 0.1604987 |  10.107350 | 0.1660670 |   9.058178 | 0.3342401 | 0.9744210 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1148449 |   7.680393 | 0.1188294 |   7.782079 | 0.2566435 | 0.9654386 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4065362 |  32.500794 | 0.4206407 |  40.784894 | 0.5853437 | 0.9704271 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7852603 | 142.503684 | 0.8125042 |  71.536272 | 1.0862024 | 0.6128620 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5628791 | 204.089960 | 1.6171018 | 140.613625 | 1.8773705 | 0.1430922 |
| healthyR      |         13 | TBATS                      | Test  | 1.4180421 | 139.203665 | 1.4672398 | 173.754828 | 1.7233165 | 0.0236852 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3604892 | 122.908632 | 1.4076902 | 172.185963 | 1.6701947 | 0.3214148 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0230404 |  87.263424 | 0.9100532 | 116.397702 | 1.3366456 | 0.6503122 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1606959 |   9.721212 | 0.1429482 |   9.826879 | 0.3498904 | 0.9510755 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6490150 | 164.016642 | 1.4668936 | 168.682850 | 1.9521849 | 0.1456142 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1610949 |   9.995584 | 0.1433031 |  10.053508 | 0.3483834 | 0.9511080 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1429222 |   6.980695 | 0.1271375 |   6.671368 | 0.2431659 | 0.9760373 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2131699 |  10.930258 | 0.1896269 |   9.869167 | 0.3811835 | 0.9550059 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1562484 |  10.111611 | 0.1389919 |  10.462318 | 0.3494619 | 0.9532313 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6773524 |  53.646471 | 0.6025438 |  76.989707 | 0.9287496 | 0.9046142 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0990650 | 253.740972 | 0.9776815 |  95.894626 | 1.3501959 | 0.5716431 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9620829 | 312.598400 | 1.7453854 | 148.727506 | 2.3187291 | 0.0907497 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5044802 | 137.042514 | 1.3383215 | 167.611858 | 1.8273906 | 0.3132129 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6534546 | 158.677894 | 1.4708428 | 181.112330 | 1.9790301 | 0.4257397 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8800849 |  85.809297 | 0.8143233 | 125.759668 | 1.0979510 | 0.7074176 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1167632 |  10.310503 | 0.1080385 |   9.994666 | 0.2167152 | 0.9736537 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.4137250 | 164.290903 | 1.3080888 | 173.771740 | 1.6534416 | 0.1795717 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1129237 |   9.403530 | 0.1044858 |   9.221203 | 0.2172038 | 0.9737605 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1190807 |   8.805075 | 0.1101828 |   8.293174 | 0.1900575 | 0.9889024 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0520331 |   3.627361 | 0.0481451 |   3.379552 | 0.1277264 | 0.9920367 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1074187 |   8.231319 | 0.0993922 |   8.111631 | 0.2115953 | 0.9751377 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5743702 |  53.698922 | 0.5314521 |  79.862581 | 0.7392861 | 0.9732105 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9394805 | 187.843452 | 0.8692808 |  94.322494 | 1.2097895 | 0.5662489 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7494285 | 296.816693 | 1.6187080 | 162.388602 | 2.0720052 | 0.0772534 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3579551 | 152.008559 | 1.2564862 | 167.051458 | 1.6337584 | 0.3516964 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4486992 | 151.681348 | 1.3404497 | 171.203346 | 1.7376931 | 0.3113371 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8932996 |  85.574772 | 0.8637177 | 119.119215 | 1.1906633 | 0.5598976 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1236027 |  12.255400 | 0.1195096 |  12.152576 | 0.2249203 | 0.9721093 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2735444 | 133.132811 | 1.2313705 | 179.115015 | 1.5704310 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1291965 |  14.288179 | 0.1249181 |  14.445627 | 0.2280026 | 0.9709831 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1028104 |   5.254968 | 0.0994058 |   5.574210 | 0.2564936 | 0.9747478 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3726153 |  30.767611 | 0.3602760 |  39.949117 | 0.5964935 | 0.9182045 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1335428 |  16.121605 | 0.1291204 |  17.479595 | 0.2253356 | 0.9704631 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5020447 |  36.385663 | 0.4854193 |  46.653238 | 0.8230060 | 0.8073059 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2342329 | 331.856745 | 1.1933608 | 105.733421 | 1.7416929 | 0.3749903 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5903355 | 324.113546 | 1.5376709 | 136.265095 | 1.9854483 | 0.0569139 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.3432343 | 148.188443 | 1.2987526 | 175.016607 | 1.6447095 | 0.0002229 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.2663075 | 131.531961 | 1.2243733 | 179.422702 | 1.5633091 | 0.3302740 |
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
    ## 1 healthyR.da~         7 EARTH       Test  0.244   8.11 0.181   8.47 0.654 0.783
    ## 2 healthyR             7 EARTH       Test  0.0793  3.83 0.0821  3.62 0.171 0.991
    ## 3 healthyR.ts          7 EARTH       Test  0.143   6.98 0.127   6.67 0.243 0.976
    ## 4 healthyverse         8 NNAR        Test  0.0520  3.63 0.0481  3.38 0.128 0.992
    ## 5 healthyR.ai          2 REGRESSION  Test  0.124  12.3  0.120  12.2  0.225 0.972

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
    ## 1 healthyR.data <tibble [355 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [345 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [296 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [270 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [85 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
