Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,069
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

The last day in the data set is 2022-02-13 18:59:49, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1826.36
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29069          |
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
| r\_version     |      19302 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19302 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19302 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2418 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-13 | 2021-08-25 |       448 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1520451.3 | 1872272.3 | 357 | 23422 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8240.5 |   15705.5 |   1 |   325 |   2802 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-13 18:59:49 | 2021-08-25 16:32:28 |     16981 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 56M 44S |        60 |

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [390|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [381|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [330|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [306|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [120|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9393483 |  437.7288113 | 0.7156101 | 126.5762790 | 1.2290513 | 0.0607598 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0615337 |   51.6876500 | 0.0468773 |  16.8707804 | 0.0834061 | 0.9936629 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8894604 |   96.6421171 | 0.6776047 | 190.7362664 | 1.0323578 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0637327 |   44.8811433 | 0.0485525 |  16.5638953 | 0.0940358 | 0.9928643 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0635435 |   40.1968238 | 0.0484084 |  16.9797026 | 0.1378003 | 0.9817153 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0166916 |   10.8114259 | 0.0127159 |   9.6164582 | 0.0267794 | 0.9993153 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0708293 |   42.4313626 | 0.0539589 |  18.0232534 | 0.1045171 | 0.9909375 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5034163 |  340.1710779 | 0.3835103 |  78.5141065 | 0.6275563 | 0.9918851 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2400605 | 1484.5180516 | 0.9446972 | 117.4692746 | 1.5076439 | 0.3034464 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6065525 | 1750.1306371 | 1.2238965 | 145.1800536 | 1.8671471 | 0.0000139 |
| healthyR.data |         13 | BATS                       | Test  | 0.9221013 |  342.5277547 | 0.7024710 | 129.4699514 | 1.2002361 | 0.0028961 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8896495 |   98.7325169 | 0.6777487 | 195.4548815 | 1.0313285 | 0.0142070 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8436812 |  645.8937832 | 0.9105558 | 155.2878105 | 1.0325940 | 0.4117076 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0593877 |   37.8325653 | 0.0640951 |  20.1809181 | 0.0721108 | 0.9950690 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6029092 |  182.3061475 | 0.6506990 | 109.9169058 | 0.8565331 | 0.2439615 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0562716 |   31.7065101 | 0.0607320 |  17.8186955 | 0.0690817 | 0.9950608 |
| healthyR      |          7 | EARTH                      | Test  | 0.0327446 |   10.4322071 | 0.0353401 |  17.1266775 | 0.0736958 | 0.9940515 |
| healthyR      |          8 | NNAR                       | Test  | 0.0081732 |    2.7824404 | 0.0088210 |   3.6434622 | 0.0202562 | 0.9996558 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0626911 |   55.7197224 | 0.0676604 |  21.8517103 | 0.0767094 | 0.9939048 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4880581 |  488.0055239 | 0.5267442 | 106.5021879 | 0.5935361 | 0.9744008 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0473498 |  361.2711721 | 1.1303683 | 104.2427650 | 1.3401637 | 0.5467832 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0759318 |  456.6547958 | 1.1612158 | 123.8165558 | 1.3813793 | 0.1405573 |
| healthyR      |         13 | TBATS                      | Test  | 0.6368167 |  195.9817133 | 0.6872942 | 117.8477997 | 0.8491985 | 0.3078707 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7591900 |  322.4726303 | 0.8193674 | 150.3626268 | 0.9547822 | 0.0399453 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.3431821 | 1442.6994132 | 1.4023157 | 161.7969421 | 1.5484176 | 0.3712785 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0756022 |   96.2031862 | 0.0789305 |  21.0184238 | 0.0948066 | 0.9911459 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.3480204 | 1497.5025778 | 1.4073670 | 163.9146747 | 1.5579505 | 0.1429675 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0719000 |   87.1715104 | 0.0750654 |  22.3169051 | 0.0879615 | 0.9914399 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0150807 |    4.5108025 | 0.0157446 |   4.1854908 | 0.0213027 | 0.9994992 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0104271 |    7.9554261 | 0.0108862 |   5.7215035 | 0.0228358 | 0.9995948 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0802270 |   94.0805445 | 0.0837590 |  24.6939974 | 0.0990223 | 0.9896884 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7700295 |  959.9017936 | 0.8039301 | 138.9370230 | 0.8592842 | 0.9732197 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0479437 |  594.6495001 | 1.0940794 |  98.8459633 | 1.4208588 | 0.5306374 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4970574 | 1999.5051628 | 1.5629654 | 140.7808275 | 1.9209200 | 0.1747451 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0094134 |  897.5398940 | 1.0538528 | 144.5054358 | 1.2718643 | 0.2242259 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.3384662 | 1416.6740751 | 1.3973922 | 157.3170206 | 1.6127350 | 0.0230261 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.0036646 |  106.8383002 | 0.9590237 | 155.9688779 | 1.2396999 | 0.0724297 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0622351 |    7.5614059 | 0.0594670 |   7.2803518 | 0.0872965 | 0.9909966 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8261226 |  141.8136593 | 0.7893785 | 124.0639934 | 0.9834218 | 0.0301644 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0597381 |    7.1952615 | 0.0570810 |   6.8640209 | 0.0889659 | 0.9909613 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0201752 |    1.9197882 | 0.0192778 |   1.9299048 | 0.0428720 | 0.9986544 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0138509 |    0.9293358 | 0.0132348 |   0.9363637 | 0.0318257 | 0.9993407 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0881059 |   17.6209963 | 0.0841871 |  16.2630654 | 0.1012312 | 0.9910674 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5855951 |   75.5329490 | 0.5595490 |  90.0353694 | 0.7067753 | 0.9651276 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9434300 |  169.9286696 | 0.9014683 |  94.3071285 | 1.1529098 | 0.3553340 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2916074 |  234.6126817 | 1.2341594 | 124.5866046 | 1.6192702 | 0.0196256 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0018109 |  127.8955252 | 0.9572525 | 139.5037641 | 1.2087714 | 0.0015824 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7637500 |  138.9159961 | 0.7297801 |  97.5456144 | 0.9550600 | 0.0350596 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.6351889 |  616.1534176 | 0.6166201 | 126.2878489 | 0.7828056 | 0.2901182 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0733935 |   76.3057002 | 0.0712479 |  27.1536591 | 0.0879801 | 0.9927127 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8144501 |  472.1152966 | 0.7906409 | 164.1281139 | 0.9774297 | 0.0966571 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0658353 |   42.8632793 | 0.0639107 |  26.1157893 | 0.0836013 | 0.9926992 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0164527 |    2.3416458 | 0.0159717 |   2.3163001 | 0.0295144 | 0.9990373 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0310135 |   19.5002668 | 0.0301068 |  17.0624130 | 0.0594160 | 0.9960953 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0737814 |   90.7070316 | 0.0716245 |  29.3805626 | 0.0919411 | 0.9909677 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8314341 | 1360.4323562 | 0.8071284 | 152.0346483 | 0.9199019 | 0.9331968 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1297119 |  607.3041678 | 1.0966866 | 123.6566541 | 1.4613521 | 0.4167288 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2018946 |  683.6137013 | 1.1667591 | 140.6408466 | 1.6202297 | 0.0952433 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.7841833 |  585.2530608 | 0.7612590 | 153.4374040 | 0.9589726 | 0.1595159 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8482165 |  513.9415157 | 0.8234202 | 154.3042971 | 1.0800136 | 0.0192946 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |

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
    ##   package  .model_id .model_desc .type     mae   mape    mase smape   rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0167  10.8   0.0127  9.62  0.0268 0.999
    ## 2 healthyR         8 NNAR        Test  0.00817  2.78  0.00882 3.64  0.0203 1.00 
    ## 3 healthy~         7 EARTH       Test  0.0151   4.51  0.0157  4.19  0.0213 0.999
    ## 4 healthy~         8 NNAR        Test  0.0139   0.929 0.0132  0.936 0.0318 0.999
    ## 5 healthy~         7 EARTH       Test  0.0165   2.34  0.0160  2.32  0.0295 0.999

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [390|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [381|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [330|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [306|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [120|28]> <mdl_time_tbl>

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
