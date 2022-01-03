Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
03 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,147
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

The last day in the data set is 2022-01-01 23:16:38, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -798.64
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26147          |
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
| r\_version     |      17538 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17538 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17538 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2215 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-01 | 2021-08-04 |       405 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1528511.98 | 1877897.34 | 357 | 17597 | 238433 | 3246398 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8025.86 |   15246.15 |   1 |   202 |   2806 |    8195 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-01 23:16:38 | 2021-08-04 06:41:28 |     15232 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 48M 40S |        60 |

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
    ## 1 healthyR.data <tibble [376 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [366 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [316 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [291 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [105 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8420971 | 102.694004 | 0.6017248 |  87.774602 | 1.1968430 | 0.6278851 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2093778 |  10.646112 | 0.1496120 |  11.785042 | 0.7164933 | 0.7775795 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0104181 | 107.734889 | 0.7219994 |  99.564509 | 1.4466137 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2124406 |  10.249726 | 0.1518005 |  11.100293 | 0.7088931 | 0.7756753 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2069163 |   6.553241 | 0.1478531 |   7.094769 | 0.6610736 | 0.7907297 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2393060 |   7.793753 | 0.1709973 |   7.809710 | 0.7234119 | 0.7524648 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2093620 |   9.632366 | 0.1496007 |  10.423386 | 0.7021373 | 0.7816622 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4998455 |  38.617950 | 0.3571672 |  53.574639 | 0.9330785 | 0.7513676 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9464525 | 125.523467 | 0.6762925 |  98.229824 | 1.1206372 | 0.5897118 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2435393 | 149.791685 | 0.8885774 | 123.170395 | 1.4913017 | 0.1408375 |
| healthyR.data |         13 | BATS                       | Test  | 1.0117283 | 114.083155 | 0.7229356 |  97.314878 | 1.4457550 | 0.0119673 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0090785 | 106.560394 | 0.7210422 |  99.905027 | 1.4460511 | 0.2459863 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8384109 | 188.763366 | 0.7845735 | 123.979267 | 0.9445236 | 0.7227380 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0861752 |   8.927237 | 0.0806416 |   8.766060 | 0.2206570 | 0.9714055 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.9986573 | 196.621128 | 0.9345300 | 131.996994 | 1.2253701 | 0.0023109 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0874818 |  10.055544 | 0.0818643 |   9.748332 | 0.2195921 | 0.9714164 |
| healthyR      |          7 | EARTH                      | Test  | 0.0410535 |   2.534228 | 0.0384173 |   2.452330 | 0.1044888 | 0.9943941 |
| healthyR      |          8 | NNAR                       | Test  | 0.0551271 |   5.290835 | 0.0515872 |   5.189473 | 0.1576035 | 0.9885788 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0863302 |   8.285754 | 0.0807867 |   8.090166 | 0.2161338 | 0.9726284 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3852337 |  76.127792 | 0.3604965 |  82.247654 | 0.4961035 | 0.9850791 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0538877 | 241.624115 | 0.9862138 | 106.350587 | 1.3038295 | 0.5902062 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2493802 | 273.157458 | 1.1691531 | 126.832533 | 1.5214045 | 0.1475914 |
| healthyR      |         13 | TBATS                      | Test  | 0.9204290 | 152.487344 | 0.8613250 | 137.865222 | 1.1494869 | 0.0748873 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.9910350 | 192.695002 | 0.9273972 | 131.540615 | 1.2112851 | 0.4467286 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9549874 | 112.439128 | 0.5773630 | 102.477404 | 1.1481833 | 0.5837477 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1264393 |   7.267895 | 0.0764423 |   7.510596 | 0.3359267 | 0.9538580 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0842620 | 119.316760 | 0.6555194 | 112.303558 | 1.3258623 | 0.1075374 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1265258 |   7.374946 | 0.0764945 |   7.583716 | 0.3368040 | 0.9540667 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1093778 |   4.884710 | 0.0661273 |   4.769735 | 0.2120476 | 0.9779556 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1440260 |   7.726177 | 0.0870748 |   7.074575 | 0.2911249 | 0.9610101 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1299714 |   8.320073 | 0.0785776 |   8.418495 | 0.3330018 | 0.9552621 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5094412 |  39.341652 | 0.3079962 |  49.917498 | 0.6857238 | 0.9660148 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0903339 | 178.215503 | 0.6591903 |  99.178013 | 1.3200823 | 0.5999843 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4403518 | 215.140478 | 0.8708029 | 126.258890 | 1.6746081 | 0.0998793 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.2314774 | 105.607961 | 0.7445223 | 158.903084 | 1.4698396 | 0.1100576 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1218996 | 120.749127 | 0.6782742 | 113.196638 | 1.3995145 | 0.3328555 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8200279 | 206.611581 | 0.7469245 | 141.349147 | 1.0059710 | 0.5909490 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1063652 |  20.525546 | 0.0968830 |  22.492061 | 0.2009464 | 0.9786138 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9845230 | 257.474410 | 0.8967552 | 154.981170 | 1.1813486 | 0.1587232 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1197470 |  28.044510 | 0.1090718 |  28.181092 | 0.2085667 | 0.9784817 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0538457 |   5.417822 | 0.0490455 |   5.311860 | 0.1202571 | 0.9928198 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0443313 |   4.218658 | 0.0403792 |   3.998593 | 0.1176303 | 0.9924637 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1076019 |  22.851581 | 0.0980094 |  22.976728 | 0.1973616 | 0.9798096 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3912639 |  69.670659 | 0.3563837 |  76.050240 | 0.5307043 | 0.9859330 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8059498 | 328.176617 | 0.7341014 | 100.178879 | 0.9769806 | 0.7397972 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2298321 | 582.076346 | 1.1201956 | 134.351681 | 1.3971941 | 0.1798416 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8422092 | 111.639664 | 0.7671284 | 144.471429 | 1.0820160 | 0.5194155 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9853186 | 189.537548 | 0.8974799 | 152.072608 | 1.2467940 | 0.4381583 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7793936 |  99.647823 | 0.5862468 | 110.641897 | 0.9614244 | 0.6435507 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1495144 |  17.678623 | 0.1124622 |  20.354249 | 0.2188079 | 0.9682410 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1166261 | 100.133267 | 0.8399074 | 178.963526 | 1.3654513 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1502542 |  17.588059 | 0.1130187 |  20.065410 | 0.2211266 | 0.9672216 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0518166 |   3.779291 | 0.0389755 |   3.806844 | 0.1125033 | 0.9912365 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3385370 |  35.214770 | 0.2546419 |  46.859771 | 0.4371074 | 0.9378909 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1395955 |  17.111402 | 0.1050014 |  18.921595 | 0.2156666 | 0.9662999 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5625027 | 106.750357 | 0.4231051 |  84.283614 | 0.7274986 | 0.8601025 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9305122 | 150.224020 | 0.6999156 |  96.423165 | 1.1925933 | 0.5924614 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2678587 | 173.816937 | 0.9536620 | 131.184936 | 1.5150755 | 0.1681472 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0254926 | 111.079324 | 0.7713583 | 157.727655 | 1.2350144 | 0.1162505 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9847501 | 111.930906 | 0.7407125 | 132.680588 | 1.2062788 | 0.1708802 |
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
    ## 1 healthyR.da~         7 EARTH       Test  0.207   6.55 0.148   7.09 0.661 0.791
    ## 2 healthyR             7 EARTH       Test  0.0411  2.53 0.0384  2.45 0.104 0.994
    ## 3 healthyR.ts          7 EARTH       Test  0.109   4.88 0.0661  4.77 0.212 0.978
    ## 4 healthyverse         8 NNAR        Test  0.0443  4.22 0.0404  4.00 0.118 0.992
    ## 5 healthyR.ai          7 EARTH       Test  0.0518  3.78 0.0390  3.81 0.113 0.991

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
    ## 1 healthyR.data <tibble [376 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [366 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [316 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [291 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [105 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
