Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
22 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,506
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

The last day in the data set is 2021-12-20 23:15:41, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -510.62
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25506          |
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
| r\_version     |      17080 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17080 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17080 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2155 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-20 | 2021-08-03 |       393 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size           |          0 |              1 | 1533979.34 | 1879700.34 | 357 | 24837 | 238655 | 3246412.50 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8116.24 |   15360.74 |   1 |   219 |   2854 |    8281.75 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-20 23:15:41 | 2021-08-03 10:34:44 |     14876 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     45 |        60 |

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
    ## 1 healthyR.data <tibble [364 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [354 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [305 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [279 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [94 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0651174 |  87.779916 | 0.7801822 | 109.830666 | 1.4696575 | 0.2157322 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2525678 |   9.430079 | 0.1850020 |  10.754043 | 0.7741067 | 0.7455340 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4622287 | 100.682097 | 1.0710602 | 198.016705 | 1.8950786 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2618929 |  11.244423 | 0.1918326 |  12.281203 | 0.7654778 | 0.7453982 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2506023 |   8.823773 | 0.1835624 |   9.220067 | 0.6509449 | 0.7828241 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2147785 |   7.028239 | 0.1573220 |   7.308338 | 0.6242137 | 0.7994904 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2521266 |   9.943696 | 0.1846789 |  11.166564 | 0.7682045 | 0.7499880 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7570041 |  49.624533 | 0.5544939 |  68.554842 | 1.1611669 | 0.7372019 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8569549 |  95.762530 | 0.6277063 |  69.948217 | 1.1955349 | 0.4602077 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5339654 | 120.016278 | 1.1236062 | 124.253275 | 2.0389488 | 0.0553827 |
| healthyR.data |         13 | BATS                       | Test  | 1.4776125 | 106.288237 | 1.0823286 | 192.223277 | 1.8838182 | 0.0755936 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4924623 | 102.609519 | 1.0932058 | 191.692676 | 1.9241896 | 0.1378609 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8083796 |  99.894635 | 0.7883964 | 107.940712 | 1.0018231 | 0.5759998 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1187623 |   9.074592 | 0.1158265 |   8.927813 | 0.2584346 | 0.9616779 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.2308002 | 127.442900 | 1.2003748 | 166.190235 | 1.5496721 | 0.0102940 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1212975 |  10.238950 | 0.1182990 |   9.785605 | 0.2565374 | 0.9617229 |
| healthyR      |          7 | EARTH                      | Test  | 0.0790418 |   4.100229 | 0.0770879 |   3.910583 | 0.1612147 | 0.9911699 |
| healthyR      |          8 | NNAR                       | Test  | 0.1576244 |   7.428953 | 0.1537279 |   6.722571 | 0.3474412 | 0.9801423 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1156719 |   8.156719 | 0.1128125 |   8.169840 | 0.2537773 | 0.9638014 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3915844 |  42.518887 | 0.3819044 |  44.414508 | 0.5654949 | 0.9688555 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7968385 | 175.482999 | 0.7771406 |  93.617256 | 1.0028992 | 0.6358730 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3649968 | 234.914350 | 1.3312541 | 137.002317 | 1.6278858 | 0.1593108 |
| healthyR      |         13 | TBATS                      | Test  | 1.2245876 | 126.749273 | 1.1943158 | 166.496066 | 1.5374800 | 0.0365359 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.2598036 | 125.840571 | 1.2286612 | 176.901333 | 1.5777642 | 0.0972069 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1503091 |  87.110066 | 0.7762160 | 128.811632 | 1.3660816 | 0.5392948 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1599854 |   9.041634 | 0.1079564 |   9.193388 | 0.3488210 | 0.9483593 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6277880 | 116.981253 | 1.0984136 | 170.268753 | 1.8860529 | 0.1554288 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1599038 |   9.062243 | 0.1079014 |   9.199666 | 0.3475596 | 0.9484463 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1650517 |   7.731795 | 0.1113751 |   7.306320 | 0.2573200 | 0.9736248 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2160111 |  10.312374 | 0.1457619 |   9.272994 | 0.3836469 | 0.9534823 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1564625 |   8.936622 | 0.1055792 |   9.076154 | 0.3449215 | 0.9505495 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6892240 |  43.761022 | 0.4650808 |  56.227141 | 0.9178868 | 0.8918487 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9797442 | 136.429638 | 0.6611207 |  80.197820 | 1.2151784 | 0.5974241 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7132778 | 184.274867 | 1.1561011 | 135.085373 | 2.0807678 | 0.1005886 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.6267717 | 117.370194 | 1.0977278 | 172.678464 | 1.8775383 | 0.1637518 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6839793 | 109.242913 | 1.1363308 | 172.916658 | 1.9896423 | 0.0301731 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8985305 |  99.501628 | 0.8580256 | 135.346878 | 1.0852869 | 0.6256423 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1153449 |  12.995462 | 0.1101452 |  13.255308 | 0.2163327 | 0.9730220 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3188590 | 169.425032 | 1.2594061 | 170.920846 | 1.5863065 | 0.2240231 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1146221 |  14.590131 | 0.1094551 |  15.926877 | 0.2171085 | 0.9729828 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1174419 |   9.751206 | 0.1121477 |   9.339949 | 0.1956090 | 0.9892391 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0896710 |   6.307176 | 0.0856288 |   5.649384 | 0.2152987 | 0.9796668 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1149949 |  16.766050 | 0.1098110 |  18.610056 | 0.2093803 | 0.9744086 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5939219 |  89.970137 | 0.5671485 |  87.578297 | 0.7511758 | 0.9766106 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6583554 | 225.170316 | 0.6286774 |  67.103135 | 0.9358842 | 0.6781543 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4051670 | 253.842331 | 1.3418234 | 142.576804 | 1.7026080 | 0.1314853 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3148812 | 173.736843 | 1.2556076 | 167.987541 | 1.5727496 | 0.2834847 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.3754164 | 189.462968 | 1.3134140 | 170.882560 | 1.6786226 | 0.1183980 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9165164 |  85.855321 | 0.8534561 | 126.708388 | 1.2209026 | 0.2264090 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1129051 |   9.791567 | 0.1051367 |   9.696804 | 0.2157510 | 0.9742982 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2664123 | 122.113189 | 1.1792776 | 175.446731 | 1.5668849 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1205399 |  11.908321 | 0.1122463 |  11.665938 | 0.2144990 | 0.9744988 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1021253 |   5.117366 | 0.0950986 |   5.440799 | 0.2564769 | 0.9729642 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2794969 |  21.450953 | 0.2602663 |  23.801484 | 0.4977310 | 0.9338225 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1215511 |  12.832245 | 0.1131878 |  12.433206 | 0.2134283 | 0.9746391 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5045987 |  38.379284 | 0.4698801 |  52.527284 | 0.8189447 | 0.7999694 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8231935 | 163.593129 | 0.7665542 |  79.478375 | 1.1607507 | 0.5189490 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4558397 | 217.786675 | 1.3556715 | 156.309676 | 1.7357231 | 0.0908839 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3959763 | 161.959481 | 1.2999270 | 182.094375 | 1.6330204 | 0.0303746 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.2672415 | 122.272539 | 1.1800498 | 175.394299 | 1.5677296 | 0.0945552 |
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
    ##   package   .model_id .model_desc    .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>         <int> <chr>          <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR~         8 NNAR           Test  0.215   7.03 0.157   7.31 0.624 0.799
    ## 2 healthyR          7 EARTH          Test  0.0790  4.10 0.0771  3.91 0.161 0.991
    ## 3 healthyR~         7 EARTH          Test  0.165   7.73 0.111   7.31 0.257 0.974
    ## 4 healthyv~         7 EARTH          Test  0.117   9.75 0.112   9.34 0.196 0.989
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.122  12.8  0.113  12.4  0.213 0.975

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
    ## 1 healthyR.data <tibble [364 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [354 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [305 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [279 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [94 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
