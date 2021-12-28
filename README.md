Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
28 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,862
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

The last day in the data set is 2021-12-26 22:10:29, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -653.53
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25862          |
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
| r\_version     |      17347 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17347 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17347 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2190 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-26 | 2021-08-04 |       399 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530225.9 | 1878260.21 | 357 | 21806.25 | 238637 | 3246377 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8086.2 |   15314.87 |   1 |   202.00 |   2823 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-26 22:10:29 | 2021-08-04 02:40:46 |     15074 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   47.5 |        60 |

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
    ## 1 healthyR.data <tibble [370 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [360 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [311 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [285 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [100 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9335728 |  97.613925 | 0.7833875 |  91.172803 | 1.2499760 | 0.2222965 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2574972 |  16.435038 | 0.2160732 |  16.817349 | 0.7023262 | 0.7573544 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.1189937 |  89.250836 | 0.9389795 | 126.905112 | 1.5271380 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2427026 |  13.832854 | 0.2036587 |  14.427623 | 0.7039034 | 0.7570779 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2150028 |   7.272863 | 0.1804149 |   8.016544 | 0.6673257 | 0.7746431 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2608791 |   9.807237 | 0.2189111 |   9.308149 | 0.6835707 | 0.7669743 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2380834 |  13.114185 | 0.1997826 |  13.555322 | 0.6951626 | 0.7642565 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5352026 |  37.000024 | 0.4491037 |  47.025685 | 0.9553570 | 0.7343587 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0597380 | 142.447495 | 0.8892563 |  93.520757 | 1.2281809 | 0.5605441 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1970941 | 126.876101 | 1.0045158 | 111.620700 | 1.5126045 | 0.1471261 |
| healthyR.data |         13 | BATS                       | Test  | 1.1440562 |  91.193508 | 0.9600101 | 137.702975 | 1.5501306 | 0.0020494 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.1300616 |  89.284566 | 0.9482669 | 130.595805 | 1.5359536 | 0.2176823 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7391846 | 116.155705 | 0.8049085 | 107.259993 | 0.8947659 | 0.5246739 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0968715 |   9.148432 | 0.1054848 |   8.870730 | 0.2203281 | 0.9662480 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.9310945 | 118.855796 | 1.0138819 | 140.777093 | 1.1742757 | 0.0190021 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1025970 |  11.278178 | 0.1117193 |  10.486190 | 0.2195040 | 0.9662778 |
| healthyR      |          7 | EARTH                      | Test  | 0.0428798 |   2.509192 | 0.0466924 |   2.432709 | 0.1042697 | 0.9933974 |
| healthyR      |          8 | NNAR                       | Test  | 0.0853149 |   7.364127 | 0.0929006 |   7.199355 | 0.1888263 | 0.9858802 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1042055 |  12.577330 | 0.1134708 |  11.396497 | 0.2144591 | 0.9681241 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3405155 |  54.961458 | 0.3707921 |  57.969406 | 0.4610320 | 0.9836302 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9439001 | 196.054305 | 1.0278261 |  97.860394 | 1.2366665 | 0.5942803 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1406848 | 191.034537 | 1.2421076 | 120.932750 | 1.4373329 | 0.1414910 |
| healthyR      |         13 | TBATS                      | Test  | 0.9500775 |  94.975558 | 1.0345527 | 142.407333 | 1.2249150 | 0.0326812 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.9440591 | 109.093835 | 1.0279992 | 136.943327 | 1.1989383 | 0.3546343 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 1.0058401 |  99.435199 | 0.6866330 | 109.884209 | 1.2015616 | 0.3850343 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1331556 |   7.533145 | 0.0908982 |   7.737403 | 0.3328118 | 0.9493676 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2010419 |  92.917839 | 0.8198868 | 137.533618 | 1.4446876 | 0.1153519 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1324611 |   7.415459 | 0.0904241 |   7.609743 | 0.3307109 | 0.9494640 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1290159 |   6.034841 | 0.0880722 |   5.840502 | 0.2276847 | 0.9741319 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1536187 |   8.320085 | 0.1048672 |   7.588677 | 0.3031410 | 0.9570446 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1340783 |   8.211792 | 0.0915281 |   8.321481 | 0.3255306 | 0.9517045 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5662833 |  39.328216 | 0.3865712 |  50.807848 | 0.7250220 | 0.9572127 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1990853 | 171.148920 | 0.8185511 |  99.706817 | 1.3935635 | 0.5787391 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4438373 | 185.486473 | 0.9856301 | 129.610756 | 1.7353657 | 0.1105013 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.4505576 | 111.751287 | 0.9902178 | 165.782341 | 1.7066226 | 0.0445898 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.3265050 |  98.572047 | 0.9055337 | 169.400400 | 1.5962011 | 0.2529192 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8554771 | 123.838491 | 0.9147481 | 134.992770 | 1.0318647 | 0.3700750 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1079486 |  15.217895 | 0.1154277 |  15.564072 | 0.1975256 | 0.9745427 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9583580 | 103.966654 | 1.0247570 | 151.040654 | 1.2177724 | 0.1873227 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1088933 |  14.306554 | 0.1164379 |  14.232924 | 0.1954426 | 0.9746536 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0746978 |   6.822760 | 0.0798732 |   6.638783 | 0.1323775 | 0.9912807 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0594325 |   5.008515 | 0.0635503 |   4.720729 | 0.1617041 | 0.9876149 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1068082 |  15.491775 | 0.1142083 |  16.006874 | 0.1885673 | 0.9762703 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4172247 |  45.675321 | 0.4461317 |  59.051947 | 0.5583345 | 0.9857958 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8324712 | 240.196569 | 0.8901482 |  95.181374 | 0.9840311 | 0.7330103 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1274601 | 334.930788 | 1.2055752 | 131.118270 | 1.3231681 | 0.1821308 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1372430 | 161.361381 | 1.2160359 | 154.773884 | 1.3944247 | 0.0812060 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.0384658 | 112.520013 | 1.1104149 | 170.182031 | 1.3352789 | 0.3910875 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7380766 |  81.527324 | 0.6933471 |  95.435057 | 0.9642642 | 0.8798439 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1324503 |  12.230193 | 0.1244235 |  12.109993 | 0.2027260 | 0.9692465 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1745606 | 107.248581 | 1.1033789 | 176.648644 | 1.4122246 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1411670 |  14.387255 | 0.1326119 |  14.282227 | 0.2035291 | 0.9701929 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0658050 |   5.185608 | 0.0618170 |   5.183390 | 0.1301513 | 0.9870770 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3312157 |  29.940725 | 0.3111431 |  33.785225 | 0.4632997 | 0.8616677 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1457113 |  15.523155 | 0.1368808 |  15.574315 | 0.2012650 | 0.9717815 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4008542 |  57.150656 | 0.3765613 |  58.739639 | 0.6007648 | 0.8690692 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8770222 | 130.003324 | 0.8238722 |  71.349386 | 1.1491131 | 0.6204149 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3575130 | 166.914757 | 1.2752439 | 138.442796 | 1.5904797 | 0.1565870 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.1860253 | 130.367006 | 1.1141488 | 144.160949 | 1.4411558 | 0.0950715 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0535107 |  96.535151 | 0.9896650 | 168.322069 | 1.2758324 | 0.1635582 |
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
    ## 1 healthyR.da~         7 EARTH       Test  0.215   7.27 0.180   8.02 0.667 0.775
    ## 2 healthyR             7 EARTH       Test  0.0429  2.51 0.0467  2.43 0.104 0.993
    ## 3 healthyR.ts          7 EARTH       Test  0.129   6.03 0.0881  5.84 0.228 0.974
    ## 4 healthyverse         7 EARTH       Test  0.0747  6.82 0.0799  6.64 0.132 0.991
    ## 5 healthyR.ai          7 EARTH       Test  0.0658  5.19 0.0618  5.18 0.130 0.987

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
    ## 1 healthyR.data <tibble [370 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [360 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [311 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [285 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [100 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
