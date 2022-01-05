Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,177
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

The last day in the data set is 2022-01-03 18:40:06, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -842.03
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26177          |
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
| country        |       2220 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-03 | 2021-08-04 |       407 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1529969.30 | 1878442.90 | 357 | 17597 | 238636 | 3246446 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8020.01 |   15238.63 |   1 |   202 |   2806 |    8154 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-03 18:40:06 | 2021-08-04 07:28:01 |     15254 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 48M 14S |        60 |

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
    ## 1 healthyR.data <tibble [378 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [368 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [318 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [293 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [107 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.3563967 | 438.993955 | 1.0327991 | 109.509162 | 1.6216836 | 0.3873220 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1939348 |  16.895631 | 0.1476674 |  19.085366 | 0.6825617 | 0.7602965 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0447112 | 280.988319 | 0.7954729 | 102.934962 | 1.4177803 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1938389 |  16.813174 | 0.1475945 |  19.413223 | 0.6828741 | 0.7591354 |
| healthyR.data |          7 | EARTH                      | Test  | 0.1676275 |   5.207461 | 0.1276364 |   5.928908 | 0.6429551 | 0.7846374 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2059884 |   6.791779 | 0.1568454 |   7.218084 | 0.7326299 | 0.7073320 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1906819 |  14.693238 | 0.1451906 |  18.184589 | 0.6755681 | 0.7651052 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4596975 |  68.184662 | 0.3500267 |  64.087882 | 0.9060905 | 0.7503492 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0799524 | 358.893169 | 0.8223065 | 109.986740 | 1.2120176 | 0.5352891 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2936458 | 265.709343 | 0.9850188 | 117.907671 | 1.5564044 | 0.1485379 |
| healthyR.data |         13 | TBATS                      | Test  | 1.2587073 | 362.801174 | 0.9584156 | 108.990557 | 1.5409950 | 0.0473303 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0393486 | 278.294264 | 0.7913896 | 102.748592 | 1.4140869 | 0.1893071 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.9516119 | 247.403419 | 0.9208403 | 130.477875 | 1.0702774 | 0.6537242 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0761623 |   9.362173 | 0.0736994 |   9.200519 | 0.2067571 | 0.9685981 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.1545736 | 302.447311 | 1.1172389 | 133.027094 | 1.3448610 | 0.0500581 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0776972 |  10.583516 | 0.0751848 |  10.315734 | 0.2061200 | 0.9685903 |
| healthyR      |          7 | EARTH                      | Test  | 0.0215805 |   1.843692 | 0.0208827 |   1.818532 | 0.0525567 | 0.9977076 |
| healthyR      |          8 | NNAR                       | Test  | 0.1174290 |  12.975785 | 0.1136318 |  10.173181 | 0.3210705 | 0.9189127 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0792343 |  11.017684 | 0.0766722 |  10.629305 | 0.2011987 | 0.9700187 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4130674 |  95.431308 | 0.3997103 |  97.792316 | 0.5186191 | 0.9865745 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0924896 | 276.467986 | 1.0571625 | 112.239715 | 1.3147669 | 0.5728150 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3450299 | 357.234932 | 1.3015366 | 125.645793 | 1.6703887 | 0.1795941 |
| healthyR      |         13 | TBATS                      | Test  | 1.1739499 | 309.801182 | 1.1359887 | 133.510909 | 1.3506497 | 0.0761260 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.0993202 | 274.822935 | 1.0637722 | 132.522787 | 1.2955703 | 0.3511149 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8890505 | 125.314887 | 0.5543050 | 101.814673 | 1.0813875 | 0.7290497 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1260845 |   8.547721 | 0.0786112 |   8.541994 | 0.3318132 | 0.9575320 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1339914 | 166.731592 | 0.7070207 | 112.417666 | 1.3436682 | 0.1904645 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1268605 |   8.528666 | 0.0790950 |   8.558298 | 0.3333573 | 0.9577841 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0972903 |   4.351939 | 0.0606585 |   4.284765 | 0.2011693 | 0.9800654 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1045331 |   5.409325 | 0.0651743 |   5.147529 | 0.2348689 | 0.9738828 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1328457 |  10.080197 | 0.0828266 |   9.866877 | 0.3314920 | 0.9587549 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4808209 |  41.219392 | 0.2997821 |  55.804341 | 0.6586955 | 0.9683896 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1001308 | 210.563923 | 0.6859093 | 100.055348 | 1.2929718 | 0.6497526 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3530240 | 226.958739 | 0.8435832 | 117.922708 | 1.5928380 | 0.1823220 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0265016 | 106.862415 | 0.6400031 | 138.351904 | 1.2223474 | 0.4354143 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1554392 | 159.093518 | 0.7203930 | 114.830666 | 1.4152398 | 0.3888392 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7838444 | 245.996761 | 0.7305275 | 139.127446 | 0.9402644 | 0.6330000 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1008186 |  20.949571 | 0.0939610 |  22.868110 | 0.1918413 | 0.9776424 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.1750514 | 428.807454 | 1.0951248 | 143.204457 | 1.3499131 | 0.2474002 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1077840 |  24.344343 | 0.1004525 |  25.254132 | 0.1950353 | 0.9775724 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0368240 |   4.094936 | 0.0343192 |   3.991068 | 0.0913746 | 0.9939389 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0264305 |   2.509420 | 0.0246327 |   2.450291 | 0.0728118 | 0.9961252 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0961652 |  21.175034 | 0.0896240 |  18.219150 | 0.1846054 | 0.9791480 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4210820 | 104.222872 | 0.3924401 |  93.295472 | 0.5449424 | 0.9867052 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8046178 | 311.736423 | 0.7498879 |  99.578068 | 0.9775319 | 0.7432353 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3676976 | 738.800513 | 1.2746672 | 133.712714 | 1.5938132 | 0.2353831 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1080025 | 314.871012 | 1.0326365 | 157.914117 | 1.2852233 | 0.0249908 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.1163585 | 336.901636 | 1.0404242 | 142.295922 | 1.3542699 | 0.3548416 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8589549 | 104.446815 | 0.6161263 | 117.757110 | 1.0713333 | 0.9049264 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1410897 |  16.268112 | 0.1012033 |  19.424642 | 0.2136237 | 0.9754970 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.0749850 | 105.497986 | 0.7710842 | 159.906355 | 1.3017414 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1501002 |  17.225860 | 0.1076665 |  19.618261 | 0.2228858 | 0.9731478 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0541827 |   4.494212 | 0.0388651 |   4.462499 | 0.1128176 | 0.9923838 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3019039 |  28.550265 | 0.2165550 |  34.234071 | 0.4217599 | 0.9516856 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1294100 |  17.184049 | 0.0928255 |  17.913461 | 0.2148651 | 0.9741032 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7002834 | 129.446443 | 0.5023116 |  98.829913 | 0.8609575 | 0.8762176 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8908364 | 155.107413 | 0.6389948 |  90.437212 | 1.0615841 | 0.7027036 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0618661 | 181.846350 | 0.7616740 | 117.093582 | 1.2081779 | 0.2667827 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9588116 | 114.095048 | 0.6877533 | 133.651204 | 1.2000577 | 0.1149875 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0589328 | 121.198689 | 0.7595700 | 136.984475 | 1.2930250 | 0.2760437 |
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
    ## 1 healthyR.d~         7 EARTH       Test  0.168   5.21 0.128   5.93 0.643  0.785
    ## 2 healthyR            7 EARTH       Test  0.0216  1.84 0.0209  1.82 0.0526 0.998
    ## 3 healthyR.ts         7 EARTH       Test  0.0973  4.35 0.0607  4.28 0.201  0.980
    ## 4 healthyver~         8 NNAR        Test  0.0264  2.51 0.0246  2.45 0.0728 0.996
    ## 5 healthyR.ai         7 EARTH       Test  0.0542  4.49 0.0389  4.46 0.113  0.992

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
    ## 1 healthyR.data <tibble [378 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [368 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [318 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [293 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [107 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
