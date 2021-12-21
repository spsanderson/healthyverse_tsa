Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
21 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,469
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

The last day in the data set is 2021-12-19 18:48:01, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -482.16
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25469          |
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
| r\_version     |      17073 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17073 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17073 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2153 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-19 | 2021-08-03 |       392 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532674.46 | 1879429.42 | 357 | 24837 | 238654 | 3246351 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8109.28 |   15344.05 |   1 |   218 |   2854 |    8269 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-19 18:48:01 | 2021-08-03 09:27:45 |     14846 |

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
    ## 1 healthyR.data <tibble [363 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [353 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [304 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [278 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [93 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0699340 |  87.137739 | 0.7611208 | 110.143262 | 1.4787775 | 0.1868906 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2505789 |   9.371205 | 0.1782547 |  10.686180 | 0.7717579 | 0.7480734 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4681374 | 102.005614 | 1.0443915 | 194.908438 | 1.9046296 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2597909 |  11.236568 | 0.1848079 |  12.256933 | 0.7630745 | 0.7479515 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2449089 |   8.424151 | 0.1742213 |   8.791512 | 0.6466824 | 0.7857853 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2268824 |   7.456125 | 0.1613977 |   7.723119 | 0.6429160 | 0.7878304 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2502523 |   9.932102 | 0.1780224 |  11.142134 | 0.7647244 | 0.7525828 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7797727 |  54.727094 | 0.5547083 |  77.091149 | 1.1713846 | 0.7395462 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8133065 |  91.670862 | 0.5785633 |  68.404236 | 1.1468360 | 0.4839424 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5370400 | 118.733999 | 1.0934068 | 123.496771 | 2.0339359 | 0.0518264 |
| healthyR.data |         13 | BATS                       | Test  | 1.4643762 | 104.100859 | 1.0417159 | 187.812486 | 1.8912741 | 0.0254152 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4984105 | 103.984971 | 1.0659269 | 189.355065 | 1.9343785 | 0.1110113 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8008788 |  93.041327 | 0.8057524 | 103.305654 | 1.0133651 | 0.6029187 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1195965 |   9.239556 | 0.1203243 |   9.064968 | 0.2575173 | 0.9601341 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3635343 | 157.882236 | 1.3718317 | 170.102096 | 1.6674617 | 0.0053877 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1222372 |  10.483700 | 0.1229810 |   9.923436 | 0.2554578 | 0.9601882 |
| healthyR      |          7 | EARTH                      | Test  | 0.0806652 |   3.921574 | 0.0811560 |   3.720209 | 0.1695649 | 0.9907780 |
| healthyR      |          8 | NNAR                       | Test  | 0.1504659 |   8.038701 | 0.1513816 |   7.637231 | 0.3257032 | 0.9806526 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1162471 |   8.330858 | 0.1169545 |   8.330647 | 0.2527489 | 0.9623911 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4005615 |  39.528784 | 0.4029990 |  40.921382 | 0.5691991 | 0.9714270 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7116312 | 148.222397 | 0.7159617 |  79.517759 | 0.9479628 | 0.6571149 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4224273 | 239.943972 | 1.4310830 | 148.477794 | 1.6800669 | 0.1733696 |
| healthyR      |         13 | TBATS                      | Test  | 1.3512223 | 155.412710 | 1.3594448 | 170.672946 | 1.6556138 | 0.0105735 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3599572 | 148.248006 | 1.3682329 | 176.207624 | 1.6651898 | 0.0607086 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1585124 |  81.468123 | 0.8056855 | 125.701415 | 1.3905097 | 0.5330218 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1627273 |   9.243987 | 0.1131684 |   9.377589 | 0.3481197 | 0.9459834 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.7267742 | 125.851055 | 1.2008821 | 173.300365 | 1.9679547 | 0.1788045 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1627023 |   9.268022 | 0.1131510 |   9.378663 | 0.3460926 | 0.9461000 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1651109 |   7.750929 | 0.1148261 |   7.328231 | 0.2563762 | 0.9725607 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2031742 |   9.259049 | 0.1412971 |   8.505016 | 0.3639341 | 0.9538937 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1588154 |   9.132480 | 0.1104479 |   9.261064 | 0.3427212 | 0.9482127 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6875351 |  43.034054 | 0.4781451 |  55.573282 | 0.9092647 | 0.8866410 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9438359 | 121.164602 | 0.6563891 |  75.402637 | 1.1789970 | 0.6217160 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7416052 | 174.323234 | 1.2111963 | 133.113008 | 2.1250897 | 0.1227849 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.6634393 | 119.378867 | 1.1568360 | 171.528774 | 1.9051626 | 0.1710694 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.7793714 | 119.200475 | 1.2374608 | 177.738828 | 2.0609363 | 0.0086299 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8699376 |  91.660248 | 0.8268246 | 128.430592 | 1.0833727 | 0.6821999 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1165267 |  13.042563 | 0.1107518 |  13.468737 | 0.2157763 | 0.9720704 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3814545 | 185.709170 | 1.3129913 | 176.348508 | 1.6258455 | 0.2512811 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1160989 |  14.100593 | 0.1103452 |  15.300599 | 0.2155699 | 0.9720395 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1212393 |  10.343224 | 0.1152308 |   9.980983 | 0.1899674 | 0.9887445 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0746735 |   5.788302 | 0.0709728 |   5.498598 | 0.1775898 | 0.9871400 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1163996 |  16.592054 | 0.1106310 |  18.491129 | 0.2080978 | 0.9734057 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5948535 |  89.831154 | 0.5653733 |  87.331667 | 0.7460287 | 0.9719743 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6389997 | 212.318100 | 0.6073317 |  62.770085 | 0.9093086 | 0.7058278 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3833744 | 226.864321 | 1.3148161 | 138.903910 | 1.7024513 | 0.1652450 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3653529 | 186.278883 | 1.2976878 | 173.287166 | 1.6069938 | 0.2655319 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4474231 | 208.986801 | 1.3756906 | 176.853104 | 1.7243635 | 0.0850119 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9005970 |  78.680415 | 0.8319312 | 117.930360 | 1.1989968 | 0.3115595 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1162534 |   9.663407 | 0.1073897 |   9.461407 | 0.2149540 | 0.9731126 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.3302301 | 130.206549 | 1.2288071 | 179.374523 | 1.6084794 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1295965 |  12.690647 | 0.1197155 |  11.977483 | 0.2133200 | 0.9732946 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1056033 |   5.157088 | 0.0975516 |   5.498489 | 0.2625820 | 0.9711639 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3339183 |  23.098887 | 0.3084588 |  26.936496 | 0.5606561 | 0.9193209 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1361971 |  14.409710 | 0.1258128 |  13.238814 | 0.2120716 | 0.9734278 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5667175 |  40.489396 | 0.5235083 |  52.932012 | 0.8720189 | 0.7919758 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7556521 | 133.206073 | 0.6980377 |  68.859647 | 1.0947776 | 0.5510894 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4068402 | 185.533441 | 1.2995761 | 149.723307 | 1.7037873 | 0.1067260 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3057350 | 136.953213 | 1.2061797 | 180.732283 | 1.5583150 | 0.1443360 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3743366 | 138.380615 | 1.2695507 | 177.959429 | 1.6514187 | 0.0691293 |
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
    ## 1 healthyR~         8 NNAR           Test  0.227   7.46 0.161   7.72 0.643 0.788
    ## 2 healthyR          7 EARTH          Test  0.0807  3.92 0.0812  3.72 0.170 0.991
    ## 3 healthyR~         7 EARTH          Test  0.165   7.75 0.115   7.33 0.256 0.973
    ## 4 healthyv~         8 NNAR           Test  0.0747  5.79 0.0710  5.50 0.178 0.987
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.136  14.4  0.126  13.2  0.212 0.973

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
    ## 1 healthyR.data <tibble [363 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [353 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [304 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [278 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [93 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
