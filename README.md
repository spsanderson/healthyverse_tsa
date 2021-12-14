Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
14 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 24,869
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

The last day in the data set is 2021-12-12 22:28:53, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -317.84
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 24869          |
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
| r\_version     |      16667 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16667 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16667 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2100 |           0.92 |   2 |   2 |     0 |        98 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-12 | 2021-07-28 |       385 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533360.10 | 1879117.79 | 357 | 27381 | 238439 | 3246087 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8123.57 |   15277.55 |   1 |   219 |   2930 |    8369 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-12 22:28:53 | 2021-07-28 08:24:26 |     14528 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |     median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-----------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 46M 5S |        60 |

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
    ## 1 healthyR.data <tibble [356 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [346 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [297 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [271 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [86 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9728897 |  63.904568 | 0.7106224 |  90.468450 | 1.4526987 | 0.3382650 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2554570 |   9.669181 | 0.1865921 |  10.987155 | 0.7778745 | 0.7571923 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4299037 | 100.902679 | 1.0444367 | 198.309453 | 1.8777431 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2621797 |  11.358738 | 0.1915025 |  12.374387 | 0.7690183 | 0.7566651 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2471694 |   8.359179 | 0.1805386 |   8.739028 | 0.6544931 | 0.7925571 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2443725 |   8.316316 | 0.1784957 |   8.094483 | 0.6542053 | 0.7938782 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2522093 |   9.424057 | 0.1842198 |  10.736575 | 0.7739820 | 0.7623065 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7483031 |  48.994542 | 0.5465789 |  67.839190 | 1.1720657 | 0.7471851 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7780802 |  85.860837 | 0.5683288 |  70.485789 | 1.1040225 | 0.5514268 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5588433 | 130.880012 | 1.1386172 | 131.687141 | 1.9659376 | 0.0745378 |
| healthyR.data |         13 | BATS                       | Test  | 1.3718342 |  90.738543 | 1.0020212 | 171.594168 | 1.8666704 | 0.0891541 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4614060 | 103.877953 | 1.0674467 | 193.810611 | 1.9102435 | 0.1905054 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7905026 |  80.150756 | 0.7962217 |  96.825403 | 1.0154394 | 0.7205630 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1213127 |   8.160239 | 0.1221903 |   8.216398 | 0.2594810 | 0.9637934 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.2821644 | 114.391720 | 1.2914405 | 157.693907 | 1.6224968 | 0.0223309 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1231277 |   8.518125 | 0.1240185 |   8.533651 | 0.2573764 | 0.9637900 |
| healthyR      |          7 | EARTH                      | Test  | 0.0794489 |   3.830958 | 0.0800237 |   3.627123 | 0.1707562 | 0.9914754 |
| healthyR      |          8 | NNAR                       | Test  | 0.1827223 |   9.262147 | 0.1840443 |   8.225159 | 0.3828232 | 0.9759425 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1180285 |   8.090880 | 0.1188824 |   8.160602 | 0.2554248 | 0.9660455 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3909830 |  33.209529 | 0.3938117 |  44.913753 | 0.5694074 | 0.9711219 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7500021 | 122.773142 | 0.7554282 |  77.690106 | 0.9477844 | 0.7028444 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3763847 | 164.545301 | 1.3863425 | 139.163591 | 1.6067822 | 0.2020525 |
| healthyR      |         13 | TBATS                      | Test  | 1.2334809 | 121.011531 | 1.2424048 | 163.442683 | 1.5299715 | 0.0840812 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3059632 | 110.931897 | 1.3154114 | 168.700331 | 1.6229677 | 0.2073073 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0199621 |  94.022216 | 0.8773862 | 114.151071 | 1.3267556 | 0.5824710 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1614428 |  10.001576 | 0.1388755 |  10.084472 | 0.3504218 | 0.9513713 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.5517910 | 135.892676 | 1.3348732 | 167.897496 | 1.8560049 | 0.1509395 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1616826 |  10.166000 | 0.1390817 |  10.211723 | 0.3488718 | 0.9514089 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1429611 |   6.913745 | 0.1229772 |   6.598447 | 0.2432616 | 0.9762604 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1980682 |   8.489463 | 0.1703812 |   7.689676 | 0.3808987 | 0.9555193 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1572807 |  10.249760 | 0.1352951 |  10.315461 | 0.3472190 | 0.9535186 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6779564 |  52.549949 | 0.5831879 |  72.206497 | 0.9294139 | 0.9054600 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0598538 | 221.106930 | 0.9117016 |  95.770278 | 1.2643719 | 0.6214450 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7813201 | 260.823710 | 1.5323174 | 142.620765 | 2.0913601 | 0.1051831 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.4632394 | 118.479661 | 1.2586998 | 162.869506 | 1.7957497 | 0.2852790 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6140693 | 141.442545 | 1.3884459 | 177.605528 | 1.9476286 | 0.3124100 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8638354 |  79.334869 | 0.7847132 | 115.059437 | 1.0871313 | 0.6779896 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1181439 |   9.716696 | 0.1073226 |   9.494674 | 0.2175133 | 0.9740197 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3589084 | 130.992969 | 1.2344403 | 167.838157 | 1.6189786 | 0.2149166 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1151137 |   9.152878 | 0.1045699 |   9.004212 | 0.2183204 | 0.9741171 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1172765 |   8.080387 | 0.1065346 |   7.575258 | 0.1958541 | 0.9892236 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0867470 |   5.011372 | 0.0788015 |   4.501797 | 0.1988113 | 0.9873517 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1113035 |   8.624058 | 0.1011088 |   8.456466 | 0.2133734 | 0.9753919 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6265140 |  58.918524 | 0.5691289 |  92.724991 | 0.7881513 | 0.9754862 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8136913 | 112.180514 | 0.7391620 |  91.329584 | 1.0154423 | 0.6651933 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5578396 | 184.352723 | 1.4151506 | 151.202033 | 1.7949436 | 0.1161474 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3523397 | 132.913376 | 1.2284732 | 167.141115 | 1.6211445 | 0.2770569 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4195346 | 132.177229 | 1.2895135 | 165.815518 | 1.7181020 | 0.2025230 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9002619 |  82.665192 | 0.8527915 | 119.499502 | 1.1912970 | 0.5858939 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1272664 |  12.643631 | 0.1205557 |  12.480312 | 0.2261931 | 0.9722417 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2658268 | 122.420661 | 1.1990804 | 174.999944 | 1.5648404 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1298229 |  13.726467 | 0.1229774 |  14.004785 | 0.2290716 | 0.9715452 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1054205 |   5.152395 | 0.0998617 |   5.491625 | 0.2638310 | 0.9737599 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.4489990 |  35.577199 | 0.4253235 |  46.738438 | 0.6753118 | 0.8918219 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1308757 |  14.876016 | 0.1239747 |  16.692828 | 0.2272434 | 0.9712213 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4856714 |  35.448004 | 0.4600622 |  46.310028 | 0.8027552 | 0.8137715 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0807689 | 219.848815 | 1.0237805 | 103.187351 | 1.4775832 | 0.4756734 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4967866 | 214.944580 | 1.4178618 | 139.373831 | 1.7899756 | 0.0880651 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.3284290 | 135.425761 | 1.2583816 | 167.601590 | 1.6425175 | 0.0395019 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.2457036 | 118.539055 | 1.1800183 | 176.504368 | 1.5434147 | 0.2260780 |
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
    ## 1 healthyR.da~         8 NNAR        Test  0.244   8.32 0.178   8.09 0.654 0.794
    ## 2 healthyR             7 EARTH       Test  0.0794  3.83 0.0800  3.63 0.171 0.991
    ## 3 healthyR.ts          7 EARTH       Test  0.143   6.91 0.123   6.60 0.243 0.976
    ## 4 healthyverse         7 EARTH       Test  0.117   8.08 0.107   7.58 0.196 0.989
    ## 5 healthyR.ai          2 REGRESSION  Test  0.127  12.6  0.121  12.5  0.226 0.972

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
    ## 1 healthyR.data <tibble [356 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [346 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [297 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [271 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [86 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
