Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
23 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,593
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

The last day in the data set is 2021-12-21 23:22:30, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -534.73
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25593          |
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
| r\_version     |      17147 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17147 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17147 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2163 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-21 | 2021-08-03 |       394 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533581.8 | 1879477.85 | 357 | 24837 | 238655 | 3246413 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8100.5 |   15345.09 |   1 |   221 |   2834 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-21 23:22:30 | 2021-08-03 22:42:11 |     14926 |

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
    ## 1 healthyR.data <tibble [365 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [355 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [306 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [280 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [95 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9552103 |  79.256153 | 0.7326657 |  85.637872 | 1.3824084 | 0.4198298 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2523329 |   9.396866 | 0.1935445 |  10.733787 | 0.7788447 | 0.7412388 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4174820 |  98.074787 | 1.0872375 | 187.335289 | 1.8539707 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2644230 |  11.382335 | 0.2028178 |  12.429201 | 0.7706298 | 0.7407323 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2330478 |   7.741822 | 0.1787524 |   8.294641 | 0.6531006 | 0.7828704 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2231463 |   7.560312 | 0.1711578 |   7.663269 | 0.6214191 | 0.8006360 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2521004 |   9.927526 | 0.1933661 |  11.163324 | 0.7726268 | 0.7455906 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7310895 |  48.681729 | 0.5607605 |  66.675494 | 1.1260695 | 0.7637604 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8230952 |  91.422707 | 0.6313308 |  66.177756 | 1.1656054 | 0.4760610 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4960075 | 119.879017 | 1.1474681 | 118.000298 | 2.0126129 | 0.0520636 |
| healthyR.data |         13 | BATS                       | Test  | 1.4037368 |  94.300515 | 1.0766947 | 174.772279 | 1.8583274 | 0.0497487 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4469028 |  99.974294 | 1.1098038 | 197.754846 | 1.8814985 | 0.1082352 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7133898 | 102.167848 | 0.7039683 |  91.436007 | 0.9294868 | 0.6573588 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1207559 |   9.342827 | 0.1191612 |   9.183465 | 0.2614550 | 0.9600690 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.0713759 |  97.706947 | 1.0572266 | 160.352674 | 1.3838128 | 0.0087625 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1240052 |  10.630791 | 0.1223675 |  10.136983 | 0.2597416 | 0.9600402 |
| healthyR      |          7 | EARTH                      | Test  | 0.0674846 |   3.180211 | 0.0665934 |   3.025690 | 0.1597723 | 0.9911945 |
| healthyR      |          8 | NNAR                       | Test  | 0.1399528 |   7.012429 | 0.1381045 |   6.353390 | 0.2908133 | 0.9871779 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1166964 |   8.391630 | 0.1151552 |   8.385931 | 0.2566951 | 0.9621474 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3731903 |  44.519708 | 0.3682617 |  45.133242 | 0.5587633 | 0.9655254 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8253695 | 183.117850 | 0.8144691 | 100.657140 | 1.0293721 | 0.6212195 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2199256 | 210.190933 | 1.2038145 | 128.127584 | 1.4523927 | 0.1627179 |
| healthyR      |         13 | TBATS                      | Test  | 1.0721012 | 100.502464 | 1.0579424 | 161.748575 | 1.3689151 | 0.0116614 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.1128325 |  99.348585 | 1.0981357 | 196.848441 | 1.4167636 | 0.0708777 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0666767 |  80.130948 | 0.7125372 | 114.947829 | 1.3000861 | 0.7054313 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1607977 |   9.023379 | 0.1074124 |   9.191699 | 0.3518785 | 0.9478761 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.4887878 | 107.752440 | 0.9945062 | 175.265456 | 1.7303125 | 0.1588462 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1608355 |   9.090815 | 0.1074377 |   9.246343 | 0.3512566 | 0.9479520 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1446277 |   6.792746 | 0.0966109 |   6.511390 | 0.2373906 | 0.9740589 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.2057702 |   9.628655 | 0.1374540 |   8.434883 | 0.4152396 | 0.9609715 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1593312 |   9.193733 | 0.1064328 |   9.326232 | 0.3497786 | 0.9497873 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6937245 |  43.877729 | 0.4634061 |  56.610070 | 0.8908942 | 0.9486577 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0071056 | 137.106399 | 0.6727438 |  84.311601 | 1.2217611 | 0.6068215 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5723088 | 174.318114 | 1.0502981 | 129.939145 | 1.9345494 | 0.0973642 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5128888 | 108.349525 | 1.0106057 | 170.299013 | 1.7712036 | 0.1363980 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.5761154 | 101.124314 | 1.0528408 | 177.826549 | 1.8740173 | 0.0167876 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8257681 | 112.342316 | 0.7954737 | 132.859733 | 1.0269545 | 0.6971662 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1164079 |  13.008194 | 0.1121374 |  13.285129 | 0.2195659 | 0.9719488 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.2065086 | 144.209140 | 1.1622462 | 169.123770 | 1.4747508 | 0.2157958 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1155323 |  14.826469 | 0.1112938 |  16.568234 | 0.2209459 | 0.9718584 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0873481 |   8.037329 | 0.0841436 |   7.887328 | 0.1438982 | 0.9907623 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0746049 |   5.864141 | 0.0718679 |   5.744868 | 0.1791021 | 0.9790506 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1158438 |  17.813402 | 0.1115939 |  18.663140 | 0.2152175 | 0.9729085 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5489037 |  82.712859 | 0.5287665 |  80.694527 | 0.7186988 | 0.9686241 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6827908 | 232.130679 | 0.6577417 |  69.378083 | 0.9741413 | 0.6572401 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2971331 | 261.925481 | 1.2495461 | 133.259917 | 1.6154678 | 0.1219581 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1842496 | 149.830681 | 1.1408038 | 163.829312 | 1.4600176 | 0.1742380 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.2723264 | 169.629443 | 1.2256494 | 172.410586 | 1.5765103 | 0.0878357 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8735076 |  87.083455 | 0.8128091 | 116.871830 | 1.2156250 | 0.0451628 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1165253 |   9.705606 | 0.1084281 |   9.617380 | 0.2214366 | 0.9724673 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2217485 | 118.864630 | 1.1368513 | 176.626958 | 1.5193818 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1228071 |  11.502344 | 0.1142735 |  11.266231 | 0.2196729 | 0.9728213 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0998567 |   5.304261 | 0.0929178 |   5.595747 | 0.2528191 | 0.9719733 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2284484 |  20.500591 | 0.2125739 |  23.923292 | 0.3774904 | 0.9453557 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1236128 |  12.382757 | 0.1150231 |  11.987382 | 0.2194852 | 0.9727458 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4252448 |  41.181025 | 0.3956953 |  49.557084 | 0.7395761 | 0.8004105 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7948566 | 159.154333 | 0.7396234 |  69.738276 | 1.1216739 | 0.5518701 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4101730 | 215.512665 | 1.3121825 | 153.018332 | 1.7019551 | 0.0971557 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.3153705 | 132.299695 | 1.2239677 | 174.022732 | 1.6518705 | 0.1595430 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.1582453 | 108.883256 | 1.0777608 | 185.519247 | 1.4535401 | 0.0550348 |
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
    ## 1 healthyR~         8 NNAR           Test  0.223   7.56 0.171   7.66 0.621 0.801
    ## 2 healthyR          7 EARTH          Test  0.0675  3.18 0.0666  3.03 0.160 0.991
    ## 3 healthyR~         7 EARTH          Test  0.145   6.79 0.0966  6.51 0.237 0.974
    ## 4 healthyv~         7 EARTH          Test  0.0873  8.04 0.0841  7.89 0.144 0.991
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.124  12.4  0.115  12.0  0.219 0.973

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
    ## 1 healthyR.data <tibble [365 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [355 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [306 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [280 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [95 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
