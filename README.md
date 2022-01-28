Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
28 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,758
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

The last day in the data set is 2022-01-26 23:52:06, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1399.23
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27758          |
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
| r\_version     |      18489 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18489 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18489 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2335 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-26 | 2021-08-15 |       430 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530173.95 | 1876683.94 | 357 | 21799.25 | 251093 | 3247923 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8130.42 |   15437.36 |   1 |   241.25 |   2798 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-26 23:52:06 | 2021-08-15 15:41:39 |     16250 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |      0 |        60 |

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
    ## 1 healthyR.data <tibble [401 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [391 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [341 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [316 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [130 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8229589 |  383.3207911 | 0.5733467 | 120.7017451 | 1.0218182 | 0.3546854 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0831607 |   17.9629901 | 0.0579372 |  20.1431121 | 0.1052749 | 0.9888419 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8343380 |  208.8143847 | 0.5812743 | 124.1652090 | 1.0486484 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0655857 |   20.4316815 | 0.0456928 |  17.5576119 | 0.0821620 | 0.9937309 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0304273 |   11.2165668 | 0.0211984 |   7.3672592 | 0.0458922 | 0.9980386 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0060025 |    3.1047699 | 0.0041819 |   4.0858795 | 0.0088739 | 0.9999302 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0665016 |   17.9986641 | 0.0463310 |  18.6064914 | 0.0906708 | 0.9920207 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5091382 |  219.3910911 | 0.3547111 |  92.2829224 | 0.6052616 | 0.9964288 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1976325 |  694.5517019 | 0.8343777 | 123.9694290 | 1.4323595 | 0.3314526 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4085341 |  669.8039289 | 0.9813106 | 139.2497593 | 1.7303103 | 0.0165407 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8419502 |  209.1498278 | 0.5865776 | 118.2283637 | 1.0846469 | 0.0261158 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8338938 |  208.5141403 | 0.5809648 | 124.2283729 | 1.0478345 | 0.0894982 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7297940 |  533.6204996 | 0.8790813 | 158.9045019 | 0.8418554 | 0.6152591 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0365469 |   22.5709449 | 0.0440230 |  18.4004303 | 0.0428220 | 0.9988316 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5160709 |  159.0053668 | 0.6216388 | 129.0340866 | 0.6807464 | 0.1195812 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0342669 |   22.2516025 | 0.0412766 |  20.7597150 | 0.0397729 | 0.9990100 |
| healthyR      |          7 | EARTH                      | Test  | 0.0181347 |    9.6380059 | 0.0218443 |   8.2942527 | 0.0202007 | 0.9995022 |
| healthyR      |          8 | NNAR                       | Test  | 0.0094651 |    4.0007461 | 0.0114013 |   4.1389605 | 0.0151055 | 0.9994773 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0397429 |   24.9017129 | 0.0478728 |  19.6969661 | 0.0487690 | 0.9980776 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5042615 |  469.8330694 | 0.6074137 | 143.3716774 | 0.5464432 | 0.9922143 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2216876 | 1259.2272055 | 1.4715971 | 125.7100395 | 1.4422411 | 0.5881471 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0711652 | 1278.9594892 | 1.2902837 | 122.7004808 | 1.2621011 | 0.3601299 |
| healthyR      |         13 | TBATS                      | Test  | 0.4891858 |  232.7101756 | 0.5892541 | 119.7806078 | 0.6344051 | 0.2077674 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6046446 |  199.9332413 | 0.7283313 | 181.6996733 | 0.7266500 | 0.0008939 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7463207 |  299.6711660 | 0.5743187 | 134.9936884 | 0.8924193 | 0.2402070 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0705533 |   99.2073354 | 0.0542931 |  27.6110922 | 0.0806995 | 0.9951263 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7185179 |  323.4880216 | 0.5529236 | 143.1163432 | 0.9180176 | 0.1870729 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0712710 |   85.3103957 | 0.0548454 |  28.1083154 | 0.0827826 | 0.9954864 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0318036 |    3.2914548 | 0.0244740 |   3.2223492 | 0.0750218 | 0.9959034 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0389382 |   18.5238074 | 0.0299642 |  16.3291079 | 0.0745829 | 0.9945433 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0774488 |  102.3833206 | 0.0595994 |  28.0040196 | 0.0980810 | 0.9939677 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4137815 |  425.6914196 | 0.3184187 |  97.4005137 | 0.4863700 | 0.9829519 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2468808 |  928.0830859 | 0.9595166 | 131.2160430 | 1.4597652 | 0.5282123 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2749470 | 1139.4791836 | 0.9811144 | 141.1228017 | 1.4875568 | 0.1604324 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7026564 |  316.3962948 | 0.5407177 | 133.2292423 | 0.8642301 | 0.2632832 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8031344 |  265.6860384 | 0.6180389 | 154.1707761 | 1.0075085 | 0.0511058 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8556910 |  291.9665249 | 0.8299775 | 172.4064139 | 0.9791481 | 0.4653598 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0599857 |   22.6402831 | 0.0581831 |  15.3029029 | 0.0694729 | 0.9967042 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6110560 |  255.5725105 | 0.5926938 | 130.5533791 | 0.7081574 | 0.0554674 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0547129 |   16.8416508 | 0.0530688 |  13.3475280 | 0.0631441 | 0.9968355 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0103070 |    3.3881345 | 0.0099973 |   3.1550479 | 0.0152410 | 0.9996913 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0028507 |    0.5955825 | 0.0027650 |   0.5981787 | 0.0046916 | 0.9999595 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0881259 |   30.7915424 | 0.0854777 |  31.9515559 | 0.0970992 | 0.9963276 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5535046 |  231.7148048 | 0.5368718 | 136.3406274 | 0.6077532 | 0.9877650 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0147611 |  627.1403250 | 0.9842676 | 116.3400119 | 1.1653251 | 0.3888546 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3204773 |  966.8889602 | 1.2807970 | 135.2246283 | 1.5106219 | 0.0682996 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5863592 |  164.9760895 | 0.5687391 | 125.7227188 | 0.7292239 | 0.1216440 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6096682 |  132.9657292 | 0.5913477 | 128.4616646 | 0.7622165 | 0.0037653 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0494129 |  457.2851396 | 0.6542491 | 126.6636135 | 1.2813774 | 0.4911207 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0547730 |   27.3789254 | 0.0341478 |  16.6426669 | 0.0650765 | 0.9981796 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9176175 |  243.9243169 | 0.5720822 | 129.9489505 | 1.1326889 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0512272 |   21.4104733 | 0.0319372 |  14.2203593 | 0.0605671 | 0.9983655 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0141266 |    2.7185625 | 0.0088071 |   2.5098320 | 0.0202588 | 0.9996929 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0770797 |   17.4216305 | 0.0480548 |  17.8699829 | 0.1108828 | 0.9909504 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0666496 |   41.4424481 | 0.0415522 |  21.2442197 | 0.0807071 | 0.9969004 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7924577 |  453.3312194 | 0.4940522 | 123.6932473 | 0.9153404 | 0.9653857 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1261427 |  432.7976487 | 0.7020858 | 120.1064592 | 1.3148610 | 0.4941214 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3870681 |  543.2487946 | 0.8647579 | 146.6404457 | 1.5298698 | 0.0921235 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9300160 |  358.0597398 | 0.5798119 | 133.6994742 | 1.1255660 | 0.0498608 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9848887 |  330.4186986 | 0.6140220 | 128.3996196 | 1.2125613 | 0.0272021 |
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
    ##   package      .model_id .model_desc .type     mae   mape    mase  smape    rmse
    ##   <chr>            <int> <chr>       <chr>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
    ## 1 healthyR.da~         8 NNAR        Test  0.00600  3.10  0.00418  4.09  0.00887
    ## 2 healthyR             8 NNAR        Test  0.00947  4.00  0.0114   4.14  0.0151 
    ## 3 healthyR.ts          8 NNAR        Test  0.0389  18.5   0.0300  16.3   0.0746 
    ## 4 healthyverse         8 NNAR        Test  0.00285  0.596 0.00277  0.598 0.00469
    ## 5 healthyR.ai          7 EARTH       Test  0.0141   2.72  0.00881  2.51  0.0203 
    ## # ... with 1 more variable: rsq <dbl>

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
    ## 1 healthyR.data <tibble [401 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [391 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [341 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [316 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [130 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
