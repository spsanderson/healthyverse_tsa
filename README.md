Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
27 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,796
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

The last day in the data set is 2021-12-25 23:46:40, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -631.14
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25796          |
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
| r\_version     |      17282 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17282 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17282 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2189 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-25 | 2021-08-04 |       398 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532014.86 | 1878810.61 | 357 | 22373.75 | 238655 | 3246423 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8105.06 |   15328.99 |   1 |   206.00 |   2831 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-25 23:46:40 | 2021-08-04 01:27:02 |     15042 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     43 |        60 |

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
    ## 1 healthyR.data <tibble [369 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [359 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [310 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [284 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [99 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8318612 |  89.711837 | 0.7243087 |  81.878305 | 1.1647283 | 0.2759554 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2634217 |  17.309213 | 0.2293635 |  17.649971 | 0.6995312 | 0.7346208 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.1682614 |  89.385626 | 1.0172153 | 140.001908 | 1.5701180 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2427791 |  13.887845 | 0.2113898 |  14.470120 | 0.7013614 | 0.7346767 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2325063 |   8.093450 | 0.2024452 |   8.646772 | 0.6673705 | 0.7492296 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2253009 |   7.362628 | 0.1961714 |   7.529571 | 0.6603858 | 0.7539653 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2356876 |  12.774055 | 0.2052152 |  13.220018 | 0.6931194 | 0.7424400 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5857438 |  40.223475 | 0.5100122 |  52.039562 | 0.9928445 | 0.7013812 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1666142 | 148.618640 | 1.0157810 |  98.517963 | 1.4534915 | 0.3685523 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4568355 | 147.131063 | 1.2684792 | 120.056826 | 1.8520772 | 0.0465315 |
| healthyR.data |         13 | TBATS                      | Test  | 1.1062251 |  88.516546 | 0.9631997 | 136.382721 | 1.5094644 | 0.0580452 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.1867740 |  90.877397 | 1.0333344 | 146.651394 | 1.5848434 | 0.1793224 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6981714 | 118.004360 | 0.7697049 | 102.963673 | 0.8457326 | 0.4845599 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0977990 |   9.671033 | 0.1078193 |   9.336070 | 0.2196600 | 0.9623653 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.9049804 | 113.900179 | 0.9977032 | 147.050715 | 1.1617146 | 0.0045188 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1048112 |  12.080475 | 0.1155500 |  11.173927 | 0.2190003 | 0.9624232 |
| healthyR      |          7 | EARTH                      | Test  | 0.0425740 |   2.520499 | 0.0469361 |   2.444381 | 0.1037397 | 0.9926233 |
| healthyR      |          8 | NNAR                       | Test  | 0.1158123 |   8.526024 | 0.1276782 |   8.013307 | 0.2288077 | 0.9849301 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1067117 |  13.583793 | 0.1176452 |  12.207486 | 0.2140924 | 0.9645524 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3209990 |  45.433713 | 0.3538881 |  50.130721 | 0.4464483 | 0.9816653 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9832594 | 213.934576 | 1.0840025 | 103.402290 | 1.2882648 | 0.5104447 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2429223 | 216.555293 | 1.3702700 | 128.875222 | 1.5177000 | 0.0976841 |
| healthyR      |         13 | TBATS                      | Test  | 0.8925912 |  97.147789 | 0.9840445 | 139.924353 | 1.1753504 | 0.0148081 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8899463 | 104.596050 | 0.9811287 | 132.888358 | 1.1586299 | 0.2438464 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9333371 |  92.644078 | 0.6724847 | 103.618970 | 1.1391947 | 0.3919554 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1370902 |   8.202538 | 0.0987757 |   8.346647 | 0.3326572 | 0.9406978 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2518900 |  99.218142 | 0.9020074 | 156.117443 | 1.4992666 | 0.0681040 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1365240 |   8.077744 | 0.0983678 |   8.208251 | 0.3300747 | 0.9409013 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1277216 |   6.020738 | 0.0920255 |   5.828814 | 0.2263270 | 0.9697534 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1849470 |  10.454293 | 0.1332574 |   9.494255 | 0.3311032 | 0.9415123 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1387493 |   8.849859 | 0.0999711 |   8.904964 | 0.3248625 | 0.9434443 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5570028 |  39.624156 | 0.4013297 |  51.175066 | 0.7187548 | 0.9501768 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2297422 | 183.896361 | 0.8860495 | 100.381567 | 1.4700262 | 0.4634671 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6010865 | 207.410517 | 1.1536091 | 136.132058 | 1.9276618 | 0.0591650 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.4527475 | 113.985418 | 1.0467285 | 171.929992 | 1.7168733 | 0.0306729 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.3267405 |  99.019155 | 0.9559384 | 186.023814 | 1.6086331 | 0.1565832 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7935832 | 122.374165 | 0.8809192 | 135.001149 | 0.9739605 | 0.3683272 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1094329 |  15.756079 | 0.1214763 |  16.076410 | 0.1972130 | 0.9692379 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9701478 | 108.252828 | 1.0769152 | 162.257506 | 1.2413129 | 0.1084577 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1105780 |  14.521642 | 0.1227474 |  14.329874 | 0.1948816 | 0.9694371 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0747488 |   6.914104 | 0.0829751 |   6.735813 | 0.1317480 | 0.9896227 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0583628 |   5.373471 | 0.0647858 |   5.085280 | 0.1284387 | 0.9885584 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1086324 |  15.893489 | 0.1205877 |  16.328234 | 0.1882722 | 0.9712560 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3899165 |  44.971429 | 0.4328279 |  56.633563 | 0.5338474 | 0.9836215 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9183066 | 280.684616 | 1.0193687 | 102.089884 | 1.0958081 | 0.6165058 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2582643 | 342.376714 | 1.3967397 | 140.432732 | 1.4929155 | 0.1010539 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1115103 | 163.429053 | 1.2338351 | 161.011615 | 1.3796584 | 0.0578527 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.0065027 | 121.759331 | 1.1172711 | 166.553807 | 1.3080317 | 0.2867731 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.6904964 |  81.297479 | 0.6981226 |  90.576671 | 0.9203049 | 0.8701996 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1302469 |  13.040255 | 0.1316854 |  12.772791 | 0.2013773 | 0.9645801 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1437340 | 110.756340 | 1.1563660 | 177.700354 | 1.3945120 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1409530 |  15.675764 | 0.1425098 |  15.369925 | 0.2031988 | 0.9656896 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0570039 |   4.766543 | 0.0576335 |   4.781917 | 0.1179633 | 0.9876230 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2352826 |  25.949412 | 0.2378811 |  26.576457 | 0.3808391 | 0.8728130 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1457649 |  17.194558 | 0.1473748 |  16.860665 | 0.2010514 | 0.9672314 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3656342 |  55.711470 | 0.3696725 |  56.259350 | 0.5668737 | 0.8494032 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0037176 | 183.456402 | 1.0148031 |  89.595953 | 1.3440469 | 0.4426614 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4662801 | 220.081740 | 1.4824744 | 143.910460 | 1.7519737 | 0.0977916 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.2460431 | 146.884473 | 1.2598051 | 169.058531 | 1.4893303 | 0.0883486 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0085716 |  96.087834 | 1.0197107 | 169.729517 | 1.2457230 | 0.0731729 |
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
    ## 1 healthyR.da~         8 NNAR        Test  0.225   7.36 0.196   7.53 0.660 0.754
    ## 2 healthyR             7 EARTH       Test  0.0426  2.52 0.0469  2.44 0.104 0.993
    ## 3 healthyR.ts          7 EARTH       Test  0.128   6.02 0.0920  5.83 0.226 0.970
    ## 4 healthyverse         8 NNAR        Test  0.0584  5.37 0.0648  5.09 0.128 0.989
    ## 5 healthyR.ai          7 EARTH       Test  0.0570  4.77 0.0576  4.78 0.118 0.988

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
    ## 1 healthyR.data <tibble [369 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [359 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [310 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [284 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [99 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
