Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,365
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

The last day in the data set is 2021-12-17 18:13:49, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -433.59
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25365          |
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
| r\_version     |      16991 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16991 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16991 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2146 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-17 | 2021-08-02 |       390 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1534190.73 | 1879713.99 | 357 | 24837 | 238655 | 3246351 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8138.64 |   15367.31 |   1 |   238 |   2888 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-17 18:13:49 | 2021-08-02 15:36:36 |     14791 |

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
    ## 1 healthyR.data <tibble [361 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [351 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [302 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [276 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [91 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9957306 |  78.109122 | 0.7304747 |  95.907696 | 1.4552843 | 0.2748766 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2513523 |   9.552691 | 0.1843937 |  10.859937 | 0.7734470 | 0.7373668 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4374425 | 102.124156 | 1.0545175 | 194.566840 | 1.8891270 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2613120 |  11.472919 | 0.1917003 |  12.476548 | 0.7648425 | 0.7369020 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2427408 |   8.209873 | 0.1780763 |   8.565477 | 0.6489668 | 0.7733542 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2328604 |   7.660749 | 0.1708279 |   7.839132 | 0.6593425 | 0.7662815 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2496434 |   9.932031 | 0.1831401 |  11.142237 | 0.7684589 | 0.7420258 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7454686 |  50.971964 | 0.5468808 |  71.414403 | 1.1560813 | 0.7267732 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8155619 |  94.205782 | 0.5983017 |  68.421366 | 1.1216564 | 0.5110490 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4568581 | 111.688937 | 1.0687609 | 118.753734 | 1.9549558 | 0.1152128 |
| healthyR.data |         13 | TBATS                      | Test  | 1.4138476 | 101.891861 | 1.0372082 | 175.032831 | 1.8687347 | 0.0586413 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4690515 | 104.224024 | 1.0777061 | 188.952979 | 1.9208593 | 0.0071764 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6712773 | 118.340497 | 0.6809265 |  77.843799 | 0.9035020 | 0.5937824 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1206376 |   9.809129 | 0.1223716 |   9.534530 | 0.2580389 | 0.9604887 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3872151 | 184.445084 | 1.4071554 | 170.077803 | 1.7026666 | 0.0191690 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1233858 |  11.310439 | 0.1251594 |  10.523869 | 0.2558986 | 0.9605170 |
| healthyR      |          7 | EARTH                      | Test  | 0.0790172 |   3.851748 | 0.0801530 |   3.651433 | 0.1697702 | 0.9908285 |
| healthyR      |          8 | NNAR                       | Test  | 0.1542532 |   9.544235 | 0.1564705 |   9.295007 | 0.3308422 | 0.9783238 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1154166 |   7.553963 | 0.1170756 |   7.675818 | 0.2554834 | 0.9625977 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3981598 |  37.478534 | 0.4038831 |  40.692419 | 0.5766077 | 0.9676818 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7234622 | 154.813032 | 0.7338615 |  75.538191 | 0.9594882 | 0.7087351 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5046972 | 291.058217 | 1.5263263 | 151.901251 | 1.7204512 | 0.2774232 |
| healthyR      |         13 | TBATS                      | Test  | 1.3861376 | 178.422650 | 1.4060624 | 170.974286 | 1.6968538 | 0.0262908 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3504727 | 163.584313 | 1.3698849 | 174.068390 | 1.6671996 | 0.0081024 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0981149 |  78.334349 | 0.8272888 | 121.906061 | 1.3660265 | 0.5900492 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1628055 |   9.671625 | 0.1226531 |   9.771882 | 0.3484163 | 0.9438153 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.7206769 | 134.771920 | 1.2963095 | 170.427754 | 1.9930748 | 0.2672264 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1629929 |   9.730949 | 0.1227943 |   9.806453 | 0.3465448 | 0.9439096 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1535092 |   7.202687 | 0.1156495 |   6.835309 | 0.2483742 | 0.9715704 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1944051 |   9.428504 | 0.1464593 |   8.686298 | 0.3483463 | 0.9523547 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1574993 |   9.447388 | 0.1186555 |   9.540526 | 0.3436970 | 0.9461080 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7392412 |  50.182422 | 0.5569235 |  66.237779 | 0.9847767 | 0.8568582 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9726834 | 129.665074 | 0.7327923 |  79.829359 | 1.1899861 | 0.6808837 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.8195769 | 197.619787 | 1.3708180 | 140.770970 | 2.1528058 | 0.2403897 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.6272404 | 124.345604 | 1.2259170 | 171.357831 | 1.8867900 | 0.3007689 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.7359548 | 123.444070 | 1.3078194 | 175.978308 | 2.0426262 | 0.0143922 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8924232 |  98.735333 | 0.8623695 | 135.396178 | 1.1473213 | 0.5969419 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1160275 |  13.648654 | 0.1121201 |  14.454541 | 0.2151752 | 0.9714400 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3631908 | 196.170195 | 1.3172833 | 173.080281 | 1.6339258 | 0.2988909 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1151005 |  14.968421 | 0.1112243 |  17.539447 | 0.2150153 | 0.9714540 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1213829 |  10.649230 | 0.1172952 |  10.398685 | 0.1905159 | 0.9881893 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0449319 |   3.547081 | 0.0434187 |   3.420748 | 0.1109488 | 0.9928727 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1145439 |  17.413348 | 0.1106864 |  18.487407 | 0.2078222 | 0.9727107 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5706337 |  87.763800 | 0.5514167 |  83.646351 | 0.7323944 | 0.9711960 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6913173 | 242.674013 | 0.6680362 |  65.544370 | 0.9620560 | 0.7251788 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3869302 | 234.479002 | 1.3402233 | 140.192327 | 1.7009452 | 0.2459495 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3478181 | 194.761210 | 1.3024283 | 173.001273 | 1.6178709 | 0.2905844 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4294873 | 222.193960 | 1.3813472 | 174.878270 | 1.7247554 | 0.0036428 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8931858 |  80.782527 | 0.8356628 | 118.964105 | 1.1932233 | 0.2592227 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1148161 |   9.978637 | 0.1074217 |   9.743704 | 0.2148458 | 0.9729269 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2878595 | 130.989329 | 1.2049188 | 179.239404 | 1.5815999 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1310742 |  13.801769 | 0.1226328 |  12.875842 | 0.2130965 | 0.9728134 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1025154 |   5.248119 | 0.0959132 |   5.562843 | 0.2548904 | 0.9718702 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.4094567 |  34.878008 | 0.3830869 |  44.759707 | 0.6177858 | 0.8801084 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1370532 |  15.392226 | 0.1282267 |  14.000484 | 0.2119456 | 0.9731043 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5604923 |  40.892762 | 0.5243954 |  53.544477 | 0.8736671 | 0.7888875 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8267455 | 147.090961 | 0.7735014 |  70.317953 | 1.2139821 | 0.5549786 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3404834 | 177.678750 | 1.2541536 | 137.063651 | 1.6816178 | 0.1740110 |
| healthyR.ai   |         13 | BATS                       | Test  | 1.3332576 | 136.052441 | 1.2473931 | 173.670032 | 1.6419349 | 0.0000000 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3304334 | 139.275729 | 1.2447508 | 177.598991 | 1.6236489 | 0.0009473 |
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
    ## 1 healthyR~         7 EARTH          Test  0.243   8.21 0.178   8.57 0.649 0.773
    ## 2 healthyR          7 EARTH          Test  0.0790  3.85 0.0802  3.65 0.170 0.991
    ## 3 healthyR~         7 EARTH          Test  0.154   7.20 0.116   6.84 0.248 0.972
    ## 4 healthyv~         8 NNAR           Test  0.0449  3.55 0.0434  3.42 0.111 0.993
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.137  15.4  0.128  14.0  0.212 0.973

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
    ## 1 healthyR.data <tibble [361 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [351 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [302 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [276 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [91 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
