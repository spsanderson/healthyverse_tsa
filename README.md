Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
24 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,335
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

The last day in the data set is 2022-01-22 22:25:21, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1301.78
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27335          |
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
| r\_version     |      18275 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18275 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18275 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2290 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-22 | 2021-08-12 |       426 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1531329.08 | 1877596.93 | 357 | 16923 | 238827 | 3247727 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8067.52 |   15305.48 |   1 |   227 |   2770 |    8243 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-22 22:25:21 | 2021-08-12 05:25:30 |     15972 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 52M 39S |        60 |

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
    ## 1 healthyR.data <tibble [397 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [387 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [337 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [312 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [126 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8117054 |  387.3983957 | 0.5821702 | 121.398263 | 0.9939694 | 0.3428622 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0637359 |   12.7927703 | 0.0457126 |  14.948851 | 0.0885699 | 0.9921349 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9164535 |  329.9876446 | 0.6572976 | 116.433375 | 1.1875807 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0628229 |   23.8741636 | 0.0450578 |  17.119838 | 0.0809091 | 0.9942930 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0305057 |   12.3163152 | 0.0218793 |   7.612730 | 0.0459476 | 0.9980570 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0138806 |    4.2337622 | 0.0099554 |   3.570488 | 0.0210872 | 0.9995472 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0643264 |   21.3947652 | 0.0461361 |  17.532566 | 0.0940131 | 0.9922023 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5686015 |  277.3702199 | 0.4078116 | 100.770766 | 0.6638475 | 0.9924860 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1658465 |  635.5862622 | 0.8361668 | 120.098800 | 1.3761494 | 0.4672741 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3567632 |  482.8937823 | 0.9730959 | 148.555674 | 1.6180234 | 0.0946992 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8543024 |  267.1433507 | 0.6127216 | 113.742985 | 1.1124928 | 0.1165096 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9160875 |  329.8158633 | 0.6570350 | 116.424345 | 1.1871597 | 0.0598894 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7575322 |  433.8452394 | 0.9093140 | 154.807601 | 0.8525467 | 0.7102436 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0351060 |   13.8808829 | 0.0421400 |  15.192450 | 0.0431141 | 0.9985448 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5712748 |  129.8431286 | 0.6857374 | 149.734325 | 0.7276698 | 0.1061832 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0342246 |   18.7002918 | 0.0410820 |  21.209151 | 0.0422354 | 0.9987675 |
| healthyR      |          7 | EARTH                      | Test  | 0.0182596 |    8.3676930 | 0.0219181 |   7.245133 | 0.0202321 | 0.9995095 |
| healthyR      |          8 | NNAR                       | Test  | 0.0100222 |    4.2349330 | 0.0120303 |   4.517872 | 0.0151383 | 0.9995774 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0382764 |   16.6056346 | 0.0459456 |  16.146208 | 0.0525358 | 0.9976116 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4497496 |  337.5089565 | 0.5398631 | 130.700585 | 0.4987077 | 0.9966331 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2063816 | 1249.1600463 | 1.4480964 | 121.697530 | 1.4294518 | 0.6873067 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9880071 | 1078.3228360 | 1.1859676 | 117.660387 | 1.2112984 | 0.4925119 |
| healthyR      |         13 | TBATS                      | Test  | 0.5798229 |  161.6136980 | 0.6959983 | 153.704690 | 0.7350311 | 0.1591647 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6618378 |  265.6264489 | 0.7944459 | 164.920628 | 0.7895719 | 0.0023565 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.6741750 |  258.8707438 | 0.4650265 | 117.325966 | 0.8383965 | 0.3522132 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0631088 |   75.4847741 | 0.0435306 |  21.568329 | 0.0752890 | 0.9956304 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7040205 |  279.2468000 | 0.4856131 | 119.497918 | 0.9046426 | 0.2359295 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0713680 |   73.0744149 | 0.0492276 |  23.592544 | 0.0859450 | 0.9959782 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0303450 |    3.5347220 | 0.0209311 |   3.499234 | 0.0650902 | 0.9970420 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0420057 |   14.8898839 | 0.0289743 |  10.100032 | 0.0932430 | 0.9916442 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0789798 |   82.9266233 | 0.0544780 |  23.016090 | 0.1039445 | 0.9947368 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4380075 |  411.0961352 | 0.3021250 |  91.576653 | 0.5244425 | 0.9839126 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2209997 |  626.5838523 | 0.8422105 | 130.806661 | 1.4471911 | 0.5001644 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2801852 |  845.7742961 | 0.8830350 | 143.146807 | 1.5042673 | 0.1559180 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6312487 |  232.1609917 | 0.4354172 | 116.829122 | 0.8015270 | 0.4184008 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8195942 |  452.3331362 | 0.5653325 | 123.050973 | 1.0254898 | 0.0105916 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8891663 |  349.0357124 | 0.8303264 | 168.314772 | 1.0397915 | 0.5867134 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0578775 |   25.9034243 | 0.0540475 |  16.942213 | 0.0690165 | 0.9966164 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6468005 |  213.8211351 | 0.6039990 | 142.643585 | 0.7830363 | 0.1919654 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0531996 |   18.5573266 | 0.0496792 |  14.038291 | 0.0631532 | 0.9969291 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0110562 |    4.0530923 | 0.0103246 |   3.679236 | 0.0158689 | 0.9997239 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0046981 |    0.7867587 | 0.0043872 |   0.790754 | 0.0067176 | 0.9999721 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0746806 |   25.6490501 | 0.0697387 |  24.319692 | 0.0863582 | 0.9945429 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5465257 |  266.6687738 | 0.5103597 | 129.859925 | 0.6160375 | 0.9842280 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9844715 |  783.8921753 | 0.9193248 | 111.186132 | 1.1615506 | 0.5556147 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1744379 |  997.0628909 | 1.0967204 | 128.922363 | 1.4028790 | 0.1855088 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5820061 |  189.9380258 | 0.5434923 | 120.885546 | 0.7043899 | 0.3216622 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6871545 |  107.9549970 | 0.6416825 | 162.502179 | 0.8684942 | 0.0054495 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8835942 |  159.8259842 | 0.5149269 | 114.950412 | 1.1005714 | 0.8075728 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0509763 |    8.7996064 | 0.0297072 |   9.715683 | 0.0604327 | 0.9985645 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9359460 |  128.8656174 | 0.5454357 | 125.421139 | 1.1403954 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0492827 |    8.1125175 | 0.0287202 |   8.501860 | 0.0585165 | 0.9986606 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0241910 |    2.3976042 | 0.0140976 |   2.379145 | 0.0393018 | 0.9989341 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.1315830 |   25.5700935 | 0.0766818 |  29.889498 | 0.1757175 | 0.9819270 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0595923 |   10.9935618 | 0.0347282 |  11.977803 | 0.0734921 | 0.9973910 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8823580 |  190.7341816 | 0.5142065 | 121.108883 | 1.0158984 | 0.9115608 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8741385 |  194.9529912 | 0.5094164 | 101.744499 | 1.0354097 | 0.6451569 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3014027 |  277.1654657 | 0.7584106 | 133.186245 | 1.5258795 | 0.0870116 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9332287 |  139.0443746 | 0.5438521 | 130.287835 | 1.0908483 | 0.0598459 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9925121 |  154.5043802 | 0.5784003 | 123.438066 | 1.2089422 | 0.0098269 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |

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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0139  4.23  0.00996 3.57  0.0211  1.00 
    ## 2 healthyR         8 NNAR        Test  0.0100  4.23  0.0120  4.52  0.0151  1.00 
    ## 3 healthy~         7 EARTH       Test  0.0303  3.53  0.0209  3.50  0.0651  0.997
    ## 4 healthy~         8 NNAR        Test  0.00470 0.787 0.00439 0.791 0.00672 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0242  2.40  0.0141  2.38  0.0393  0.999

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
    ## 1 healthyR.data <tibble [397 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [387 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [337 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [312 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [126 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
