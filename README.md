Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
03 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,223
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

The last day in the data set is 2022-02-01 20:08:18, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1539.5
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28223          |
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
| r\_version     |      18781 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18781 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18781 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2363 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-01 | 2021-08-21 |       436 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1525725.90 | 1874955.73 | 357 | 23626 | 261875 | 3247926 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8221.42 |   15718.95 |   1 |   245 |   2806 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-01 20:08:18 | 2021-08-21 00:44:14 |     16519 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 32S |        60 |

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
    ## 1 healthyR.data <tibble [407 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [397 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [347 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [322 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [136 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9828719 |   97.8536986 | 0.7065227 | 124.1419627 | 1.2384861 | 0.4930747 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0794894 |    9.7806081 | 0.0571398 |  10.8916260 | 0.1287136 | 0.9850923 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0020375 |   92.4750222 | 0.7202996 | 140.1502602 | 1.2047365 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0603003 |    7.2164229 | 0.0433459 |   7.0505609 | 0.0739336 | 0.9961243 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0288337 |    3.1963150 | 0.0207267 |   3.1706865 | 0.0383462 | 0.9988681 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0073719 |    0.8586620 | 0.0052992 |   0.8575654 | 0.0090305 | 0.9999303 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0639967 |    8.0090463 | 0.0460031 |   8.1064182 | 0.0843125 | 0.9949580 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5753062 |   62.6584150 | 0.4135502 |  83.4578686 | 0.6979046 | 0.9943976 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9879819 |  135.1396387 | 0.7101960 |  85.1059950 | 1.2927543 | 0.5204152 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2962583 |  178.5495049 | 0.9317958 | 127.9269028 | 1.6362073 | 0.0684403 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0587819 |  101.5418023 | 0.7610894 | 133.1378450 | 1.3045836 | 0.0005652 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0015910 |   92.4392963 | 0.7199787 | 140.2245538 | 1.2039269 | 0.2212981 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7955787 |  432.3883011 | 0.9513661 | 163.1334319 | 0.9948431 | 0.3580794 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0506383 |   30.2506443 | 0.0605541 |  19.5758821 | 0.0660637 | 0.9938542 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5707779 |  379.2876590 | 0.6825455 | 123.1637085 | 0.7395166 | 0.2238218 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0476925 |   30.0232423 | 0.0570315 |  25.6318435 | 0.0638374 | 0.9939258 |
| healthyR      |          7 | EARTH                      | Test  | 0.0287597 |   11.1047100 | 0.0343913 |   9.1219400 | 0.0694930 | 0.9933821 |
| healthyR      |          8 | NNAR                       | Test  | 0.0105985 |    2.2422845 | 0.0126739 |   2.2347216 | 0.0236800 | 0.9992094 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0514122 |   31.1886710 | 0.0614795 |  19.1811507 | 0.0704985 | 0.9927515 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4896925 |  452.3935025 | 0.5855823 | 122.5609107 | 0.5878641 | 0.9663609 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1980713 | 1474.3285467 | 1.4326733 | 118.9005849 | 1.4363961 | 0.5011078 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1299430 | 1585.1508135 | 1.3512043 | 126.5354512 | 1.3802020 | 0.1775171 |
| healthyR      |         13 | TBATS                      | Test  | 0.5952320 |  323.9424122 | 0.7117881 | 127.2247589 | 0.7900403 | 0.1848894 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6219426 |  320.4573440 | 0.7437292 | 130.3999352 | 0.8175172 | 0.2585757 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8408884 |  299.3419619 | 0.6519005 | 137.1496317 | 1.0512132 | 0.4884316 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0793780 |  100.9657554 | 0.0615379 |  23.1872528 | 0.0952002 | 0.9940410 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8280038 |  283.2057357 | 0.6419117 | 154.1480320 | 1.0643911 | 0.1866948 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0778731 |   91.4923538 | 0.0603713 |  23.1699354 | 0.0933357 | 0.9944335 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0345040 |    3.2052624 | 0.0267493 |   3.1414640 | 0.0777797 | 0.9962793 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0364390 |    8.4117894 | 0.0282494 |   8.2572552 | 0.1012012 | 0.9925561 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0856788 |  121.6220445 | 0.0664227 |  24.3599040 | 0.1067113 | 0.9927993 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4783586 |  384.9180487 | 0.3708485 |  98.6469902 | 0.5871631 | 0.9824010 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1846310 |  900.9477438 | 0.9183876 | 120.4298943 | 1.4278928 | 0.5999632 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2230557 | 1091.4913624 | 0.9481764 | 126.5304603 | 1.4773532 | 0.1849239 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.8417350 |  349.5236917 | 0.6525568 | 144.0950117 | 1.0608606 | 0.1792894 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9060589 |  111.3645249 | 0.7024240 | 191.8721634 | 1.1647536 | 0.3761187 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8561022 |  223.4781405 | 0.7512820 | 164.5529669 | 1.0694601 | 0.2863524 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0639087 |   13.4308970 | 0.0560838 |  11.3740220 | 0.0826763 | 0.9911077 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7342227 |  407.6816478 | 0.6443252 | 123.6515121 | 0.8856414 | 0.0475377 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0585701 |    9.3867567 | 0.0513989 |   8.9627157 | 0.0814195 | 0.9910843 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0191119 |    4.0876415 | 0.0167719 |   3.6746337 | 0.0372822 | 0.9992050 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0084575 |    0.9077962 | 0.0074220 |   0.8971262 | 0.0242787 | 0.9994910 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1034713 |   25.3349294 | 0.0908024 |  28.9196238 | 0.1172567 | 0.9897632 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5555616 |  195.6260726 | 0.4875393 | 110.2832697 | 0.6697939 | 0.9678168 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2928287 | 1001.9955617 | 1.1345361 | 119.0906149 | 1.4647251 | 0.3285901 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4019748 | 1226.3698168 | 1.2303185 | 136.7464335 | 1.6424909 | 0.0328923 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8152396 |  344.3450772 | 0.7154226 | 143.3057018 | 1.0258611 | 0.0050995 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7065626 |  282.1624200 | 0.6200518 | 105.5605914 | 0.9018126 | 0.1940411 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0956758 |  567.1008137 | 0.7700340 | 144.6059575 | 1.2652481 | 0.3934661 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0611072 |   33.1865662 | 0.0429457 |  16.3520198 | 0.0774443 | 0.9956887 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9499656 |  292.2660046 | 0.6676298 | 160.9785415 | 1.0719750 | 0.0951023 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0570664 |   24.9285414 | 0.0401059 |  14.9573000 | 0.0733427 | 0.9957442 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0145170 |    3.0982360 | 0.0102025 |   2.8081134 | 0.0218105 | 0.9996127 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0823420 |   12.2540545 | 0.0578694 |  16.0701648 | 0.1162436 | 0.9919466 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0688962 |   47.3262857 | 0.0484198 |  20.9610564 | 0.0877293 | 0.9941443 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.0205310 |  676.5043784 | 0.7172227 | 144.1932112 | 1.1565574 | 0.8726231 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2003631 |  551.8498232 | 0.8436076 | 125.0103286 | 1.4160858 | 0.4799199 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3029581 |  586.9440281 | 0.9157107 | 138.7606559 | 1.6031553 | 0.0956651 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9473918 |  359.5088066 | 0.6658209 | 160.2851842 | 1.0889274 | 0.1170532 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9613683 |  226.4880886 | 0.6756436 | 152.0482105 | 1.1784350 | 0.2650478 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.00737 0.859 0.00530 0.858 0.00903 1.00 
    ## 2 healthyR         8 NNAR        Test  0.0106  2.24  0.0127  2.23  0.0237  0.999
    ## 3 healthy~         7 EARTH       Test  0.0345  3.21  0.0267  3.14  0.0778  0.996
    ## 4 healthy~         8 NNAR        Test  0.00846 0.908 0.00742 0.897 0.0243  0.999
    ## 5 healthy~         7 EARTH       Test  0.0145  3.10  0.0102  2.81  0.0218  1.00

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
    ## 1 healthyR.data <tibble [407 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [397 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [347 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [322 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [136 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
