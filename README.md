Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,930
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

The last day in the data set is 2022-03-17 21:30:52, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2596.87
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31930         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     21193 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     21193 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     21193 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2635 |          0.92 |   2 |   2 |     0 |      102 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-17 | 2021-09-14 |      480 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1492161.51 | 1858532.75 | 357 | 16879 | 271097 | 3246457 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8363.75 |   16080.35 |   1 |   376 |   2669 |    8410 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-17 21:30:52 | 2021-09-14 07:21:43 |    18581 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      9 |       60 |

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
  #step_rm(yr) %>%
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
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [421|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [362|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [338|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [152|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [28 x 6]> <tibble>     <split [0|28]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9659688 |  224.396776 | 0.7138564 | 142.230231 | 1.1987786 | 0.1135024 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0643293 |   20.016408 | 0.0475397 |  15.846244 | 0.0791159 | 0.9934714 |
| healthyR.data |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7720951 |  161.436405 | 0.5705826 | 125.398011 | 0.9391121 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0468886 |   10.663418 | 0.0346510 |   9.020733 | 0.0602619 | 0.9963578 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2012127 |  103.354265 | 0.1486973 |  35.872320 | 0.3128485 | 0.9286636 |
| healthyR.data |         8 | NNAR                       | Test  | 0.1548771 |   26.627418 | 0.1144550 |  26.884727 | 0.1964146 | 0.9710769 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0581288 |   10.533985 | 0.0429575 |  10.946787 | 0.0717768 | 0.9948155 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3427325 |   86.158638 | 0.2532812 |  57.958869 | 0.4314470 | 0.9948239 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8638350 |  397.186072 | 0.6383789 | 110.300170 | 1.0519329 | 0.3696949 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2664115 |  431.541656 | 0.9358852 | 144.875403 | 1.5050122 | 0.0024483 |
| healthyR.data |        13 | BATS                       | Test  | 0.7808415 |  169.952704 | 0.5770463 | 125.155572 | 0.9425632 | 0.0001394 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7724414 |  163.595622 | 0.5708385 | 124.604838 | 0.9391178 | 0.0000816 |
| healthyR.data |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7237181 |  160.361278 | 0.6200189 | 145.416714 | 0.9329612 | 0.2728909 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0469795 |   13.693963 | 0.0402479 |  16.429888 | 0.0670869 | 0.9948453 |
| healthyR      |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7678871 |  315.875277 | 0.6578591 | 160.496396 | 0.9144179 | 0.0264374 |
| healthyR      |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0446693 |   10.782973 | 0.0382688 |  10.813459 | 0.0663705 | 0.9948627 |
| healthyR      |         7 | EARTH                      | Test  | 0.0114492 |    2.938805 | 0.0098087 |   2.795513 | 0.0235257 | 0.9994188 |
| healthyR      |         8 | NNAR                       | Test  | 0.0457481 |   14.021121 | 0.0391930 |  16.351566 | 0.0804103 | 0.9933892 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0527572 |   13.961726 | 0.0451978 |  14.778298 | 0.0751313 | 0.9934082 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3752853 |  144.424235 | 0.3215119 |  92.911960 | 0.4838970 | 0.9827631 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1903115 |  816.625284 | 1.0197556 | 133.886003 | 1.3572362 | 0.4412730 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2092005 |  961.366750 | 1.0359381 | 127.857144 | 1.4524175 | 0.0799588 |
| healthyR      |        13 | TBATS                      | Test  | 0.7560131 |  271.446335 | 0.6476864 | 160.351594 | 0.9141793 | 0.0396977 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7151434 |  175.930709 | 0.6126728 | 145.776559 | 0.9269768 | 0.0016866 |
| healthyR      |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7369646 |  272.652373 | 0.6322518 | 141.976553 | 0.9323098 | 0.3057968 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0535608 |   34.346495 | 0.0459505 |  20.187091 | 0.0684525 | 0.9969894 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.8072703 |  404.980714 | 0.6925680 | 137.378757 | 0.9705905 | 0.0036562 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0531574 |   35.148759 | 0.0456045 |  20.199701 | 0.0686291 | 0.9970896 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0158265 |    3.165617 | 0.0135778 |   3.101263 | 0.0284332 | 0.9990735 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0586783 |   29.709730 | 0.0503409 |  27.962191 | 0.0987975 | 0.9893956 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0558066 |   15.644460 | 0.0478773 |  14.082465 | 0.0700205 | 0.9956578 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3991119 |  208.338110 | 0.3424034 |  97.747582 | 0.4682428 | 0.9985586 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0806118 |  923.494499 | 0.9270713 | 125.165588 | 1.3584789 | 0.1952205 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.4584363 | 1071.646751 | 1.2512120 | 144.944793 | 1.7435576 | 0.0034742 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.8071319 |  407.952371 | 0.6924493 | 157.808030 | 0.9758480 | 0.0008854 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7476824 |  450.411333 | 0.6414467 | 121.019299 | 0.9352693 | 0.0012546 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.6456910 |  120.947460 | 0.5231768 | 132.646732 | 0.8651045 | 0.2444929 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0658531 |   25.957020 | 0.0533580 |  19.499966 | 0.0818943 | 0.9949292 |
| healthyverse  |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9119195 |  352.220367 | 0.7388909 | 125.790588 | 1.1360722 | 0.0261002 |
| healthyverse  |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0662508 |   26.158500 | 0.0536803 |  19.761713 | 0.0829044 | 0.9948811 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0234485 |    3.609802 | 0.0189994 |   3.707060 | 0.0444621 | 0.9979192 |
| healthyverse  |         8 | NNAR                       | Test  | 0.1130371 |   17.759654 | 0.0915893 |  15.908356 | 0.2114778 | 0.9809223 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0736488 |   22.114442 | 0.0596746 |  20.684936 | 0.0922278 | 0.9934330 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2749710 |   52.192881 | 0.2227977 |  52.514487 | 0.3715992 | 0.9886437 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8488096 |  372.462766 | 0.6877555 | 118.818912 | 0.9742496 | 0.3920102 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1787224 |  313.359227 | 0.9550702 | 132.455964 | 1.5075762 | 0.0002419 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7462267 |  153.327711 | 0.6046368 | 126.559231 | 0.9731823 | 0.0049305 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9291534 |  379.464170 | 0.7528548 | 128.245151 | 1.1151777 | 0.1653365 |
| healthyverse  |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.7215927 |  319.229194 | 0.7594687 | 144.505236 | 0.8983198 | 0.2265635 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7020355 |  357.694836 | 0.7388850 | 159.814784 | 0.8362089 | 0.0006248 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0501062 |   36.800760 | 0.0527362 |  20.497299 | 0.0633638 | 0.9974645 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0120439 |    3.283677 | 0.0126761 |   3.201413 | 0.0142362 | 0.9996921 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0379718 |   35.514528 | 0.0399649 |  23.062594 | 0.0465798 | 0.9973543 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0593032 |   33.506971 | 0.0624160 |  21.609170 | 0.0726054 | 0.9956787 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4104824 |  267.156488 | 0.4320284 | 108.371812 | 0.4666423 | 0.9945464 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1270787 | 1044.617916 | 1.1862385 | 144.550492 | 1.4074635 | 0.0080721 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6607338 |  302.242579 | 0.6954154 | 149.147153 | 0.8125308 | 0.0040167 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6074499 |  181.157105 | 0.6393346 | 162.914763 | 0.7515183 | 0.0354750 |
| healthyR.ai   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         1 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         6 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         7 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         8 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         9 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        10 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        12 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        13 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        14 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |

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
    ##   # A tibble: 6 x 10
    ##   package       .model_id .model_desc .type     mae  mape     mase smape    rmse
    ##   <chr>             <int> <chr>       <chr>   <dbl> <dbl>    <dbl> <dbl>   <dbl>
    ## 1 healthyR.data         6 LM          Test   0.0469 10.7   0.0347   9.02  0.0603
    ## 2 healthyR              7 EARTH       Test   0.0114  2.94  0.00981  2.80  0.0235
    ## 3 healthyR.ts           7 EARTH       Test   0.0158  3.17  0.0136   3.10  0.0284
    ## 4 healthyverse          7 EARTH       Test   0.0234  3.61  0.0190   3.71  0.0445
    ## 5 healthyR.ai           7 EARTH       Test   0.0120  3.28  0.0127   3.20  0.0142
    ## 6 TidyDensity          NA <NA>        <NA>  NA      NA    NA       NA    NA     
    ## # ... with 1 more variable: rsq <dbl>

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [421|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [362|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [338|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [152|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [28 x 6]> <tibble>     <split [0|28]>   <mdl_time_tbl>

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
