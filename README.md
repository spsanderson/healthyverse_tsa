Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
01 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,951
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

The last day in the data set is 2022-02-27 23:33:42, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2166.92
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 29951         |
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
| r_version     |     19905 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     19905 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     19905 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       17 |          0 |
| country       |      2502 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-02-27 | 2021-08-31 |      462 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|--------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1512948 | 1870088.58 | 357 | 18669.5 | 271098 | 3247952 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8245 |   15825.29 |   1 |   277.0 |   2709 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-02-27 23:33:42 | 2021-08-31 03:21:37 |    17444 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 0M 7S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [395|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [344|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [320|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [134|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [10 x 6]> <tibble>     <split [0|10]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9923760 | 294.925901 | 0.8908224 | 143.913939 | 1.1935431 | 0.5518711 |
| healthyR.data |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.8367655 | 129.524363 | 0.7511361 | 159.404337 | 0.9943166 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0709834 |  25.553380 | 0.0637194 |  23.612113 | 0.0994008 | 0.9916915 |
| healthyR.data |         7 | EARTH                      | Test  | 0.1194395 |  85.087357 | 0.1072168 |  31.230934 | 0.1925086 | 0.9732946 |
| healthyR.data |         8 | NNAR                       | Test  | 0.7063323 | 146.412865 | 0.6340506 |  89.883954 | 0.9700770 | 0.7105323 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0742402 |  22.661448 | 0.0666429 |  19.942681 | 0.1074311 | 0.9906473 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4296493 | 111.525089 | 0.3856816 |  83.585799 | 0.5339926 | 0.9910018 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8194722 | 360.536763 | 0.7356125 |  90.121552 | 1.0304441 | 0.5538848 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0578026 | 380.389698 | 0.9495536 | 128.219102 | 1.2645488 | 0.0594738 |
| healthyR.data |        13 | BATS                       | Test  | 0.8294616 | 102.332947 | 0.7445796 | 165.779400 | 1.0079897 | 0.0220709 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.8355471 | 130.848910 | 0.7500423 | 158.052125 | 0.9934542 | 0.0734021 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7016160 | 172.982494 | 0.7206460 | 148.016607 | 0.8776234 | 0.3861168 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0554023 |  13.123406 | 0.0569050 |  15.271588 | 0.0700826 | 0.9948859 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6775667 | 319.693146 | 0.6959445 | 112.497376 | 0.9213171 | 0.1077150 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0535411 |  24.520439 | 0.0549933 |  18.340645 | 0.0784117 | 0.9949017 |
| healthyR      |         7 | EARTH                      | Test  | 0.0329681 |   4.963898 | 0.0338623 |   4.984382 | 0.0703926 | 0.9945305 |
| healthyR      |         8 | NNAR                       | Test  | 0.5668308 | 176.014513 | 0.5822050 |  76.240818 | 0.6903872 | 0.9013834 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0548448 |  18.944642 | 0.0563324 |  18.535337 | 0.0801691 | 0.9940746 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4080568 | 137.996012 | 0.4191247 |  96.968335 | 0.5156345 | 0.9733432 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8469047 | 457.123365 | 0.8698754 |  83.386597 | 1.1717619 | 0.5244509 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1082073 | 653.803474 | 1.1382654 | 123.976923 | 1.3594194 | 0.0708538 |
| healthyR      |        13 | TBATS                      | Test  | 0.6688977 | 192.970057 | 0.6870403 | 139.496273 | 0.8741439 | 0.1590472 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7452456 | 295.799671 | 0.7654590 | 118.436124 | 0.9796395 | 0.0039585 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1996894 | 407.035719 | 1.2385189 | 169.143383 | 1.3797213 | 0.7657175 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0781417 |  17.423200 | 0.0806708 |  17.265230 | 0.0928208 | 0.9910481 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7874154 | 146.143175 | 0.8129011 | 145.265840 | 0.9863193 | 0.0882574 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0691517 |  19.117713 | 0.0713899 |  15.942779 | 0.0847920 | 0.9913617 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0139172 |   2.525685 | 0.0143676 |   2.481979 | 0.0202102 | 0.9995309 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0141100 |   2.012164 | 0.0145667 |   1.990417 | 0.0339653 | 0.9988689 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0797620 |  18.185982 | 0.0823437 |  20.999924 | 0.0959803 | 0.9893456 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7012518 | 260.303785 | 0.7239487 | 136.585827 | 0.7956596 | 0.9782219 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8638817 | 238.428918 | 0.8918424 |  87.441748 | 1.2750270 | 0.4535248 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0907279 | 289.788859 | 1.1260307 | 132.992931 | 1.4593619 | 0.0729312 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.9758261 | 211.890052 | 1.0074100 | 162.420916 | 1.1759983 | 0.0910288 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.8299526 | 102.306100 | 0.8568151 | 192.234108 | 1.0579775 | 0.0652878 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.8797342 | 140.828406 | 0.8870521 | 160.932350 | 1.0664653 | 0.5808156 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0708049 |  16.801652 | 0.0713938 |  13.746090 | 0.0860771 | 0.9942871 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.8786198 | 290.442763 | 0.8859284 | 107.754301 | 1.0869071 | 0.0000156 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0706433 |  16.700422 | 0.0712309 |  13.684012 | 0.0859301 | 0.9942887 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0267114 |   3.214907 | 0.0269336 |   3.212713 | 0.0499003 | 0.9979473 |
| healthyverse  |         8 | NNAR                       | Test  | 0.2338704 | 140.287336 | 0.2358158 |  74.465977 | 0.2753625 | 0.9701520 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0771096 |  20.680716 | 0.0777511 |  18.955032 | 0.0916076 | 0.9939479 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4509151 |  78.011037 | 0.4546659 |  94.357530 | 0.5683597 | 0.9837237 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8906196 | 268.142907 | 0.8980281 | 109.439750 | 1.0245643 | 0.4410119 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1733263 | 350.588589 | 1.1830864 | 124.773257 | 1.5017200 | 0.0000295 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7839397 | 118.719902 | 0.7904607 | 123.737485 | 0.9941899 | 0.1378184 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.8527838 | 311.517278 | 0.8598775 | 102.322382 | 1.0638480 | 0.4475373 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.7025616 | 238.221951 | 0.7617985 | 142.960085 | 0.8667370 | 0.3945211 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6327113 | 244.799228 | 0.6860587 | 117.714298 | 0.8752920 | 0.0899290 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0891765 |  52.829487 | 0.0966954 |  39.191939 | 0.1186430 | 0.9915391 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0164999 |   2.511855 | 0.0178911 |   2.480186 | 0.0287429 | 0.9990176 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.7355268 | 464.407591 | 0.7975431 | 119.316701 | 0.8735052 | 0.9009732 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0829451 |  42.642232 | 0.0899387 |  37.695666 | 0.1099660 | 0.9898340 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3680256 | 144.889100 | 0.3990559 |  97.261124 | 0.4670359 | 0.9821685 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0680854 | 548.579867 | 1.1581415 | 135.114058 | 1.3157361 | 0.0897343 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6187282 | 244.388564 | 0.6708966 | 115.321793 | 0.8699072 | 0.1015442 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6725177 | 200.211531 | 0.7292214 | 136.951587 | 0.8917267 | 0.0434390 |
| healthyR.ai   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         1 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         6 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         7 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         8 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         9 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        10 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        12 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        13 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        14 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ##   package .model_id .model_desc .type     mae  mape    mase smape    rmse    rsq
    ##   <chr>       <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl>  <dbl>
    ## 1 health~         6 LM          Test   0.0710 25.6   0.0637 23.6   0.0994  0.992
    ## 2 health~         2 REGRESSION  Test   0.0554 13.1   0.0569 15.3   0.0701  0.995
    ## 3 health~         7 EARTH       Test   0.0139  2.53  0.0144  2.48  0.0202  1.00 
    ## 4 health~         7 EARTH       Test   0.0267  3.21  0.0269  3.21  0.0499  0.998
    ## 5 health~         7 EARTH       Test   0.0165  2.51  0.0179  2.48  0.0287  0.999
    ## 6 TidyDe~        NA <NA>        <NA>  NA      NA    NA      NA    NA      NA

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [395|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [344|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [320|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [134|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [10 x 6]> <tibble>     <split [0|10]>   <mdl_time_tbl>

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
