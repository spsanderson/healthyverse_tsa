Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,525
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

The last day in the data set is 2022-03-03 23:03:25, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2262.42
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30525         |
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
| r_version     |     20284 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20284 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20284 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2543 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-03 | 2021-09-02 |      466 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1506518.16 | 1866498.50 | 357 | 16920 | 271098 | 3247734 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8296.73 |   15966.25 |   1 |   243 |   2687 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-03 23:03:25 | 2021-09-02 12:21:54 |    17754 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 0M 0S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [407|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [399|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [348|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [324|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [138|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [14 x 6]> <tibble>     <split [0|14]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.7766121 | 244.726457 | 0.6344480 | 134.872278 | 0.9520189 | 0.2484572 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0791659 |  20.662857 | 0.0646741 |  17.480426 | 0.1039038 | 0.9895317 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.8481516 | 338.396602 | 0.6928918 | 123.311161 | 1.0141002 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.1182674 |  50.445103 | 0.0966177 |  32.947810 | 0.1453185 | 0.9926114 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2234781 | 113.718251 | 0.1825690 |  47.664090 | 0.3566399 | 0.9067923 |
| healthyR.data |         8 | NNAR                       | Test  | 1.9214783 | 948.678000 | 1.5697389 | 145.721148 | 2.1221694 | 0.7679593 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.1187001 |  48.766077 | 0.0969713 |  33.753226 | 0.1519019 | 0.9906660 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3319223 | 116.802618 | 0.2711617 |  69.766995 | 0.4194871 | 0.9919901 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8012258 | 441.387873 | 0.6545561 |  91.840340 | 1.0505517 | 0.4975797 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1550474 | 512.243999 | 0.9436083 | 131.097054 | 1.4078159 | 0.0289276 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7862481 | 330.075209 | 0.6423201 | 118.088336 | 0.9652539 | 0.1421684 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.8510549 | 341.825642 | 0.6952636 | 123.212225 | 1.0177005 | 0.0023338 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6301139 | 210.518134 | 0.7265986 | 138.210924 | 0.7714079 | 0.2225747 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0488347 |  15.462472 | 0.0563124 |  18.591890 | 0.0570666 | 0.9977912 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6696512 | 407.103266 | 0.7721900 | 114.746139 | 0.9167968 | 0.0819151 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0466288 |  13.296093 | 0.0537687 |  16.639477 | 0.0546569 | 0.9978000 |
| healthyR      |         7 | EARTH                      | Test  | 0.0234313 |   5.670709 | 0.0270192 |   5.680415 | 0.0342640 | 0.9989498 |
| healthyR      |         8 | NNAR                       | Test  | 0.2305805 | 149.917330 | 0.2658876 |  65.797137 | 0.2640709 | 0.9516329 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0490619 |  16.372623 | 0.0565744 |  21.285055 | 0.0588706 | 0.9967407 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3207379 | 113.225306 | 0.3698501 |  94.336983 | 0.3707077 | 0.9989083 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9214683 | 618.213927 | 1.0625660 | 101.006025 | 1.2313129 | 0.4221588 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1880627 | 825.181989 | 1.3699820 | 122.600604 | 1.4520374 | 0.0511321 |
| healthyR      |        13 | TBATS                      | Test  | 0.5988671 | 240.707680 | 0.6905672 | 122.101743 | 0.8148422 | 0.0578753 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6459875 | 294.677251 | 0.7449028 | 110.226026 | 0.8825986 | 0.0107257 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.8183881 | 249.798182 | 0.9417553 | 156.138136 | 1.0367097 | 0.1305752 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0606121 |  29.635365 | 0.0697490 |  21.746692 | 0.0747455 | 0.9925539 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6163473 | 340.800372 | 0.7092580 | 119.892128 | 0.7917219 | 0.0224202 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0594713 |  27.396033 | 0.0684362 |  20.589418 | 0.0732853 | 0.9927546 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0122797 |   3.257555 | 0.0141308 |   3.171819 | 0.0159864 | 0.9995722 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1593832 | 110.026000 | 0.1834092 |  58.847316 | 0.1831989 | 0.9725091 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0651339 |  16.983097 | 0.0749524 |  18.719578 | 0.0795253 | 0.9912290 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5486149 | 268.331139 | 0.6313153 | 131.643575 | 0.6225864 | 0.9870312 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9706442 | 286.906792 | 1.1169630 | 102.454457 | 1.3752270 | 0.2379183 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2360151 | 538.049416 | 1.4223370 | 143.033287 | 1.5750143 | 0.0082340 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7120357 | 159.713372 | 0.8193709 | 145.667911 | 0.9267028 | 0.0812204 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6062514 | 317.712301 | 0.6976402 | 113.717954 | 0.7706170 | 0.0800071 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.6918634 | 114.594927 | 0.6693797 | 146.599105 | 0.8765786 | 0.6144847 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.0386920 | 517.199891 | 1.0049373 | 114.501587 | 1.2879666 | 0.0189229 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0518309 |  18.899634 | 0.0501466 |  15.359445 | 0.0665119 | 0.9962121 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0219260 |   3.188835 | 0.0212135 |   3.202564 | 0.0391431 | 0.9985167 |
| healthyverse  |         8 | NNAR                       | Test  | 0.3215968 | 214.380088 | 0.3111458 |  89.333217 | 0.3911420 | 0.9762506 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0552244 |  17.414560 | 0.0534298 |  17.543132 | 0.0692789 | 0.9959934 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3664829 |  80.647859 | 0.3545732 |  94.146078 | 0.4653661 | 0.9904085 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2155695 | 511.889652 | 1.1760668 | 126.669994 | 1.6041167 | 0.0196256 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7052451 | 171.001556 | 0.6823265 | 121.722436 | 0.8801634 | 0.1722225 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.0077078 | 499.313920 | 0.9749601 | 114.887774 | 1.2241570 | 0.3003402 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.5526592 | 248.330750 | 0.6588086 | 127.820328 | 0.6723752 | 0.3669664 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0539620 |  23.308200 | 0.0643265 |  20.623301 | 0.0698820 | 0.9969980 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7836921 | 510.620745 | 0.9342160 | 133.983624 | 1.0193627 | 0.0584583 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0671279 |  38.716094 | 0.0800212 |  29.711810 | 0.0857115 | 0.9964054 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0148762 |   2.611543 | 0.0177335 |   2.576997 | 0.0264522 | 0.9990652 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.1707255 | 115.516342 | 0.2035168 |  57.973150 | 0.1917063 | 0.9910043 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0681821 |  38.986489 | 0.0812778 |  32.179534 | 0.0881559 | 0.9949657 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3325201 | 198.168245 | 0.3963873 | 104.348310 | 0.3723061 | 0.9981093 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8797668 | 508.693632 | 1.0487438 | 116.793198 | 1.1771385 | 0.3878833 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1634303 | 790.426897 | 1.3868906 | 139.688577 | 1.4354759 | 0.0858049 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6910430 | 326.311191 | 0.8237717 | 125.873595 | 0.9280381 | 0.0633852 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6706685 | 417.686790 | 0.7994839 | 118.352639 | 0.9262412 | 0.0134464 |
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
    ## 1 health~         2 REGRESSION  Test   0.0792 20.7   0.0647 17.5   0.104   0.990
    ## 2 health~         7 EARTH       Test   0.0234  5.67  0.0270  5.68  0.0343  0.999
    ## 3 health~         7 EARTH       Test   0.0123  3.26  0.0141  3.17  0.0160  1.00 
    ## 4 health~         7 EARTH       Test   0.0219  3.19  0.0212  3.20  0.0391  0.999
    ## 5 health~         7 EARTH       Test   0.0149  2.61  0.0177  2.58  0.0265  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [407|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [399|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [348|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [324|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [138|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [14 x 6]> <tibble>     <split [0|14]>   <mdl_time_tbl>

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
