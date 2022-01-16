Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
16 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,866
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

The last day in the data set is 2022-01-14 20:18:54, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1107.67
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26866          |
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
| r\_version     |      17979 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17979 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17979 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2247 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-14 | 2021-08-08 |       418 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533356.17 | 1878578.73 | 357 | 16914.00 | 238827 | 3247008 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8070.51 |   15294.63 |   1 |   219.25 |   2798 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-14 20:18:54 | 2021-08-08 21:52:47 |     15671 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     46 |        60 |

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
    ## 1 healthyR.data <tibble [389 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [379 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [329 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [304 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [118 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7119840 | 236.747311 | 0.5558634 |  99.008100 | 0.9199107 | 0.5062857 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1434345 |  27.300065 | 0.1119828 |  21.655343 | 0.3361616 | 0.9175187 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9412090 | 314.416110 | 0.7348249 | 102.165464 | 1.2484666 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0828970 |  21.813360 | 0.0647197 |  17.637949 | 0.1018651 | 0.9946425 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0225820 |   7.630291 | 0.0176303 |   5.731721 | 0.0304686 | 0.9992740 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0193230 |   3.516828 | 0.0150859 |   3.492936 | 0.0274307 | 0.9993480 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0843911 |  20.948422 | 0.0658862 |  17.804042 | 0.1151132 | 0.9923459 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4436810 | 170.438883 | 0.3463927 |  77.857398 | 0.5527868 | 0.9936284 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0259257 | 503.479339 | 0.8009653 |  92.811241 | 1.3114233 | 0.5931598 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2560880 | 394.579925 | 0.9806586 | 128.828014 | 1.4498855 | 0.2086672 |
| healthyR.data |         13 | BATS                       | Test  | 0.8682034 | 277.267585 | 0.6778277 | 101.557383 | 1.1447907 | 0.0707831 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9401964 | 313.925533 | 0.7340344 | 102.129521 | 1.2474377 | 0.0075894 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7959975 | 368.499334 | 0.8060154 | 151.013853 | 0.8904697 | 0.5482347 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0334655 |  11.386964 | 0.0338867 |  14.891646 | 0.0418320 | 0.9992404 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8306159 | 339.307215 | 0.8410695 | 148.828883 | 0.9797978 | 0.0442086 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0361574 |  16.149124 | 0.0366125 |  19.098165 | 0.0465521 | 0.9992508 |
| healthyR      |          7 | EARTH                      | Test  | 0.0186228 |   6.562573 | 0.0188571 |   6.033949 | 0.0213756 | 0.9994319 |
| healthyR      |          8 | NNAR                       | Test  | 0.1204902 |  17.722260 | 0.1220066 |  14.920715 | 0.3110646 | 0.9427638 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0391433 |  16.331755 | 0.0396359 |  18.025977 | 0.0553055 | 0.9984493 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4396745 | 242.535423 | 0.4452080 | 124.834740 | 0.4967670 | 0.9949550 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2530639 | 727.975309 | 1.2688342 | 128.534990 | 1.5174151 | 0.5177969 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1600475 | 557.343194 | 1.1746471 | 132.977932 | 1.5089410 | 0.2416286 |
| healthyR      |         13 | TBATS                      | Test  | 0.6266023 | 175.962765 | 0.6344883 | 150.387323 | 0.7839657 | 0.1018138 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8955454 | 430.196473 | 0.9068162 | 146.903749 | 1.0490824 | 0.0089132 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8151336 | 275.862659 | 0.5258108 | 113.392586 | 1.0026651 | 0.2161508 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0653837 |  45.275505 | 0.0421765 |  20.694506 | 0.0751931 | 0.9958605 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9148604 | 424.864215 | 0.5901407 | 115.260626 | 1.0796748 | 0.1610927 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0797096 |  45.237449 | 0.0514176 |  21.769228 | 0.0992793 | 0.9961947 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0346547 |   3.217022 | 0.0223544 |   3.159278 | 0.0735982 | 0.9958713 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0415429 |   7.264673 | 0.0267977 |   6.404148 | 0.0884253 | 0.9940927 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1086896 |  64.289627 | 0.0701114 |  31.537860 | 0.1330246 | 0.9950942 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4027414 | 222.952834 | 0.2597927 |  81.528474 | 0.4745013 | 0.9887492 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3138817 | 670.670858 | 0.8475337 | 122.905273 | 1.5594662 | 0.5304465 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3351038 | 784.271949 | 0.8612233 | 131.466118 | 1.6164460 | 0.1513211 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6916145 | 183.157689 | 0.4461335 | 118.211931 | 0.8597741 | 0.4493117 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0112099 | 548.600317 | 0.6522920 | 114.086327 | 1.2203563 | 0.0434719 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9351013 | 293.307443 | 0.8545011 | 156.098833 | 1.1140504 | 0.4807741 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0676489 |  21.469363 | 0.0618179 |  17.132111 | 0.0793178 | 0.9978432 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8912882 | 230.062791 | 0.8144644 | 161.864059 | 1.0625861 | 0.2113632 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0722106 |  23.972548 | 0.0659865 |  18.785543 | 0.0842266 | 0.9977945 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0107930 |   2.876075 | 0.0098627 |   2.768175 | 0.0154970 | 0.9997484 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0066167 |   1.744568 | 0.0060464 |   1.687045 | 0.0119070 | 0.9998069 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0620660 |  18.917600 | 0.0567163 |  14.961332 | 0.0738586 | 0.9969538 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5497492 | 210.439262 | 0.5023640 | 125.414976 | 0.6217526 | 0.9829777 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1939031 | 605.548797 | 1.0909957 | 122.924586 | 1.3972742 | 0.5096536 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1448596 | 565.132652 | 1.0461795 | 125.638192 | 1.4766464 | 0.1994913 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5522743 | 143.835390 | 0.5046715 | 124.886300 | 0.6600766 | 0.3806576 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9908755 | 290.789360 | 0.9054679 | 156.366604 | 1.1990792 | 0.0017306 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9890815 | 201.707948 | 0.5943176 | 110.140307 | 1.2761405 | 0.8783567 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1144266 |  17.845710 | 0.0687565 |  20.147133 | 0.1311517 | 0.9978769 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSAAN                     | Test  | 1.0397568 | 178.609651 | 0.6247673 | 115.010970 | 1.2969306 | 0.0000929 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1139843 |  17.805328 | 0.0684907 |  20.172570 | 0.1304143 | 0.9979544 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0303827 |   3.277989 | 0.0182563 |   3.226960 | 0.0473560 | 0.9983537 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2361162 |  27.367775 | 0.1418771 |  34.816372 | 0.3089050 | 0.9694327 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1495619 |  23.338651 | 0.0898685 |  24.870484 | 0.1697526 | 0.9957541 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9103257 | 175.368901 | 0.5469949 | 115.560719 | 1.0663179 | 0.8883320 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9818757 | 150.962889 | 0.5899878 |  88.230976 | 1.2612489 | 0.6405328 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1536316 | 211.173365 | 0.6931921 | 127.905404 | 1.3673564 | 0.1945456 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8838144 | 145.877474 | 0.5310649 | 118.218980 | 1.0828785 | 0.1290247 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0190421 | 171.461410 | 0.6123202 | 116.498343 | 1.2516472 | 0.0000929 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.0193   3.52 0.0151   3.49 0.0274 0.999
    ## 2 healthyR          7 EARTH       Test  0.0186   6.56 0.0189   6.03 0.0214 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0347   3.22 0.0224   3.16 0.0736 0.996
    ## 4 healthyv~         8 NNAR        Test  0.00662  1.74 0.00605  1.69 0.0119 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0304   3.28 0.0183   3.23 0.0474 0.998

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
    ## 1 healthyR.data <tibble [389 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [379 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [329 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [304 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [118 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
