Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
13 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,598
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

The last day in the data set is 2022-01-11 21:48:17, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1037.16
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26598          |
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
| r\_version     |      17798 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17798 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17798 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2234 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-11 | 2021-08-05 |       415 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|--------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size           |          0 |              1 | 1533230 | 1878540.94 | 357 | 17579 | 238827 | 3246662.00 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8071 |   15303.67 |   1 |   209 |   2803 |    8230.75 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-11 21:48:17 | 2021-08-05 13:37:49 |     15522 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   58.5 |        60 |

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
    ## 1 healthyR.data <tibble [386 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [376 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [326 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [301 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [115 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.6983994 | 278.800150 | 0.5954112 |  98.488822 | 0.9024632 | 0.5096177 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0947797 |  31.561635 | 0.0808032 |  26.081371 | 0.1195534 | 0.9889265 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0588286 | 440.213733 | 0.9026903 | 107.825130 | 1.3404797 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0955369 |  33.759891 | 0.0814487 |  25.553622 | 0.1143867 | 0.9938375 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0275074 |   9.648549 | 0.0234511 |   7.106433 | 0.0447162 | 0.9980870 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0215528 |  11.561972 | 0.0183746 |  10.648563 | 0.0309216 | 0.9990728 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1021040 |  34.124214 | 0.0870474 |  26.288088 | 0.1290806 | 0.9915614 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4814367 | 229.845276 | 0.4104425 |  85.170542 | 0.5863904 | 0.9890390 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0693618 | 570.613593 | 0.9116702 | 104.494161 | 1.3106317 | 0.5594072 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3786612 | 374.427699 | 1.1753592 | 134.505985 | 1.6617682 | 0.1733590 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9751453 | 367.498314 | 0.8313472 | 106.261437 | 1.2402726 | 0.1589922 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0574595 | 439.528395 | 0.9015231 | 107.776678 | 1.3391386 | 0.0114888 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8853661 | 341.480240 | 0.9706555 | 153.875266 | 0.9538643 | 0.5286559 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0351691 |  14.089256 | 0.0385570 |  16.527156 | 0.0431184 | 0.9991161 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.0968775 | 438.953352 | 1.2025423 | 153.271228 | 1.2275550 | 0.0454520 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0387407 |  17.784624 | 0.0424727 |  18.188488 | 0.0485066 | 0.9991243 |
| healthyR      |          7 | EARTH                      | Test  | 0.0180401 |   5.675665 | 0.0197779 |   5.275914 | 0.0210625 | 0.9993660 |
| healthyR      |          8 | NNAR                       | Test  | 0.0968190 |  18.425538 | 0.1061458 |  18.410441 | 0.2728758 | 0.9354756 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0438169 |  20.323385 | 0.0480379 |  19.705689 | 0.0595986 | 0.9979938 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4035994 | 185.590199 | 0.4424791 | 120.351026 | 0.4634874 | 0.9971425 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2640721 | 669.607461 | 1.3858433 | 131.602712 | 1.4978621 | 0.4903835 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2962246 | 511.414393 | 1.4210931 | 136.444726 | 1.6911000 | 0.2171027 |
| healthyR      |         13 | TBATS                      | Test  | 0.7531480 | 254.831627 | 0.8257006 | 144.392562 | 0.9063958 | 0.1479155 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.0676513 | 472.713843 | 1.1705007 | 148.942345 | 1.2232919 | 0.0621473 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7976755 | 322.197431 | 0.5069597 | 116.191907 | 0.9534040 | 0.4013458 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0546583 |  48.809174 | 0.0347379 |  19.703682 | 0.0644003 | 0.9970209 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.3278860 | 831.494460 | 0.8439331 | 122.854677 | 1.5472703 | 0.1335305 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0737307 |  48.482349 | 0.0468593 |  20.585909 | 0.0912622 | 0.9971903 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0272723 |   3.137902 | 0.0173328 |   3.103105 | 0.0473315 | 0.9979948 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0448175 |   5.813560 | 0.0284836 |   5.892084 | 0.1446399 | 0.9827623 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0989797 |  70.470191 | 0.0629062 |  30.099331 | 0.1233422 | 0.9955977 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4016918 | 246.752595 | 0.2552938 |  87.633787 | 0.4750320 | 0.9922735 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2652327 | 705.024739 | 0.8041140 | 123.176860 | 1.4704329 | 0.4899612 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.7093455 | 924.864991 | 1.0863683 | 134.222439 | 2.0419144 | 0.1058542 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.3091088 | 809.446189 | 0.8319994 | 122.910382 | 1.5198903 | 0.1579366 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2764509 | 875.729576 | 0.8112438 | 118.582238 | 1.5578374 | 0.0216689 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0479123 | 362.899565 | 1.0035959 | 158.096754 | 1.2194405 | 0.5533743 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0685870 |  22.888677 | 0.0656864 |  17.551739 | 0.0811044 | 0.9975207 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.1997000 | 409.686047 | 1.1489645 | 162.129894 | 1.3590253 | 0.2225340 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0731443 |  25.591018 | 0.0700510 |  19.216213 | 0.0857609 | 0.9974517 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0096293 |   2.819105 | 0.0092221 |   2.697025 | 0.0142384 | 0.9997812 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0051246 |   1.474557 | 0.0049079 |   1.453003 | 0.0070458 | 0.9999356 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0641643 |  20.901940 | 0.0614507 |  15.862844 | 0.0770043 | 0.9966703 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5613622 | 240.516361 | 0.5376222 | 129.253171 | 0.6342366 | 0.9786381 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0868653 | 579.237287 | 1.0409016 | 122.795746 | 1.2437686 | 0.5491155 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3327988 | 543.609308 | 1.2764346 | 141.303439 | 1.6710868 | 0.2122726 |
| healthyverse  |         13 | TBATS                      | Test  | 0.7751715 | 162.393705 | 0.7423894 | 162.627620 | 0.9395350 | 0.1750482 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.2068946 | 439.793273 | 1.1558549 | 159.928578 | 1.4047437 | 0.0381697 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2993833 | 283.627246 | 0.8528937 | 120.065066 | 1.5532333 | 0.6004368 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1175035 |  19.622911 | 0.0771274 |  21.343382 | 0.1357619 | 0.9970486 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.0034231 | 198.314374 | 0.6586304 | 113.147656 | 1.2723234 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1166108 |  19.557492 | 0.0765414 |  21.373418 | 0.1345611 | 0.9972103 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0244858 |   2.888784 | 0.0160721 |   2.859101 | 0.0338196 | 0.9989236 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2838522 |  34.704850 | 0.1863159 |  43.791790 | 0.3721205 | 0.9552769 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1572664 |  25.598197 | 0.1032270 |  26.452141 | 0.1785709 | 0.9931751 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9975244 | 208.698287 | 0.6547586 | 118.981097 | 1.1375125 | 0.8582739 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1608179 | 205.136172 | 0.7619417 |  99.916415 | 1.4062220 | 0.5859962 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3541362 | 282.426012 | 0.8888326 | 121.976373 | 1.5945588 | 0.1510661 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.1213194 | 229.520421 | 0.7360155 | 118.980936 | 1.3541821 | 0.0384849 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0733823 | 225.063612 | 0.7045504 | 111.559623 | 1.3953233 | 0.0081341 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0216  11.6  0.0184  10.6  0.0309  0.999
    ## 2 healthyR         7 EARTH       Test  0.0180   5.68 0.0198   5.28 0.0211  0.999
    ## 3 healthy~         7 EARTH       Test  0.0273   3.14 0.0173   3.10 0.0473  0.998
    ## 4 healthy~         8 NNAR        Test  0.00512  1.47 0.00491  1.45 0.00705 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0245   2.89 0.0161   2.86 0.0338  0.999

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
    ## 1 healthyR.data <tibble [386 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [376 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [326 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [301 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [115 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
