Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
08 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,315
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

The last day in the data set is 2022-01-06 22:59:12, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -918.35
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26315          |
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
| r\_version     |      17616 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17616 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17616 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2221 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-06 | 2021-08-04 |       410 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1530839.63 | 1878493.26 | 357 | 17189.5 | 238637 | 3246522 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8048.63 |   15250.17 |   1 |   204.5 |   2817 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-06 22:59:12 | 2021-08-04 13:42:08 |     15340 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 49M 34S |        60 |

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
    ## 1 healthyR.data <tibble [381 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [371 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [321 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [296 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [110 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.3079177 |  385.312867 | 1.1704690 | 114.483033 | 1.5880538 | 0.0203797 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1839758 |   40.924850 | 0.1646419 |  42.951709 | 0.3454450 | 0.8961076 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.5898213 |  566.565158 | 1.4227474 | 123.341859 | 1.8057684 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.1193680 |   33.957525 | 0.1068237 |  29.777586 | 0.1399946 | 0.9913699 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0330541 |    8.549820 | 0.0295805 |   7.132687 | 0.0619442 | 0.9957121 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0304998 |    5.415563 | 0.0272945 |   5.058370 | 0.0465286 | 0.9971558 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.1387613 |   38.735860 | 0.1241789 |  31.740459 | 0.1633964 | 0.9901938 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4455422 |  177.462317 | 0.3987203 |  86.490516 | 0.5364772 | 0.9885971 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2608846 |  491.811926 | 1.1283786 | 113.530873 | 1.4869632 | 0.4892451 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.8850120 |  383.877469 | 1.6869167 | 135.547574 | 2.2243346 | 0.1141936 |
| healthyR.data |         13 | TBATS                      | Test  | 2.0803267 |  701.670744 | 1.8617058 | 132.894873 | 2.2833837 | 0.0220710 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.5880780 |  565.909476 | 1.4211873 | 123.297400 | 1.8042119 | 0.0033858 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 1.3777291 |  384.952051 | 1.4354471 | 148.182743 | 1.4894001 | 0.2839655 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0489223 |   11.555595 | 0.0509718 |  10.879568 | 0.0650061 | 0.9983055 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.5449314 |  444.771584 | 1.6096542 | 147.941520 | 1.7000277 | 0.0045980 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0471475 |   13.128542 | 0.0491227 |  12.407758 | 0.0584337 | 0.9991596 |
| healthyR      |          7 | EARTH                      | Test  | 0.0105300 |    1.684829 | 0.0109712 |   1.678213 | 0.0187733 | 0.9994640 |
| healthyR      |          8 | NNAR                       | Test  | 0.1092810 |   14.327447 | 0.1138591 |  11.195057 | 0.3693638 | 0.8935843 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0539809 |   15.525002 | 0.0562424 |  14.477733 | 0.0702888 | 0.9982797 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3939425 |  109.656577 | 0.4104462 | 109.052434 | 0.4655236 | 0.9964185 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3175437 |  327.940664 | 1.3727404 | 135.619212 | 1.5194944 | 0.3952112 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.8655024 |  512.626542 | 1.9436551 | 163.988337 | 2.2244307 | 0.1064100 |
| healthyR      |         13 | TBATS                      | Test  | 1.6603974 |  474.038515 | 1.7299575 | 149.644037 | 1.8161446 | 0.0010803 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.5453734 |  448.099159 | 1.6101148 | 147.151403 | 1.7090118 | 0.0733204 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9782714 |  325.105636 | 0.6274344 | 108.326638 | 1.2537085 | 0.2012073 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0931436 |   30.368397 | 0.0597396 |  21.833951 | 0.1603890 | 0.9846038 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.7517131 |  652.777090 | 1.1234972 | 127.626257 | 1.9749184 | 0.0925204 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0822336 |   16.090854 | 0.0527422 |  16.169621 | 0.1019002 | 0.9966418 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0367484 |    2.363273 | 0.0235693 |   2.311573 | 0.0865500 | 0.9952590 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0654608 |    7.910682 | 0.0419846 |   7.107963 | 0.1655745 | 0.9836412 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1197809 |   30.770323 | 0.0768240 |  30.861019 | 0.1381193 | 0.9951651 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3700107 |  104.604883 | 0.2373139 |  78.744221 | 0.4365581 | 0.9955483 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3459374 |  385.419339 | 0.8632446 | 120.878791 | 1.5390492 | 0.4478171 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9993854 |  766.579019 | 1.2823469 | 136.038593 | 2.3540594 | 0.0683466 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.9581388 |  736.470839 | 1.2558925 | 129.990957 | 2.2017767 | 0.0005339 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6800766 |  627.993343 | 1.0775516 | 123.812375 | 1.9693930 | 0.1341291 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 2.0256978 |  833.878357 | 2.0140724 | 164.956165 | 2.1669516 | 0.0632743 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0705680 |   16.637542 | 0.0701630 |  18.915857 | 0.0942089 | 0.9949980 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.7116986 |  717.662509 | 1.7018752 | 159.808701 | 1.8666759 | 0.1227201 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0647479 |   15.220832 | 0.0643763 |  15.914948 | 0.0807028 | 0.9969991 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0101057 |    2.882594 | 0.0100477 |   2.835046 | 0.0149183 | 0.9998437 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0082754 |    1.880255 | 0.0082280 |   1.896943 | 0.0126514 | 0.9998324 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0569488 |   13.752584 | 0.0566220 |  12.214196 | 0.0705789 | 0.9970811 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4950333 |  194.178135 | 0.4921923 | 119.057101 | 0.5862208 | 0.9860047 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0804320 |  420.957953 | 1.0742315 | 129.292170 | 1.1909561 | 0.5504446 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.9583625 | 1036.368118 | 1.9471235 | 163.391864 | 2.2693411 | 0.1287731 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1624498 |  507.426901 | 1.1557785 | 149.599680 | 1.3556216 | 0.1647253 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.7409720 |  684.324587 | 1.7309806 | 160.199517 | 1.9144285 | 0.0862038 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.8720822 |  386.169569 | 1.3636954 | 134.345837 | 2.0583376 | 0.3782485 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1408020 |   23.058375 | 0.1025655 |  24.106993 | 0.1612484 | 0.9961345 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1924560 |  252.854501 | 0.8686300 | 114.221383 | 1.5117445 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1411436 |   23.675983 | 0.1028143 |  25.446730 | 0.1600601 | 0.9969966 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0274550 |    2.786266 | 0.0199992 |   2.797491 | 0.0405451 | 0.9983142 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2576094 |   46.744984 | 0.1876524 |  49.751604 | 0.3304415 | 0.9083191 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1684411 |   27.514886 | 0.1226989 |  29.278532 | 0.1937567 | 0.9935206 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9855168 |  204.959237 | 0.7178877 | 118.886326 | 1.1100556 | 0.9028625 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1064783 |  202.738260 | 0.8060006 | 109.660429 | 1.2632082 | 0.5492631 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5318322 |  334.553208 | 1.1158444 | 127.947227 | 1.7787345 | 0.1293955 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3427789 |  283.677841 | 0.9781308 | 120.476646 | 1.6199201 | 0.0171710 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3561349 |  287.132502 | 0.9878599 | 118.813097 | 1.6676997 | 0.0733003 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |

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
    ## 1 healthyR~         8 NNAR        Test  0.0305   5.42 0.0273   5.06 0.0465 0.997
    ## 2 healthyR          7 EARTH       Test  0.0105   1.68 0.0110   1.68 0.0188 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0367   2.36 0.0236   2.31 0.0866 0.995
    ## 4 healthyv~         8 NNAR        Test  0.00828  1.88 0.00823  1.90 0.0127 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0275   2.79 0.0200   2.80 0.0405 0.998

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
    ## 1 healthyR.data <tibble [381 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [371 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [321 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [296 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [110 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
