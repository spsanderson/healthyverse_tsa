Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
01 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,107
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

The last day in the data set is 2021-12-30 21:37:47, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -748.99
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26107          |
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
| r\_version     |      17524 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17524 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17524 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2215 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-30 | 2021-08-04 |       403 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1527769.02 | 1877685.84 | 357 | 17540.5 | 238433 | 3246351 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8035.07 |   15255.75 |   1 |   202.0 |   2806 |    8199 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-30 21:37:47 | 2021-08-04 05:29:26 |     15196 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |     median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-----------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 49M 8S |        60 |

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
    ## 1 healthyR.data <tibble [374 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [364 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [315 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [289 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [104 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8536606 | 102.377112 | 0.6426226 |  89.176588 | 1.1928283 | 0.5351081 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2183171 |  10.583414 | 0.1643457 |  11.409943 | 0.7113004 | 0.7718424 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0094522 | 101.191211 | 0.7599002 | 103.635203 | 1.4517399 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2214994 |  11.199184 | 0.1667414 |  12.021314 | 0.7111672 | 0.7715542 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2020373 |   6.049306 | 0.1520906 |   6.657332 | 0.6740421 | 0.7822628 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2347950 |   7.513086 | 0.1767501 |   7.789515 | 0.7359003 | 0.7419921 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2155355 |   9.932006 | 0.1622518 |  10.695006 | 0.7042089 | 0.7777322 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5086857 |  38.359472 | 0.3829308 |  50.779078 | 0.9442700 | 0.7478507 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9082037 | 125.893074 | 0.6836818 |  88.553215 | 1.1257197 | 0.5999333 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2055724 | 138.923247 | 0.9075365 | 116.800379 | 1.4784195 | 0.1380541 |
| healthyR.data |         13 | BATS                       | Test  | 1.0376340 | 100.176571 | 0.7811150 | 110.631400 | 1.4592576 | 0.0046626 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0111283 | 100.366564 | 0.7611619 | 104.627556 | 1.4518505 | 0.3027439 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7729409 | 158.817126 | 0.7447478 | 113.387252 | 0.9007047 | 0.6260403 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0902141 |   9.331971 | 0.0869235 |   9.082887 | 0.2219446 | 0.9684973 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.9224431 | 172.019872 | 0.8887969 | 122.433179 | 1.1733324 | 0.0008424 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0927035 |  10.873062 | 0.0893221 |  10.339348 | 0.2208234 | 0.9685258 |
| healthyR      |          7 | EARTH                      | Test  | 0.0403132 |   2.409311 | 0.0388427 |   2.325767 | 0.1050191 | 0.9939532 |
| healthyR      |          8 | NNAR                       | Test  | 0.0798412 |   7.184906 | 0.0769290 |   6.909506 | 0.1778352 | 0.9875414 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0926939 |  10.632240 | 0.0893129 |  10.076476 | 0.2165939 | 0.9698524 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3502864 |  70.303165 | 0.3375097 |  72.876615 | 0.4760172 | 0.9832815 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0274325 | 232.080585 | 0.9899568 | 112.025174 | 1.2793758 | 0.5570991 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2620793 | 248.799573 | 1.2160448 | 133.581698 | 1.5173776 | 0.1056318 |
| healthyR      |         13 | TBATS                      | Test  | 0.9358820 | 189.244854 | 0.9017456 | 126.031979 | 1.1659166 | 0.0093990 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.9024928 | 170.310605 | 0.8695743 | 118.925019 | 1.1539298 | 0.3529450 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 1.0053305 | 115.958669 | 0.6209814 | 108.148346 | 1.1840214 | 0.4860546 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1280349 |   7.358582 | 0.0790857 |   7.591803 | 0.3369109 | 0.9532230 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0867731 | 114.692071 | 0.6712876 | 113.620650 | 1.3314041 | 0.1077634 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1280889 |   7.443818 | 0.0791191 |   7.657549 | 0.3369384 | 0.9533710 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1102961 |   4.943840 | 0.0681287 |   4.827498 | 0.2135719 | 0.9776904 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1429219 |   7.090958 | 0.0882813 |   6.552592 | 0.2832311 | 0.9634385 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1300153 |   8.107522 | 0.0803090 |   8.231561 | 0.3330435 | 0.9547644 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5199804 |  39.291566 | 0.3211861 |  49.456083 | 0.6938675 | 0.9660280 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0875852 | 172.013258 | 0.6717892 |  96.499846 | 1.3309767 | 0.5942126 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4423735 | 211.361264 | 0.8909380 | 124.213047 | 1.6798383 | 0.0971510 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.2859021 | 107.652122 | 0.7942873 | 161.814295 | 1.5285938 | 0.0569520 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1161679 | 116.574558 | 0.6894444 | 111.776991 | 1.4029496 | 0.3039353 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8003764 | 181.228922 | 0.7554576 | 138.353482 | 0.9705192 | 0.5735973 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1051571 |  20.063023 | 0.0992555 |  22.009063 | 0.2001269 | 0.9765929 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9369640 | 226.583379 | 0.8843796 | 153.958230 | 1.1493078 | 0.1221327 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1123033 |  24.898574 | 0.1060006 |  26.013023 | 0.2040874 | 0.9764574 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0524194 |   4.817115 | 0.0494775 |   4.723145 | 0.1211624 | 0.9924465 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0434748 |   4.043063 | 0.0410349 |   3.830094 | 0.1222532 | 0.9934211 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1033937 |  20.008749 | 0.0975911 |  20.857581 | 0.1947722 | 0.9779863 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3652245 |  51.289117 | 0.3447274 |  59.489712 | 0.5199551 | 0.9852817 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8009588 | 340.233499 | 0.7560073 | 101.509259 | 0.9941420 | 0.7071900 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2341854 | 560.455927 | 1.1649203 | 132.971416 | 1.4033906 | 0.1359597 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8870078 | 109.991597 | 0.8372271 | 145.234395 | 1.1662140 | 0.3752729 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.9237278 | 155.388295 | 0.8718862 | 147.497266 | 1.2129346 | 0.3717795 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7839204 |  96.108807 | 0.6050458 | 107.162603 | 0.9928698 | 0.8881660 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1409131 |  15.858685 | 0.1087596 |  19.077543 | 0.2133343 | 0.9694229 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1201173 | 100.676999 | 0.8645294 | 176.744102 | 1.3725098 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1409246 |  15.751392 | 0.1087685 |  18.901339 | 0.2142673 | 0.9690132 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0467057 |   3.414304 | 0.0360484 |   3.444243 | 0.1023453 | 0.9928020 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2236984 |  20.843468 | 0.1726550 |  21.922088 | 0.3412781 | 0.9636387 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1321970 |  15.438571 | 0.1020323 |  17.058990 | 0.2122206 | 0.9677051 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5411348 |  99.300874 | 0.4176589 |  82.618485 | 0.7152389 | 0.8653536 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9101927 | 144.712232 | 0.7025053 |  87.107208 | 1.1938101 | 0.5803884 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2660690 | 168.829368 | 0.9771779 | 132.687963 | 1.5242270 | 0.1640805 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0554232 | 112.662695 | 0.8145972 | 158.039419 | 1.2720345 | 0.1222111 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9943258 | 110.712115 | 0.7674410 | 136.109838 | 1.2138373 | 0.1586475 |
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
    ##   package      .model_id .model_desc .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>            <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.da~         7 EARTH       Test  0.202   6.05 0.152   6.66 0.674 0.782
    ## 2 healthyR             7 EARTH       Test  0.0403  2.41 0.0388  2.33 0.105 0.994
    ## 3 healthyR.ts          7 EARTH       Test  0.110   4.94 0.0681  4.83 0.214 0.978
    ## 4 healthyverse         7 EARTH       Test  0.0524  4.82 0.0495  4.72 0.121 0.992
    ## 5 healthyR.ai          7 EARTH       Test  0.0467  3.41 0.0360  3.44 0.102 0.993

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
    ## 1 healthyR.data <tibble [374 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [364 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [315 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [289 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [104 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
