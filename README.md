Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,271
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

The last day in the data set is 2021-12-16 22:45:55, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -414.12
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25271          |
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
| r\_version     |      16914 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      16914 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16914 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2141 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-16 | 2021-08-01 |       389 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1535326.26 | 1879993.56 | 357 | 26285.0 | 238655 | 3246371 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8150.48 |   15388.94 |   1 |   234.5 |   2930 |    8327 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-16 22:45:55 | 2021-08-01 10:34:38 |     14752 |

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
    ## 1 healthyR.data <tibble [360 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [350 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [301 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [275 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [90 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0039519 |  80.657081 | 0.7285233 | 101.118716 | 1.4550103 | 0.3051061 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2518919 |   9.819816 | 0.1827868 |  11.106314 | 0.7717783 | 0.7470371 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4249385 | 101.009694 | 1.0340145 | 195.187778 | 1.8790698 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2593831 |  11.169883 | 0.1882228 |  12.193708 | 0.7633003 | 0.7465393 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2491284 |   8.811397 | 0.1807814 |   9.200454 | 0.6485640 | 0.7827174 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2217587 |   7.627339 | 0.1609204 |   7.693682 | 0.6212612 | 0.7997981 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2504927 |  10.258707 | 0.1817714 |  11.445270 | 0.7669631 | 0.7517549 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7493127 |  51.102335 | 0.5437429 |  70.931918 | 1.1598027 | 0.7395451 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7946285 |  92.134726 | 0.5766266 |  68.818821 | 1.1036888 | 0.5330879 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4638155 | 116.418087 | 1.0622258 | 121.226646 | 1.9411941 | 0.1033093 |
| healthyR.data |         13 | BATS                       | Test  | 1.4056549 |  97.481277 | 1.0200213 | 181.694960 | 1.8745560 | 0.0799535 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4537913 | 102.366135 | 1.0549517 | 188.105343 | 1.9104370 | 0.0007310 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7956950 |  68.684105 | 0.7671835 |  97.889236 | 1.0129573 | 0.7858136 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1205774 |   8.082537 | 0.1162568 |   8.148327 | 0.2573379 | 0.9637747 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.3276737 | 119.374244 | 1.2801003 | 165.958124 | 1.6247785 | 0.0158404 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1226278 |   8.469642 | 0.1182338 |   8.478260 | 0.2552704 | 0.9637905 |
| healthyR      |          7 | EARTH                      | Test  | 0.0789535 |   3.762815 | 0.0761245 |   3.562143 | 0.1693664 | 0.9913772 |
| healthyR      |          8 | NNAR                       | Test  | 0.1492702 |   6.543991 | 0.1439216 |   5.897708 | 0.3307861 | 0.9823364 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1180519 |   7.790800 | 0.1138218 |   7.875906 | 0.2523214 | 0.9657596 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4246559 |  32.221525 | 0.4094395 |  39.442322 | 0.5988497 | 0.9669543 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6686950 |  88.504937 | 0.6447342 |  62.995910 | 0.9243889 | 0.7295588 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3940943 | 152.816451 | 1.3441410 | 147.673833 | 1.5931581 | 0.2621079 |
| healthyR      |         13 | TBATS                      | Test  | 1.3518933 | 122.585170 | 1.3034521 | 167.848023 | 1.6427129 | 0.0027087 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.3094166 | 113.021425 | 1.2624974 | 178.653998 | 1.5968543 | 0.0068377 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0843230 |  76.470148 | 0.8171712 | 117.357866 | 1.3529626 | 0.6967428 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1614103 |   9.470310 | 0.1216426 |   9.584032 | 0.3474527 | 0.9499112 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6157309 | 124.332001 | 1.2176526 | 166.180191 | 1.8938279 | 0.2566915 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1613933 |   9.510105 | 0.1216298 |   9.603976 | 0.3458220 | 0.9499711 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1575183 |   7.323542 | 0.1187095 |   6.921646 | 0.2527534 | 0.9757304 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1958938 |   8.801300 | 0.1476301 |   8.108414 | 0.3481287 | 0.9587645 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1569697 |   9.268933 | 0.1182961 |   9.367639 | 0.3427596 | 0.9519503 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6881867 |  44.587819 | 0.5186336 |  57.850969 | 0.9284036 | 0.9008973 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0127027 | 134.757137 | 0.7631965 |  85.285252 | 1.2045458 | 0.6743539 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6739565 | 180.571314 | 1.2615328 | 134.174458 | 2.0233533 | 0.1949821 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5563074 | 117.739723 | 1.1728697 | 165.360171 | 1.8328977 | 0.3383285 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6584631 | 115.473065 | 1.2498566 | 174.789598 | 1.9674702 | 0.0493062 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.8630837 |  88.893956 | 0.8036020 | 124.067592 | 1.0808568 | 0.6790658 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1156590 |  12.602055 | 0.1076880 |  13.334918 | 0.2147372 | 0.9736075 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3567311 | 182.843096 | 1.2632282 | 172.972998 | 1.6171357 | 0.2810395 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1138217 |  13.841602 | 0.1059774 |  16.748980 | 0.2148980 | 0.9736840 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1215293 |  10.467130 | 0.1131537 |  10.188710 | 0.1901025 | 0.9893116 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0334771 |   4.696175 | 0.0311700 |   5.409509 | 0.0729493 | 0.9966955 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1128861 |  16.346751 | 0.1051063 |  17.132233 | 0.2089332 | 0.9746838 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6444341 | 111.320368 | 0.6000212 | 101.982304 | 0.7934131 | 0.9762043 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7421954 | 239.762727 | 0.6910450 |  74.085285 | 0.9704960 | 0.7138242 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3834224 | 225.686486 | 1.2880801 | 144.414154 | 1.6923371 | 0.1887453 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3326283 | 186.979173 | 1.2407865 | 169.985449 | 1.5989169 | 0.3443371 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4280794 | 206.372459 | 1.3296594 | 176.075255 | 1.7063948 | 0.0105648 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8994954 |  74.165038 | 0.8070893 | 113.757758 | 1.1906073 | 0.4752264 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1154199 |   9.404339 | 0.1035627 |   9.231458 | 0.2148216 | 0.9747898 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.3032465 | 120.374769 | 1.1693625 | 178.229328 | 1.5810680 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1279519 |  11.741069 | 0.1148073 |  11.200304 | 0.2126922 | 0.9745975 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1031832 |   5.105578 | 0.0925831 |   5.427908 | 0.2551287 | 0.9742129 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.4051491 |  32.899760 | 0.3635276 |  43.273665 | 0.6257392 | 0.8994448 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1322425 |  12.465033 | 0.1186571 |  11.756379 | 0.2111626 | 0.9750093 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5163753 |  35.507157 | 0.4633275 |  46.840124 | 0.8228646 | 0.8086134 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8356572 | 149.447590 | 0.7498092 |  71.553346 | 1.1950717 | 0.5645419 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3745877 | 171.778629 | 1.2333748 | 125.432955 | 1.7191495 | 0.1249105 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.3018567 | 134.114766 | 1.1681156 | 174.753344 | 1.5608284 | 0.1337443 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.2993554 | 119.856127 | 1.1658712 | 178.662340 | 1.5766559 | 0.0139013 |
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
    ##   package   .model_id .model_desc   .type    mae  mape   mase smape   rmse   rsq
    ##   <chr>         <int> <chr>         <chr>  <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR          Test  0.222   7.63 0.161   7.69 0.621  0.800
    ## 2 healthyR          7 EARTH         Test  0.0790  3.76 0.0761  3.56 0.169  0.991
    ## 3 healthyR~         7 EARTH         Test  0.158   7.32 0.119   6.92 0.253  0.976
    ## 4 healthyv~         8 NNAR          Test  0.0335  4.70 0.0312  5.41 0.0729 0.997
    ## 5 healthyR~         9 PROPHET W RE~ Test  0.132  12.5  0.119  11.8  0.211  0.975

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
    ## 1 healthyR.data <tibble [360 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [350 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [301 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [275 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [90 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
