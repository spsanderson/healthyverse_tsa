Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
20 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 25,383
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

The last day in the data set is 2021-12-18 21:17:01, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -460.64
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 25383          |
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
| r\_version     |      17000 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17000 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17000 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2151 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-18 | 2021-08-02 |       391 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1533955.85 | 1879681.13 | 357 | 24837 | 238655 | 3246351 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8135.67 |   15363.03 |   1 |   238 |   2886 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-18 21:17:01 | 2021-08-02 19:53:08 |     14807 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 45M 45S |        60 |

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
    ## 1 healthyR.data <tibble [362 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [352 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [303 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [277 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [92 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0409081 |  82.631427 | 0.7574388 | 100.874058 | 1.4635277 | 0.2834800 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2507792 |   9.444285 | 0.1824848 |  10.747742 | 0.7694764 | 0.7654290 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.4463489 | 100.608353 | 1.0524663 | 197.769137 | 1.8851212 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2578046 |  11.028625 | 0.1875970 |  12.061181 | 0.7609525 | 0.7652380 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2434510 |   8.351313 | 0.1771522 |   8.714529 | 0.6438525 | 0.8021622 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2243216 |   7.415116 | 0.1632323 |   7.826041 | 0.6629871 | 0.7901021 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2501805 |   9.941242 | 0.1820491 |  11.142900 | 0.7643851 | 0.7694113 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7499716 |  50.671506 | 0.5457327 |  70.912623 | 1.1543675 | 0.7587462 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8136866 |  90.939590 | 0.5920962 |  66.169360 | 1.1229461 | 0.5434920 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4683290 | 112.350617 | 1.0684605 | 122.403789 | 1.9382837 | 0.1031890 |
| healthyR.data |         13 | BATS                       | Test  | 1.4339890 | 100.465388 | 1.0434724 | 188.031134 | 1.8737337 | 0.0683673 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.4737481 | 102.238900 | 1.0724039 | 190.747995 | 1.9134364 | 0.0480838 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7001982 | 110.636230 | 0.7257729 |  84.665134 | 0.9062020 | 0.5664553 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.1199138 |   9.428824 | 0.1242936 |   9.238137 | 0.2567423 | 0.9623186 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.2792335 | 149.420203 | 1.3259574 | 162.711619 | 1.6062947 | 0.0208439 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.1218009 |  10.515341 | 0.1262497 |   9.974799 | 0.2547260 | 0.9623542 |
| healthyR      |          7 | EARTH                      | Test  | 0.0785988 |   3.799293 | 0.0814696 |   3.600843 | 0.1687224 | 0.9912679 |
| healthyR      |          8 | NNAR                       | Test  | 0.1452353 |   7.145891 | 0.1505400 |   6.607546 | 0.3264816 | 0.9813362 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.1166497 |   8.077546 | 0.1209103 |   8.142820 | 0.2530353 | 0.9643005 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3873958 |  41.795545 | 0.4015454 |  42.237287 | 0.5622882 | 0.9696649 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7135013 | 152.652719 | 0.7395619 |  77.332962 | 0.9459107 | 0.7288897 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3515317 | 240.108293 | 1.4008962 | 145.008463 | 1.5680851 | 0.2741191 |
| healthyR      |         13 | TBATS                      | Test  | 1.2921280 | 153.895327 | 1.3393229 | 164.727868 | 1.6177633 | 0.0491996 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.2818622 | 138.992833 | 1.3286821 | 174.558450 | 1.5980499 | 0.0366886 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1193745 |  80.265748 | 0.8144946 | 122.859592 | 1.3709328 | 0.5402935 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1621945 |   9.356829 | 0.1180182 |   9.481066 | 0.3469033 | 0.9491031 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.6256186 | 120.665329 | 1.1828549 | 166.357316 | 1.8947758 | 0.2672207 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1621677 |   9.381596 | 0.1179987 |   9.487907 | 0.3452956 | 0.9491727 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1522860 |   7.106856 | 0.1108084 |   6.742954 | 0.2465743 | 0.9751982 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1940618 |   8.773562 | 0.1412059 |   8.066041 | 0.3456125 | 0.9603648 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1586276 |   9.387301 | 0.1154228 |   9.481858 | 0.3432115 | 0.9510497 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6784332 |  43.283523 | 0.4936509 |  56.094800 | 0.9063944 | 0.9015781 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9337845 | 125.206610 | 0.6794531 |  71.174374 | 1.1588025 | 0.6954258 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6660106 | 175.934004 | 1.2122454 | 130.828457 | 1.9974802 | 0.2148655 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.5909102 | 117.081130 | 1.1575999 | 166.042257 | 1.8483277 | 0.2603729 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.6907419 | 114.752067 | 1.2302408 | 174.333511 | 1.9902669 | 0.0011201 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8543197 |  85.658737 | 0.8233626 | 120.118824 | 1.0812545 | 0.6426308 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1171078 |  12.989834 | 0.1128643 |  13.352947 | 0.2150544 | 0.9735019 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.3346299 | 174.647333 | 1.2862684 | 169.469634 | 1.6052829 | 0.3030251 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1167006 |  14.431982 | 0.1124718 |  15.875652 | 0.2153675 | 0.9734866 |
| healthyverse  |          7 | EARTH                      | Test  | 0.1165250 |   9.636884 | 0.1123026 |   9.247997 | 0.1944568 | 0.9896263 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0753538 |   5.211056 | 0.0726233 |   4.787033 | 0.1717691 | 0.9885250 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1166762 |  17.018220 | 0.1124483 |  18.548408 | 0.2086772 | 0.9747542 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6075486 |  95.480928 | 0.5855336 |  89.670480 | 0.7615957 | 0.9762470 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6639636 | 215.510361 | 0.6399043 |  60.583302 | 0.9415440 | 0.7334406 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3525915 | 216.022812 | 1.3035792 | 135.364724 | 1.6619965 | 0.2198094 |
| healthyverse  |         13 | TBATS                      | Test  | 1.3335957 | 179.783468 | 1.2852717 | 166.944320 | 1.5955256 | 0.3089593 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.4145659 | 199.320793 | 1.3633078 | 172.079913 | 1.7041261 | 0.0367533 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.9178518 |  78.502400 | 0.8526269 | 122.048230 | 1.2071659 | 0.3647400 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1143384 |   9.263933 | 0.1062132 |   9.097030 | 0.2142449 | 0.9745621 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.2955400 | 123.470880 | 1.2034756 | 177.321331 | 1.5785700 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1248633 |  11.822109 | 0.1159902 |  11.220043 | 0.2119785 | 0.9746290 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.1018121 |   5.043224 | 0.0945770 |   5.362029 | 0.2540882 | 0.9737095 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3684035 |  29.060007 | 0.3422238 |  35.544181 | 0.5794938 | 0.9045849 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1292874 |  13.112218 | 0.1200999 |  12.145836 | 0.2103227 | 0.9747335 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5445814 |  37.729140 | 0.5058820 |  49.301148 | 0.8477080 | 0.8090453 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8097045 | 138.912646 | 0.7521648 |  67.345378 | 1.1964185 | 0.5695713 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3809185 | 179.487716 | 1.2827868 | 145.426453 | 1.6814540 | 0.1558930 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.2633151 | 129.542037 | 1.1735407 | 174.311129 | 1.5368545 | 0.2032810 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.3150329 | 126.908029 | 1.2215833 | 175.731738 | 1.5996823 | 0.0260647 |
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
    ##   package   .model_id .model_desc    .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>         <int> <chr>          <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR~         7 EARTH          Test  0.243   8.35 0.177   8.71 0.644 0.802
    ## 2 healthyR          7 EARTH          Test  0.0786  3.80 0.0815  3.60 0.169 0.991
    ## 3 healthyR~         7 EARTH          Test  0.152   7.11 0.111   6.74 0.247 0.975
    ## 4 healthyv~         8 NNAR           Test  0.0754  5.21 0.0726  4.79 0.172 0.989
    ## 5 healthyR~         9 PROPHET W REG~ Test  0.129  13.1  0.120  12.1  0.210 0.975

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
    ## 1 healthyR.data <tibble [362 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [352 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [303 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [277 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [92 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
