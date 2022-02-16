Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
16 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,135
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

The last day in the data set is 2022-02-14 13:58:15, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1845.33
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29135          |
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
| r\_version     |      19364 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19364 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19364 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2418 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-14 | 2021-08-25 |       449 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1519230.62 | 1871973.42 | 357 | 23249 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8235.09 |   15717.73 |   1 |   339 |   2770 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-14 13:58:15 | 2021-08-25 19:08:01 |     17010 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 57M 13S |        60 |

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [391|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [382|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [331|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [307|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [121|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0016664 |  399.633924 | 0.7745913 | 133.756295 | 1.2892084 | 0.0110948 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0619210 |   45.951931 | 0.0478837 |  17.974702 | 0.0830886 | 0.9936664 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8913597 |  118.941871 | 0.6892908 | 182.517763 | 1.0472855 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0679057 |   41.958955 | 0.0525116 |  19.181603 | 0.0967610 | 0.9925068 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0675465 |   39.747027 | 0.0522339 |  19.587628 | 0.1404935 | 0.9811100 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0112201 |    1.891049 | 0.0086765 |   1.920602 | 0.0206440 | 0.9996068 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0743176 |   38.924145 | 0.0574700 |  20.240188 | 0.1064567 | 0.9907183 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4868097 |  288.746768 | 0.3764513 |  79.857570 | 0.6126035 | 0.9910068 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2736006 | 1263.685568 | 0.9848787 | 127.880933 | 1.5430097 | 0.2785135 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5632250 | 1281.573612 | 1.2088460 | 144.925816 | 1.8470097 | 0.0011431 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8918486 |  207.197685 | 0.6896688 | 132.382836 | 1.1717853 | 0.0435929 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8911686 |  117.011753 | 0.6891430 | 183.275737 | 1.0464161 | 0.0306454 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8368018 |  543.578871 | 0.9166733 | 151.058031 | 1.0288981 | 0.4140015 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0613220 |   34.070592 | 0.0671751 |  18.838711 | 0.0738839 | 0.9949249 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6587097 |  281.746626 | 0.7215826 | 124.820041 | 0.8513083 | 0.3017548 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0577782 |   29.038523 | 0.0632930 |  18.004607 | 0.0703166 | 0.9949337 |
| healthyR      |          7 | EARTH                      | Test  | 0.0329711 |    9.395081 | 0.0361181 |  13.683547 | 0.0740338 | 0.9940245 |
| healthyR      |          8 | NNAR                       | Test  | 0.0090974 |    2.559804 | 0.0099658 |   2.253774 | 0.0188720 | 0.9996578 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0642477 |   49.125761 | 0.0703800 |  22.007126 | 0.0781106 | 0.9937430 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4841680 |  419.641641 | 0.5303811 | 104.151096 | 0.5902766 | 0.9725749 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9996566 |  318.517904 | 1.0950724 |  99.419873 | 1.3127168 | 0.5705246 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0466465 |  410.018658 | 1.1465473 | 123.513918 | 1.3250521 | 0.1586931 |
| healthyR      |         13 | TBATS                      | Test  | 0.6250853 |  141.070128 | 0.6847487 | 110.650810 | 0.8494209 | 0.2794777 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7572199 |  338.316628 | 0.8294955 | 143.252533 | 0.9528613 | 0.0292871 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1587451 | 1037.164902 | 1.2722788 | 153.823140 | 1.3998663 | 0.1752412 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0746747 |   80.104974 | 0.0819914 |  20.355492 | 0.0929647 | 0.9909558 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9854665 |  790.964741 | 1.0820223 | 158.854645 | 1.2260279 | 0.1538144 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0723285 |   73.090335 | 0.0794152 |  22.414161 | 0.0876569 | 0.9912188 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0152720 |    4.367280 | 0.0167683 |   4.082191 | 0.0216571 | 0.9995203 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0136764 |    5.726089 | 0.0150164 |   7.650981 | 0.0295798 | 0.9994933 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0806895 |   80.363741 | 0.0885955 |  24.620041 | 0.0992882 | 0.9892150 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7586605 |  838.103988 | 0.8329939 | 139.582966 | 0.8604510 | 0.9764907 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0794049 |  552.755385 | 1.1851648 |  98.636416 | 1.4781462 | 0.5333220 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2131327 | 1184.698118 | 1.3319952 | 122.538103 | 1.6688558 | 0.1748310 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9281315 |  576.461870 | 1.0190696 | 148.182875 | 1.1697693 | 0.1125048 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0132658 |  722.281888 | 1.1125454 | 152.600372 | 1.2939545 | 0.0921656 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1510909 |  131.092994 | 1.1185725 | 158.095405 | 1.3789553 | 0.1132095 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0630786 |    7.845319 | 0.0612967 |   7.514404 | 0.0880417 | 0.9907464 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7976120 |  151.468895 | 0.7750794 | 112.342250 | 0.9346989 | 0.0427013 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0604747 |    7.537004 | 0.0587662 |   7.126780 | 0.0897356 | 0.9907464 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0208895 |    1.973551 | 0.0202993 |   1.984229 | 0.0433970 | 0.9986383 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0143742 |    1.702427 | 0.0139681 |   1.878232 | 0.0340441 | 0.9991528 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0718434 |   11.538333 | 0.0698138 |  10.575725 | 0.0930860 | 0.9905785 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6068794 |   79.416352 | 0.5897350 |  93.056603 | 0.7259313 | 0.9651102 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9498393 |  181.379918 | 0.9230063 |  92.341291 | 1.1639707 | 0.3731003 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2438248 |  245.593266 | 1.2086866 | 130.195650 | 1.5310804 | 0.0231232 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0555283 |  130.496517 | 1.0257096 | 146.813701 | 1.2491003 | 0.0040031 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7459618 |  147.033841 | 0.7248883 |  92.879487 | 0.9329810 | 0.0383279 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0286695 | 1025.859821 | 1.0446330 | 158.826946 | 1.2069360 | 0.4452212 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0752154 |   68.951987 | 0.0763827 |  27.335314 | 0.0897041 | 0.9924295 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8384872 |  556.491734 | 0.8514993 | 151.717649 | 1.0210393 | 0.1662301 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0665219 |   41.374728 | 0.0675542 |  27.144336 | 0.0841546 | 0.9924092 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0163581 |    3.583464 | 0.0166120 |   3.455442 | 0.0272910 | 0.9991437 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0271498 |   15.938461 | 0.0275711 |  12.217304 | 0.0528985 | 0.9972218 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0749310 |   85.967508 | 0.0760938 |  29.308430 | 0.0930710 | 0.9906907 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8372017 | 1165.395123 | 0.8501938 | 155.850897 | 0.9179769 | 0.9409501 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1150248 |  578.767531 | 1.1323284 | 127.858286 | 1.4212487 | 0.4838208 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1387959 |  585.272614 | 1.1564683 | 135.811259 | 1.5779765 | 0.1513617 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8109363 |  586.996819 | 0.8235208 | 147.340912 | 0.9977804 | 0.1959657 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8968720 |  519.039375 | 0.9107901 | 158.085710 | 1.1196181 | 0.0315081 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0112   1.89 0.00868  1.92 0.0206 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00910  2.56 0.00997  2.25 0.0189 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0153   4.37 0.0168   4.08 0.0217 1.00 
    ## 4 healthyv~         8 NNAR        Test  0.0144   1.70 0.0140   1.88 0.0340 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0164   3.58 0.0166   3.46 0.0273 0.999

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [391|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [382|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [331|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [307|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [121|28]> <mdl_time_tbl>

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
