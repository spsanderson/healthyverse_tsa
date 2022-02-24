Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
24 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,559
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

The last day in the data set is 2022-02-22 23:56:27, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2047.3
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29559          |
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
| r\_version     |      19637 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19637 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19637 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2482 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-22 | 2021-08-28 |       457 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1514885.22 | 1870357.49 | 357 | 20774 | 271098 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8285.79 |   15856.59 |   1 |   286 |   2742 |    8293 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-22 23:56:27 | 2021-08-28 14:07:16 |     17240 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |     median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-----------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 59M 0S |        60 |

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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data     .future_data .splits          .modeltime_tables
    ##   <chr>         <list>           <list>       <list>           <list>           
    ## 1 healthyR.data <tibble>         <tibble>     <split [398|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [390|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [339|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [315|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [129|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [5 x 6]> <tibble>     <split [0|5]>    <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9727944 | 230.957441 | 0.8634824 | 137.438105 | 1.2155310 | 0.5905056 |
| healthyR.data |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8665626 | 111.891164 | 0.7691878 | 170.331395 | 1.0013878 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0665979 |  19.389352 | 0.0591143 |  20.327054 | 0.0970551 | 0.9932527 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0844709 |  28.573953 | 0.0749790 |  22.931520 | 0.1468034 | 0.9817282 |
| healthyR.data |          8 | NNAR                       | Test  | 0.8104350 | 138.523967 | 0.7193672 |  78.482445 | 1.1878346 | 0.6707288 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0718807 |  19.357817 | 0.0638035 |  18.365559 | 0.1073905 | 0.9917324 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4532192 | 100.601769 | 0.4022914 |  81.165061 | 0.5713470 | 0.9916176 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1393782 | 398.578003 | 1.0113474 | 120.976469 | 1.3126951 | 0.3629974 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3752070 | 435.994108 | 1.2206763 | 142.332291 | 1.5935673 | 0.0090157 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9050783 | 120.704107 | 0.8033755 | 143.841533 | 1.1386588 | 0.2124947 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8655717 | 112.408780 | 0.7683082 | 169.148159 | 1.0000536 | 0.0115352 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7234366 | 148.446668 | 0.7863317 | 138.192785 | 0.8876951 | 0.5084793 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0574250 |   9.073325 | 0.0624175 |   8.500058 | 0.0698664 | 0.9953646 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6151012 | 127.680707 | 0.6685777 | 104.240911 | 0.8597598 | 0.2129013 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0557986 |   9.985349 | 0.0606496 |   9.293124 | 0.0677761 | 0.9953715 |
| healthyR      |          7 | EARTH                      | Test  | 0.0326383 |   4.201780 | 0.0354758 |   4.200558 | 0.0726722 | 0.9942418 |
| healthyR      |          8 | NNAR                       | Test  | 0.9722057 | 219.543010 | 1.0567286 |  80.266452 | 1.2669069 | 0.8268152 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0612894 |  12.393655 | 0.0666179 |  15.108725 | 0.0747769 | 0.9945049 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4379495 | 109.626668 | 0.4760246 |  92.113732 | 0.5440186 | 0.9772524 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8772332 | 307.824861 | 0.9534993 |  87.362508 | 1.1699561 | 0.5433865 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0353308 | 375.844791 | 1.1253418 | 122.027518 | 1.2889986 | 0.1296749 |
| healthyR      |         13 | TBATS                      | Test  | 0.6471667 | 102.824095 | 0.7034309 | 106.302716 | 0.8856660 | 0.1960112 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7633001 | 167.745340 | 0.8296610 | 129.184461 | 0.9588372 | 0.0531641 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0976153 | 404.043848 | 1.2648207 | 166.438361 | 1.2848233 | 0.5730745 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0764642 |  28.005388 | 0.0881124 |  20.764298 | 0.0928644 | 0.9907477 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8256526 | 168.688604 | 0.9514286 | 150.080461 | 1.0401490 | 0.1298136 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0686098 |  26.497817 | 0.0790614 |  21.378815 | 0.0841000 | 0.9911112 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0140701 |   3.237338 | 0.0162135 |   3.141437 | 0.0206622 | 0.9995045 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0162621 |   4.622008 | 0.0187393 |   4.617241 | 0.0315486 | 0.9990307 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0799496 |  28.600220 | 0.0921288 |  23.413281 | 0.0962261 | 0.9892436 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7181831 | 334.187454 | 0.8275876 | 142.235996 | 0.8080508 | 0.9764604 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9566699 | 295.141322 | 1.1024044 |  93.491115 | 1.3477779 | 0.4885234 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0784724 | 330.320361 | 1.2427617 | 132.036886 | 1.5015042 | 0.1312970 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9183002 | 230.902502 | 1.0581896 | 163.250463 | 1.1224279 | 0.1318868 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8313626 | 133.408204 | 0.9580084 | 173.842499 | 1.0631239 | 0.0284038 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.0144559 | 130.974184 | 0.9687920 | 169.311551 | 1.1936390 | 0.7090114 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0658806 |   8.347889 | 0.0629151 |   8.349709 | 0.0890054 | 0.9923478 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8552549 | 136.451360 | 0.8167572 | 116.083800 | 1.0329493 | 0.0253997 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0707421 |   9.590797 | 0.0675578 |  10.060767 | 0.0946685 | 0.9923060 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0246752 |   2.207932 | 0.0235645 |   2.198735 | 0.0472335 | 0.9982698 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0190210 |   1.374367 | 0.0181648 |   1.353699 | 0.0414687 | 0.9986014 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0654841 |   8.315753 | 0.0625364 |   8.147299 | 0.0895836 | 0.9921571 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5463795 |  73.516263 | 0.5217852 |  96.002525 | 0.6715577 | 0.9746682 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8421270 | 219.851058 | 0.8042202 |  89.183752 | 1.0352122 | 0.4930182 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1912964 | 264.281392 | 1.1376724 | 140.097956 | 1.4189943 | 0.0159317 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9161711 | 104.708676 | 0.8749313 | 142.280695 | 1.1492552 | 0.0380975 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8308208 | 126.963336 | 0.7934229 | 102.984909 | 1.0352815 | 0.2644140 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7725280 | 293.749097 | 0.8932015 | 150.746614 | 0.9531805 | 0.4421978 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0682019 |  18.619327 | 0.0788554 |  22.514741 | 0.0814961 | 0.9935300 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.6214028 | 129.558564 | 0.7184696 | 130.142465 | 0.8609528 | 0.1208299 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0701634 |  19.114181 | 0.0811234 |  20.521040 | 0.0906009 | 0.9930406 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0166593 |   2.512698 | 0.0192616 |   2.482299 | 0.0288538 | 0.9990393 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0266349 |   6.370178 | 0.0307954 |   6.985189 | 0.0499439 | 0.9976572 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0705536 |  21.373812 | 0.0815745 |  27.488484 | 0.0921656 | 0.9916541 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4633932 | 217.969463 | 0.5357780 | 114.374887 | 0.5570438 | 0.9746520 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8974215 | 338.767117 | 1.0376040 | 107.252650 | 1.2345158 | 0.4882713 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0496451 | 439.067956 | 1.2136060 | 135.439115 | 1.3231452 | 0.1224419 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.6630875 | 245.521613 | 0.7666657 | 136.363270 | 0.8723316 | 0.1183161 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.6936468 | 109.389317 | 0.8019985 | 165.134269 | 0.9142872 | 0.1314656 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          1 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          4 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          6 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          7 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          8 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |          9 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         10 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         12 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         13 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         14 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ## 1 health~         6 LM          Test   0.0666 19.4   0.0591 20.3   0.0971  0.993
    ## 2 health~         6 LM          Test   0.0558  9.99  0.0606  9.29  0.0678  0.995
    ## 3 health~         7 EARTH       Test   0.0141  3.24  0.0162  3.14  0.0207  1.00 
    ## 4 health~         8 NNAR        Test   0.0190  1.37  0.0182  1.35  0.0415  0.999
    ## 5 health~         7 EARTH       Test   0.0167  2.51  0.0193  2.48  0.0289  0.999
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
    ##   package       .actual_data     .future_data .splits          .modeltime_tables
    ##   <chr>         <list>           <list>       <list>           <list>           
    ## 1 healthyR.data <tibble>         <tibble>     <split [398|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [390|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [339|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [315|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [129|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [5 x 6]> <tibble>     <split [0|5]>    <mdl_time_tbl>

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
