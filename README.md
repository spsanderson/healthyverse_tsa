Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
14 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,465
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

The last day in the data set is 2022-03-12 23:42:39, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2479.07
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31465         |
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
| r_version     |     20860 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     20860 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20860 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2616 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-12 | 2021-09-10 |      475 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1498760.91 | 1861342.61 | 357 | 16906 | 271482 | 3246661 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8311.76 |   16030.65 |   1 |   322 |   2660 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-12 23:42:39 | 2021-09-10 08:03:49 |    18337 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 10H 59M 39S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [416|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [357|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [333|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [147|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [23 x 6]> <tibble>     <split [0|23]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.0323597 |  255.657484 | 0.9267168 | 149.158254 | 1.2441818 | 0.0356949 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0619159 |   14.767128 | 0.0555799 |  14.437444 | 0.0745934 | 0.9932578 |
| healthyR.data |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7051658 |  133.087446 | 0.6330051 | 134.665950 | 0.8597024 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0495378 |   16.910790 | 0.0444685 |  13.766619 | 0.0607343 | 0.9960013 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2377708 |  117.746833 | 0.2134394 |  45.564511 | 0.3500810 | 0.9031695 |
| healthyR.data |         8 | NNAR                       | Test  | 0.1626881 |   35.453139 | 0.1460400 |  42.604502 | 0.2088540 | 0.9400206 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0510790 |   12.136159 | 0.0458520 |  12.044831 | 0.0625751 | 0.9953074 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3337486 |   79.296319 | 0.2995956 |  69.383766 | 0.4084979 | 0.9932479 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8484091 |  403.381928 | 0.7615901 | 108.720588 | 1.0046864 | 0.4067889 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1344979 |  444.159504 | 1.0184030 | 139.354269 | 1.3800273 | 0.0015432 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7570167 |  136.730327 | 0.6795500 | 162.966890 | 0.8839457 | 0.0004019 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7030993 |  134.560304 | 0.6311500 | 133.049716 | 0.8585511 | 0.0000848 |
| healthyR.data |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.8390089 |  334.855091 | 0.9928090 | 157.294942 | 0.9756430 | 0.1426181 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0424793 |   16.025792 | 0.0502663 |  18.932559 | 0.0513800 | 0.9983964 |
| healthyR      |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7224943 |  257.988983 | 0.8549359 | 168.322356 | 0.8603932 | 0.0761426 |
| healthyR      |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0380349 |   11.444847 | 0.0450071 |  13.484036 | 0.0464416 | 0.9984008 |
| healthyR      |         7 | EARTH                      | Test  | 0.0168262 |    5.903355 | 0.0199107 |   5.993301 | 0.0200257 | 0.9995269 |
| healthyR      |         8 | NNAR                       | Test  | 0.0200020 |   13.909314 | 0.0236686 |  20.025167 | 0.0239597 | 0.9990730 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0436436 |   16.420901 | 0.0516440 |  19.728523 | 0.0523708 | 0.9972241 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3620107 |  177.115349 | 0.4283714 | 108.618985 | 0.4129250 | 0.9989312 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1477757 |  812.545233 | 1.3581763 | 135.290497 | 1.3431420 | 0.3935509 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1220089 |  736.323795 | 1.3276861 | 131.326984 | 1.4767039 | 0.0719994 |
| healthyR      |        13 | TBATS                      | Test  | 0.7004688 |  236.871915 | 0.8288729 | 165.883871 | 0.8385720 | 0.0214076 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6532666 |  150.521617 | 0.7730179 | 169.161079 | 0.8277647 | 0.0685650 |
| healthyR      |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7430819 |  268.158900 | 0.8029593 | 157.827078 | 0.8911989 | 0.3412485 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0481680 |   24.501877 | 0.0520494 |  18.202061 | 0.0587802 | 0.9970965 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7147969 |  381.753359 | 0.7723951 | 129.237386 | 0.8929621 | 0.0003039 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0472360 |   25.091009 | 0.0510422 |  18.132938 | 0.0580656 | 0.9971848 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0153512 |    3.806757 | 0.0165882 |   3.682530 | 0.0266854 | 0.9990272 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0754233 |   22.189515 | 0.0815009 |  20.147631 | 0.1021345 | 0.9914213 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0505949 |   12.859148 | 0.0546718 |  12.536093 | 0.0651680 | 0.9955685 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4336139 |  230.115321 | 0.4685544 | 116.898374 | 0.4853421 | 0.9980596 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0365549 |  896.728532 | 1.1200804 | 117.294827 | 1.3207229 | 0.1829728 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3032054 | 1015.920091 | 1.4082174 | 137.476231 | 1.6261930 | 0.0007866 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7234938 |  295.602496 | 0.7817928 | 153.281663 | 0.9077236 | 0.0049540 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6301916 |  358.606405 | 0.6809724 | 115.822419 | 0.8154792 | 0.0553787 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5542406 |  115.850804 | 0.6171740 | 131.769370 | 0.7079976 | 0.3412923 |
| healthyverse  |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.7777931 |  299.045348 | 0.8661107 | 115.391774 | 1.0281539 | 0.0134589 |
| healthyverse  |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0549236 |   20.540347 | 0.0611601 |  16.313968 | 0.0637737 | 0.9992341 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0167962 |    2.762163 | 0.0187034 |   2.794931 | 0.0287461 | 0.9988486 |
| healthyverse  |         8 | NNAR                       | Test  | 0.1007629 |   14.317224 | 0.1122044 |  12.583170 | 0.1866518 | 0.9871956 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0607117 |   17.778258 | 0.0676054 |  17.359115 | 0.0709183 | 0.9985102 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2486053 |   50.356030 | 0.2768341 |  63.216409 | 0.2982646 | 0.9949037 |
| healthyverse  |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 0.9902403 |  277.937093 | 1.1026810 | 134.036597 | 1.2987930 | 0.0075433 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6322283 |  109.999015 | 0.7040171 | 130.548561 | 0.8223789 | 0.0199505 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.8535821 |  371.741148 | 0.9505054 | 122.418366 | 1.0502750 | 0.0275722 |
| healthyverse  |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6955457 |  337.485021 | 0.8977647 | 143.702193 | 0.8350980 | 0.2710198 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0476861 |   17.971457 | 0.0615501 |  15.512190 | 0.0546037 | 0.9990730 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6000720 |  296.100312 | 0.7745336 | 138.726774 | 0.7670230 | 0.0070282 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0473572 |   29.137629 | 0.0611256 |  18.325399 | 0.0590027 | 0.9988533 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0108998 |    3.081083 | 0.0140687 |   3.016187 | 0.0123170 | 0.9996782 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0581915 |   51.611149 | 0.0751098 |  26.482638 | 0.0799674 | 0.9897978 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0512093 |   25.025320 | 0.0660976 |  19.193748 | 0.0622165 | 0.9978379 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3415838 |  203.877167 | 0.4408939 |  97.406149 | 0.3862388 | 0.9982865 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9201732 |  861.881500 | 1.1876992 | 104.256777 | 1.1997219 | 0.2597888 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0792940 |  878.200891 | 1.3930818 | 140.065946 | 1.3342598 | 0.0199601 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.5755889 |  259.822418 | 0.7429324 | 143.412440 | 0.7486712 | 0.0022103 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5558916 |   95.162159 | 0.7175084 | 184.555231 | 0.6836724 | 0.0063852 |
| healthyR.ai   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         1 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         6 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         7 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         8 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         9 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        10 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        12 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        13 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        14 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |

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
    ## 1 health~         6 LM          Test   0.0495 16.9   0.0445 13.8   0.0607  0.996
    ## 2 health~         7 EARTH       Test   0.0168  5.90  0.0199  5.99  0.0200  1.00 
    ## 3 health~         7 EARTH       Test   0.0154  3.81  0.0166  3.68  0.0267  0.999
    ## 4 health~         7 EARTH       Test   0.0168  2.76  0.0187  2.79  0.0287  0.999
    ## 5 health~         7 EARTH       Test   0.0109  3.08  0.0141  3.02  0.0123  1.00 
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [416|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [357|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [333|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [147|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [23 x 6]> <tibble>     <split [0|23]>   <mdl_time_tbl>

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
