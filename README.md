Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,570
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

The last day in the data set is 2022-03-13 23:38:31, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2503 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31570         |
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
| r_version     |     20955 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     20955 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20955 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2621 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-13 | 2021-09-10 |      476 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1496559.70 | 1860560.42 | 357 | 16906 | 271098 | 3246657 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8303.91 |   16020.76 |   1 |   337 |   2659 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-13 23:38:31 | 2021-09-10 12:12:32 |    18388 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     19 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [417|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [358|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [334|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [148|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [24 x 6]> <tibble>     <split [0|24]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.1048717 |  262.885816 | 0.9382929 | 152.201392 | 1.3135939 | 0.0525746 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0663909 |   22.026221 | 0.0563813 |  17.568423 | 0.0797972 | 0.9941200 |
| healthyR.data |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7552420 |  122.886371 | 0.6413760 | 139.733768 | 0.9070564 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0473388 |   10.272296 | 0.0402016 |   9.761934 | 0.0562127 | 0.9965828 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2091925 |  102.731388 | 0.1776531 |  38.846162 | 0.3207348 | 0.9187961 |
| healthyR.data |         8 | NNAR                       | Test  | 0.2634667 |   45.481239 | 0.2237445 |  58.654631 | 0.3526915 | 0.8603158 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0532927 |   11.363507 | 0.0452579 |  13.224024 | 0.0627849 | 0.9956004 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3440987 |   82.160510 | 0.2922197 |  64.425585 | 0.4126014 | 0.9926015 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8574368 |  371.167806 | 0.7281632 | 105.086531 | 1.0268859 | 0.3637691 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1990251 |  416.462673 | 1.0182510 | 140.369820 | 1.4675734 | 0.0020064 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7961904 |  130.234965 | 0.6761507 | 163.887003 | 0.9210218 | 0.0000181 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7523467 |  124.094709 | 0.6389172 | 137.806390 | 0.9048134 | 0.0021242 |
| healthyR.data |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.8596716 |  292.644301 | 0.9135074 | 157.705475 | 1.0304633 | 0.1846007 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0474701 |   15.742558 | 0.0504429 |  18.511964 | 0.0669028 | 0.9943464 |
| healthyR      |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7350449 |  296.046138 | 0.7810761 | 150.199862 | 0.9003491 | 0.1041086 |
| healthyR      |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0435153 |   10.514214 | 0.0462404 |  11.870017 | 0.0648569 | 0.9943536 |
| healthyR      |         7 | EARTH                      | Test  | 0.0275159 |    6.335099 | 0.0292390 |   6.492994 | 0.0646107 | 0.9949364 |
| healthyR      |         8 | NNAR                       | Test  | 0.0543631 |   16.891171 | 0.0577676 |  18.074703 | 0.0810320 | 0.9934357 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0506568 |   15.942236 | 0.0538291 |  18.174758 | 0.0723485 | 0.9929563 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3700615 |  153.578899 | 0.3932361 | 102.375772 | 0.4749922 | 0.9811794 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1490507 |  882.385802 | 1.2210085 | 134.818320 | 1.3444070 | 0.4317021 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1830714 |  862.137015 | 1.2571596 | 133.585778 | 1.4809247 | 0.0851030 |
| healthyR      |        13 | TBATS                      | Test  | 0.7541733 |  226.153430 | 0.8014024 | 161.141956 | 0.9231757 | 0.0298439 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6543294 |  106.028428 | 0.6953058 | 176.487600 | 0.8953828 | 0.0003352 |
| healthyR      |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7540494 |  274.840190 | 0.8141024 | 153.462479 | 0.9059485 | 0.3702380 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0521444 |   24.327936 | 0.0562972 |  17.835780 | 0.0625778 | 0.9970252 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7223075 |  344.718278 | 0.7798326 | 134.318795 | 0.8870876 | 0.0004089 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0512878 |   25.177556 | 0.0553724 |  17.960335 | 0.0619037 | 0.9971071 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0157727 |    3.632113 | 0.0170288 |   3.532144 | 0.0272735 | 0.9990030 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0700027 |   47.228359 | 0.0755778 |  31.997255 | 0.0920475 | 0.9888829 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0554266 |   14.966492 | 0.0598408 |  16.172536 | 0.0676926 | 0.9957474 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4413625 |  237.733972 | 0.4765129 | 112.745197 | 0.4961598 | 0.9978851 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0293511 |  905.606277 | 1.1113294 | 117.707194 | 1.3174096 | 0.1857560 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2963728 |  996.406963 | 1.3996168 | 138.722329 | 1.6245075 | 0.0012956 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7463010 |  300.095886 | 0.8057370 | 158.173993 | 0.9211573 | 0.0034484 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6475737 |  325.175785 | 0.6991469 | 121.623697 | 0.8240890 | 0.0954868 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5853243 |  118.192335 | 0.5948843 | 133.715138 | 0.7859230 | 0.2805517 |
| healthyverse  |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.8602703 |  350.025999 | 0.8743209 | 118.677217 | 1.1086132 | 0.0252698 |
| healthyverse  |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0624922 |   21.674708 | 0.0635129 |  16.898345 | 0.0789599 | 0.9942686 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0192995 |    2.873334 | 0.0196148 |   2.916743 | 0.0337720 | 0.9986724 |
| healthyverse  |         8 | NNAR                       | Test  | 0.1116570 |   26.914612 | 0.1134807 |  24.452033 | 0.1671887 | 0.9881658 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0694197 |   18.415914 | 0.0705536 |  17.794823 | 0.0881863 | 0.9925775 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2614637 |   50.898480 | 0.2657342 |  51.964912 | 0.3580838 | 0.9874147 |
| healthyverse  |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0443279 |  252.401119 | 1.0613847 | 136.998223 | 1.4064396 | 0.0094306 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6889520 |  119.768499 | 0.7002045 | 126.494520 | 0.9113731 | 0.0064440 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9504546 |  438.335362 | 0.9659781 | 123.484400 | 1.1715333 | 0.0138832 |
| healthyverse  |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.7178780 |  321.792409 | 0.8309768 | 143.681633 | 0.8968859 | 0.2385130 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0457533 |   18.621264 | 0.0529616 |  14.410377 | 0.0532572 | 0.9976365 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6376602 |  300.065618 | 0.7381210 | 149.245186 | 0.8151788 | 0.0118371 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0484124 |   33.189116 | 0.0560396 |  19.492529 | 0.0618329 | 0.9974395 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0117743 |    3.160329 | 0.0136293 |   3.088638 | 0.0140644 | 0.9996987 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0369655 |   22.196245 | 0.0427893 |  24.292456 | 0.0446994 | 0.9991228 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0552664 |   29.979953 | 0.0639734 |  21.378458 | 0.0673111 | 0.9958766 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4456342 |  312.115863 | 0.5158420 | 113.919094 | 0.4963078 | 0.9891458 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9968518 |  969.176910 | 1.1539017 | 108.334391 | 1.2742573 | 0.3140471 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1154089 | 1012.084067 | 1.2911370 | 137.031130 | 1.3908913 | 0.0404609 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6504750 |  334.120772 | 0.7529547 | 144.534159 | 0.8145165 | 0.0020199 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5969747 |  140.537020 | 0.6910256 | 171.633649 | 0.7527290 | 0.0160272 |
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
    ## 1 health~         6 LM          Test   0.0473 10.3   0.0402  9.76  0.0562  0.997
    ## 2 health~         7 EARTH       Test   0.0275  6.34  0.0292  6.49  0.0646  0.995
    ## 3 health~         7 EARTH       Test   0.0158  3.63  0.0170  3.53  0.0273  0.999
    ## 4 health~         7 EARTH       Test   0.0193  2.87  0.0196  2.92  0.0338  0.999
    ## 5 health~         7 EARTH       Test   0.0118  3.16  0.0136  3.09  0.0141  1.00 
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [417|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [409|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [358|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [334|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [148|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [24 x 6]> <tibble>     <split [0|24]>   <mdl_time_tbl>

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
