Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
27 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,821
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

The last day in the data set is 2022-02-25 22:11:30, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2117.55
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29821          |
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
| r\_version     |      19812 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19812 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19812 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        17 |          0 |
| country        |       2495 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-25 | 2021-08-30 |       460 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1511990.16 | 1869778.35 | 357 | 17597 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8253.47 |   15832.45 |   1 |   258 |   2735 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-25 22:11:30 | 2021-08-30 14:42:50 |     17367 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |    median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|----------:|----------:|
| time           |          0 |              1 |   0 |  59 | 11H 0M 0S |        60 |

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
    ## 1 healthyR.data <tibble>         <tibble>     <split [401|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [393|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [342|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [318|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [132|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [8 x 6]> <tibble>     <split [0|8]>    <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9742686 | 232.009598 | 0.8266340 | 140.268499 | 1.1885592 | 0.6713713 |
| healthyR.data |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8687537 | 117.318384 | 0.7371082 | 157.969455 | 1.0124346 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0687451 |  19.853431 | 0.0583279 |  20.747854 | 0.0974509 | 0.9926765 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0880402 |  29.251018 | 0.0746991 |  23.102461 | 0.1512862 | 0.9821360 |
| healthyR.data |          8 | NNAR                       | Test  | 0.6802235 |  98.838694 | 0.5771467 |  72.432587 | 1.0349363 | 0.7362790 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0698773 |  18.326750 | 0.0592885 |  16.917389 | 0.1061912 | 0.9916291 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4305598 |  88.464494 | 0.3653154 |  78.610568 | 0.5404497 | 0.9928211 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9040890 | 334.530397 | 0.7670890 |  99.746418 | 1.0749395 | 0.5181334 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1740057 | 389.830794 | 0.9961043 | 136.318932 | 1.3472089 | 0.0360832 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8470819 | 119.483739 | 0.7187204 | 164.583933 | 1.0036495 | 0.1054103 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8674836 | 117.749703 | 0.7360306 | 156.721378 | 1.0116863 | 0.0479940 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7598139 | 179.026772 | 0.8275747 | 148.720316 | 0.9202338 | 0.3933549 |
| healthyR      |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6376934 | 207.533656 | 0.6945634 | 107.587831 | 0.8822941 | 0.1233555 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0552326 |  10.049529 | 0.0601583 |   9.550336 | 0.0680316 | 0.9951639 |
| healthyR      |          7 | EARTH                      | Test  | 0.0326618 |   4.796253 | 0.0355746 |   4.814149 | 0.0707014 | 0.9942967 |
| healthyR      |          8 | NNAR                       | Test  | 1.0458444 | 417.315238 | 1.1391138 | 103.709708 | 1.2699797 | 0.8571463 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0760524 |  27.222845 | 0.0828348 |  25.197726 | 0.0892947 | 0.9944280 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4320435 | 132.139946 | 0.4705735 |  99.263681 | 0.5322467 | 0.9761131 |
| healthyR      |         11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0637429 | 475.208312 | 1.1586084 | 121.347500 | 1.3466163 | 0.0693467 |
| healthyR      |         13 | TBATS                      | Test  | 0.6373206 | 153.353714 | 0.6941574 | 110.846216 | 0.8831771 | 0.1439631 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7195745 | 183.540941 | 0.7837468 | 124.674976 | 0.9336280 | 0.0576730 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2162793 | 380.547858 | 1.2977840 | 168.852242 | 1.4004276 | 0.7971268 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0799379 |  17.040368 | 0.0852947 |  15.741286 | 0.0945342 | 0.9902809 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7508365 | 131.542279 | 0.8011512 | 140.104528 | 0.9551902 | 0.0783786 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0705842 |  18.682726 | 0.0753141 |  15.396123 | 0.0864007 | 0.9906101 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0145127 |   2.787728 | 0.0154852 |   2.729203 | 0.0207136 | 0.9994923 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0103535 |   2.595524 | 0.0110473 |   2.717268 | 0.0198419 | 0.9996312 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0836827 |  18.296631 | 0.0892904 |  18.996518 | 0.0991036 | 0.9884776 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6761410 | 233.676038 | 0.7214502 | 137.087461 | 0.7694882 | 0.9799507 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8915057 | 248.749157 | 0.9512468 |  89.538133 | 1.2989932 | 0.4428400 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0542505 | 289.710868 | 1.1248974 | 128.802905 | 1.4540043 | 0.0751087 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9135912 | 182.002290 | 0.9748123 | 158.399890 | 1.1244151 | 0.0844678 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8212512 | 102.328328 | 0.8762845 | 180.418260 | 1.0522130 | 0.0544882 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9369558 | 145.095814 | 0.9833459 | 165.272724 | 1.1424218 | 0.7803306 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0684970 |  18.156557 | 0.0718884 |  12.814817 | 0.0930054 | 0.9923397 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8419849 | 204.909526 | 0.8836729 | 117.398547 | 1.0211439 | 0.0388211 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0759114 |  22.281497 | 0.0796699 |  15.378284 | 0.1001458 | 0.9923308 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0251194 |   2.918205 | 0.0263631 |   2.944059 | 0.0494506 | 0.9980764 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0249596 |   2.403163 | 0.0261954 |   2.379585 | 0.0534622 | 0.9976494 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0732400 |  16.358007 | 0.0768663 |  13.307918 | 0.0983686 | 0.9917050 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5238195 |  95.987794 | 0.5497547 | 101.926030 | 0.6591625 | 0.9759931 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.6946559 | 200.146600 | 0.7290494 |  91.746952 | 0.8221717 | 0.6474064 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0590603 | 281.823605 | 1.1114960 | 137.646821 | 1.2999989 | 0.0364043 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8838658 | 110.105954 | 0.9276274 | 141.315452 | 1.1487079 | 0.0451867 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8445386 | 196.358022 | 0.8863530 | 111.522526 | 1.0448016 | 0.5292546 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8116097 | 244.664917 | 0.9102112 | 156.226038 | 0.9806711 | 0.4911532 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0725158 |  18.615311 | 0.0813257 |  20.477626 | 0.0836622 | 0.9934437 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.6401539 | 138.575588 | 0.7179255 | 132.403726 | 0.8715137 | 0.1042857 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0703471 |  24.844735 | 0.0788934 |  20.808046 | 0.0900781 | 0.9930482 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0172028 |   2.462012 | 0.0192928 |   2.432965 | 0.0291133 | 0.9990314 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0285187 |   5.664766 | 0.0319834 |   5.594410 | 0.0558619 | 0.9966609 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0728948 |  24.115680 | 0.0817507 |  24.878005 | 0.0920898 | 0.9916204 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8288164 | 433.787398 | 0.9295083 | 151.692686 | 0.9071900 | 0.9395637 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8591745 | 238.913666 | 0.9635546 | 102.284981 | 1.1596791 | 0.5112879 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9752650 | 263.209629 | 1.0937488 | 130.324032 | 1.2509236 | 0.1271143 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.6544239 | 132.540096 | 0.7339291 | 127.528053 | 0.8679971 | 0.1209140 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.7241058 |  99.741551 | 0.8120766 | 182.614101 | 0.9301483 | 0.1508866 |
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
    ## 1 health~         6 LM          Test   0.0687 19.9   0.0583 20.7   0.0975  0.993
    ## 2 health~         6 LM          Test   0.0552 10.0   0.0602  9.55  0.0680  0.995
    ## 3 health~         8 NNAR        Test   0.0104  2.60  0.0110  2.72  0.0198  1.00 
    ## 4 health~         7 EARTH       Test   0.0251  2.92  0.0264  2.94  0.0495  0.998
    ## 5 health~         7 EARTH       Test   0.0172  2.46  0.0193  2.43  0.0291  0.999
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
    ## 1 healthyR.data <tibble>         <tibble>     <split [401|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [393|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [342|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [318|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [132|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [8 x 6]> <tibble>     <split [0|8]>    <mdl_time_tbl>

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
