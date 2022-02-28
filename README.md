Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
28 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,842
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

The last day in the data set is 2022-02-26 22:22:56, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2141.74
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 29842         |
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
| r_version     |     19815 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     19815 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     19815 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       17 |          0 |
| country       |      2500 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-02-26 | 2021-08-30 |      461 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1512709.87 | 1869957.41 | 357 | 18553.25 | 271097.0 | 3247952 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8253.81 |   15832.74 |   1 |   258.00 |   2723.5 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-02-26 22:22:56 | 2021-08-30 16:29:35 |    17386 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |    3.5 |       60 |

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
    ##   package       .actual_data     .future_data .splits          .modeltime_tables
    ##   <chr>         <list>           <list>       <list>           <list>           
    ## 1 healthyR.data <tibble>         <tibble>     <split [402|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [394|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [343|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [319|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [133|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [9 x 6]> <tibble>     <split [0|9]>    <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.0378858 | 229.602478 | 0.9221443 | 147.348576 | 1.2385890 | 0.5986892 |
| healthyR.data |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.8821440 | 104.326295 | 0.7837703 | 181.175575 | 1.0307994 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0665253 |  18.881563 | 0.0591066 |  19.597997 | 0.0966321 | 0.9923585 |
| healthyR.data |         7 | EARTH                      | Test  | 0.1036675 |  29.944091 | 0.0921069 |  24.787425 | 0.1702010 | 0.9776305 |
| healthyR.data |         8 | NNAR                       | Test  | 0.6459858 |  91.027255 | 0.5739477 |  65.119202 | 0.9790813 | 0.7626065 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0712416 |  18.177727 | 0.0632970 |  17.053285 | 0.1053841 | 0.9912596 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4489316 |  86.411159 | 0.3988683 |  83.347866 | 0.5508156 | 0.9924093 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8483539 | 305.586628 | 0.7537483 |  90.832145 | 1.0275341 | 0.5506775 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1115574 | 338.025453 | 0.9876003 | 131.871208 | 1.3042127 | 0.0540981 |
| healthyR.data |        13 | TBATS                      | Test  | 0.8827858 | 122.990940 | 0.7843405 | 164.988694 | 1.0350762 | 0.0382307 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.8806246 | 104.561269 | 0.7824203 | 179.387923 | 1.0294568 | 0.0757614 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7624326 | 171.520096 | 0.7987246 | 148.120203 | 0.9218522 | 0.4342355 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0553986 |  10.153322 | 0.0580356 |   9.900125 | 0.0681802 | 0.9955451 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6673620 | 192.295533 | 0.6991286 | 110.704528 | 0.8954446 | 0.1421903 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0534604 |  22.457483 | 0.0560051 |  17.092035 | 0.0777535 | 0.9955571 |
| healthyR      |         7 | EARTH                      | Test  | 0.0328053 |   4.514991 | 0.0343668 |   4.505126 | 0.0700904 | 0.9947988 |
| healthyR      |         8 | NNAR                       | Test  | 0.5189680 | 148.084700 | 0.5436710 |  69.308984 | 0.6060016 | 0.9410379 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0552507 |  18.141750 | 0.0578806 |  18.488571 | 0.0790529 | 0.9948463 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4307960 | 118.192210 | 0.4513021 |  95.656009 | 0.5355213 | 0.9737184 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8360766 | 328.854195 | 0.8758741 |  83.053665 | 1.1670222 | 0.5567915 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0135325 | 439.771258 | 1.0617769 | 120.013928 | 1.3139444 | 0.0867237 |
| healthyR      |        13 | TBATS                      | Test  | 0.6702200 | 156.259256 | 0.7021226 | 110.186575 | 0.9080455 | 0.1668901 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.7502106 | 188.308703 | 0.7859208 | 122.954105 | 0.9647196 | 0.0129272 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1035868 | 345.772607 | 1.1850849 | 168.419688 | 1.2936344 | 0.6105180 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0784639 |  17.651887 | 0.0842583 |  17.287542 | 0.0934266 | 0.9904264 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7675651 | 141.045941 | 0.8242486 | 146.720167 | 0.9746363 | 0.0821264 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0700110 |  19.028431 | 0.0751812 |  16.044219 | 0.0856086 | 0.9907399 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0143114 |   2.853249 | 0.0153682 |   2.791143 | 0.0205005 | 0.9994963 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.0139194 |   3.723816 | 0.0149473 |   3.395415 | 0.0319247 | 0.9990339 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0824617 |  20.749496 | 0.0885513 |  23.328807 | 0.0979685 | 0.9886289 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6793887 | 255.498849 | 0.7295605 | 139.341369 | 0.7713514 | 0.9786100 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8758544 | 251.158961 | 0.9405349 |  89.758171 | 1.2777450 | 0.4449817 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0453732 | 270.186059 | 1.1225724 | 137.789706 | 1.4575576 | 0.0745560 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.9301988 | 204.771770 | 0.9988926 | 159.136657 | 1.1377414 | 0.0904751 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.8256429 | 109.030686 | 0.8866154 | 178.378550 | 1.0573483 | 0.1068562 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.8738799 | 141.366335 | 0.9285055 | 159.601282 | 1.0643183 | 0.5449705 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.7846064 | 275.791884 | 0.8336516 | 101.651706 | 0.9622171 | 0.0232095 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.1919402 |  70.185326 | 0.2039383 |  41.579249 | 0.2068270 | 0.9936462 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0214112 |   2.668343 | 0.0227496 |   2.673217 | 0.0415073 | 0.9983098 |
| healthyverse  |         8 | NNAR                       | Test  | 1.0421577 | 497.911977 | 1.1073022 | 104.037681 | 1.1556748 | 0.9483988 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.1957039 |  74.873780 | 0.2079372 |  41.912544 | 0.2113018 | 0.9932867 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4390858 |  69.886381 | 0.4665328 |  85.868062 | 0.5584787 | 0.9800012 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0824563 | 330.509799 | 1.1501199 | 121.221217 | 1.3578191 | 0.0150198 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7007158 | 165.778328 | 0.7445170 | 106.906577 | 0.8812119 | 0.1878731 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.7806274 | 277.389549 | 0.8294238 |  98.750674 | 0.9620525 | 0.3949813 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.7861382 | 239.593391 | 0.8519652 | 153.808141 | 0.9252164 | 0.5425680 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0716383 |  18.509328 | 0.0776369 |  18.820210 | 0.0838074 | 0.9931602 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6314326 | 162.270851 | 0.6843054 | 114.962827 | 0.8679485 | 0.1061584 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0689954 |  24.570606 | 0.0747727 |  21.661865 | 0.0907283 | 0.9929616 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0170885 |   2.472921 | 0.0185194 |   2.443593 | 0.0288995 | 0.9990411 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0368897 |  10.436458 | 0.0399787 |  12.416550 | 0.0649229 | 0.9959845 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0745121 |  25.839131 | 0.0807514 |  27.524883 | 0.0955702 | 0.9914630 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4409485 | 168.436794 | 0.4778711 | 108.226000 | 0.5360403 | 0.9745787 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8668399 | 226.794565 | 0.9394244 | 104.739640 | 1.1692475 | 0.5063809 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0058638 | 298.344607 | 1.0900894 | 129.176690 | 1.2610792 | 0.1278655 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6514184 | 169.181692 | 0.7059647 | 132.117098 | 0.8677588 | 0.1058097 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.7051505 | 120.490496 | 0.7641960 | 151.455422 | 0.9137460 | 0.0710974 |
| healthyR.ai   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         1 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         6 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         7 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         8 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         9 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        10 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        12 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        13 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        14 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ## 1 health~         6 LM          Test   0.0665 18.9   0.0591 19.6   0.0966  0.992
    ## 2 health~         2 REGRESSION  Test   0.0554 10.2   0.0580  9.90  0.0682  0.996
    ## 3 health~         7 EARTH       Test   0.0143  2.85  0.0154  2.79  0.0205  0.999
    ## 4 health~         7 EARTH       Test   0.0214  2.67  0.0227  2.67  0.0415  0.998
    ## 5 health~         7 EARTH       Test   0.0171  2.47  0.0185  2.44  0.0289  0.999
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
    ## 1 healthyR.data <tibble>         <tibble>     <split [402|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [394|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [343|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [319|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [133|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [9 x 6]> <tibble>     <split [0|9]>    <mdl_time_tbl>

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
