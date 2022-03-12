Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
12 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,275
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

The last day in the data set is 2022-03-10 21:47:07, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2429.14
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31275         |
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
| r_version     |     20761 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     20761 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20761 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2598 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-10 | 2021-09-08 |      473 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |       p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|----------:|--------:|:------|
| size          |         0 |             1 | 1499267.68 | 1862155.09 | 357 | 16906 | 271098 | 3246663.0 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8338.64 |   16063.82 |   1 |   307 |   2678 |    8278.5 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-10 21:47:07 | 2021-09-08 18:32:47 |    18218 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 0M 0S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [414|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [406|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [355|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [331|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [145|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [21 x 6]> <tibble>     <split [0|21]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9519183 | 201.058601 | 0.8828574 | 145.572700 | 1.1814659 | 0.0000370 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0691212 |  22.151126 | 0.0641066 |  17.273652 | 0.0880174 | 0.9925593 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7453275 | 193.566138 | 0.6912546 | 115.987947 | 0.9139787 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0766701 |  31.845641 | 0.0711078 |  21.407428 | 0.1021870 | 0.9944616 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2755939 | 129.529533 | 0.2555998 |  50.623089 | 0.3939183 | 0.8971299 |
| healthyR.data |         8 | NNAR                       | Test  | 0.5376506 | 114.522703 | 0.4986445 |  79.504960 | 0.7377766 | 0.7231306 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0788241 |  27.789398 | 0.0731055 |  21.541661 | 0.1058060 | 0.9931215 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3251460 |  71.497385 | 0.3015569 |  60.752322 | 0.3950768 | 0.9936427 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9186923 | 409.377507 | 0.8520420 | 106.130477 | 1.0892783 | 0.4375046 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1321680 | 369.630007 | 1.0500302 | 126.437513 | 1.4126680 | 0.0101402 |
| healthyR.data |        13 | BATS                       | Test  | 0.7634691 | 214.789565 | 0.7080801 | 114.065528 | 0.9506669 | 0.0002248 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7458956 | 196.041325 | 0.6917815 | 115.505086 | 0.9154460 | 0.0410409 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.8098123 | 332.559810 | 0.9491124 | 157.073329 | 0.9461648 | 0.1745551 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0393935 |  15.205537 | 0.0461697 |  18.090855 | 0.0471795 | 0.9984678 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6602793 | 278.139235 | 0.7738575 | 144.478966 | 0.8015454 | 0.0694534 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0344436 |   9.884477 | 0.0403684 |  10.650428 | 0.0418245 | 0.9984741 |
| healthyR      |         7 | EARTH                      | Test  | 0.0163281 |   5.986619 | 0.0191367 |   6.053643 | 0.0194102 | 0.9995353 |
| healthyR      |         8 | NNAR                       | Test  | 0.0386824 |  16.900714 | 0.0453364 |  20.201016 | 0.0571294 | 0.9949922 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0408898 |  15.387827 | 0.0479235 |  17.024938 | 0.0486757 | 0.9972540 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3602598 | 185.319014 | 0.4222301 | 112.916112 | 0.4100411 | 0.9992301 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0603990 | 750.108394 | 1.2428039 | 134.385795 | 1.2670515 | 0.3970099 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0760926 | 716.557536 | 1.2611971 | 133.695140 | 1.4012864 | 0.0567825 |
| healthyR      |        13 | TBATS                      | Test  | 0.6652831 | 251.438947 | 0.7797220 | 155.268545 | 0.8104470 | 0.0247025 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6304353 | 144.961707 | 0.7388799 | 170.706007 | 0.8094018 | 0.0403463 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7293861 | 197.734265 | 0.7915522 | 154.366933 | 0.8810552 | 0.4020603 |
| healthyR.ts   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7174930 | 357.308385 | 0.7786455 | 125.222735 | 0.9055803 | 0.0003251 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0475934 |  21.419475 | 0.0516498 |  16.680251 | 0.0612388 | 0.9971036 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0155648 |   3.228762 | 0.0168914 |   3.159566 | 0.0270851 | 0.9990053 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.2093004 |  83.678446 | 0.2271393 |  55.735770 | 0.2318747 | 0.9831815 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0494194 |  12.771309 | 0.0536314 |  12.055883 | 0.0643994 | 0.9957239 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4250623 | 180.530091 | 0.4612907 | 112.039556 | 0.4790775 | 0.9980776 |
| healthyR.ts   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3133103 | 627.123925 | 1.4252449 | 136.012490 | 1.6495760 | 0.0000427 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7157910 | 143.065085 | 0.7767985 | 140.647903 | 0.9168606 | 0.0063363 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6641570 | 345.561389 | 0.7207636 | 111.520507 | 0.8577698 | 0.0459976 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5533291 | 126.274147 | 0.6079936 | 132.306625 | 0.7054305 | 0.2957621 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0521635 |  18.889575 | 0.0573168 |  15.137645 | 0.0614453 | 0.9990446 |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9183201 | 441.406723 | 1.0090428 | 120.438476 | 1.1726907 | 0.0198300 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0522136 |  19.186047 | 0.0573719 |  15.426683 | 0.0616342 | 0.9990522 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0190621 |   3.222743 | 0.0209453 |   3.255588 | 0.0345761 | 0.9983855 |
| healthyverse  |         8 | NNAR                       | Test  | 0.1081077 |  25.544505 | 0.1187879 |  24.883377 | 0.1473228 | 0.9906806 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0581600 |  16.032016 | 0.0639057 |  15.811486 | 0.0687227 | 0.9982383 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2526779 |  45.786786 | 0.2776405 |  60.662792 | 0.3091489 | 0.9974448 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8087008 | 350.164514 | 0.8885940 | 116.135718 | 0.9639817 | 0.4168889 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0577654 | 356.178495 | 1.1622643 | 127.006998 | 1.4727988 | 0.0102579 |
| healthyverse  |        13 | TBATS                      | Test  | 0.5904750 | 118.578269 | 0.6488092 | 123.907057 | 0.7940745 | 0.0682774 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9104753 | 452.541008 | 1.0004230 | 121.314977 | 1.1336024 | 0.0595555 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6937632 | 386.123658 | 0.9593944 | 146.343115 | 0.8066977 | 0.2625798 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.5506541 | 280.499240 | 0.7614911 | 147.140345 | 0.7114566 | 0.0078355 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0509097 |  37.883523 | 0.0704023 |  22.133213 | 0.0655916 | 0.9988075 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0096038 |   2.373517 | 0.0132809 |   2.344690 | 0.0112288 | 0.9997329 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0976577 |  72.946409 | 0.1350493 |  51.231674 | 0.1087989 | 0.9938757 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0542348 |  32.734151 | 0.0750005 |  22.942069 | 0.0670436 | 0.9976103 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4136889 | 315.678361 | 0.5720840 | 120.454495 | 0.4469035 | 0.9870933 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0461736 | 879.020421 | 1.4467374 | 150.231464 | 1.2793496 | 0.0165836 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.5375913 | 272.317476 | 0.7434267 | 143.459430 | 0.6916838 | 0.0125206 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5091831 | 118.881858 | 0.7041415 | 174.896463 | 0.6269037 | 0.0070757 |
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
    ##   package       .model_id .model_desc .type      mae  mape    mase smape    rmse
    ##   <chr>             <int> <chr>       <chr>    <dbl> <dbl>   <dbl> <dbl>   <dbl>
    ## 1 healthyR.data         2 REGRESSION  Test   0.0691  22.2   0.0641 17.3   0.0880
    ## 2 healthyR              7 EARTH       Test   0.0163   5.99  0.0191  6.05  0.0194
    ## 3 healthyR.ts           7 EARTH       Test   0.0156   3.23  0.0169  3.16  0.0271
    ## 4 healthyverse          7 EARTH       Test   0.0191   3.22  0.0209  3.26  0.0346
    ## 5 healthyR.ai           7 EARTH       Test   0.00960  2.37  0.0133  2.34  0.0112
    ## 6 TidyDensity          NA <NA>        <NA>  NA       NA    NA      NA    NA     
    ## # ... with 1 more variable: rsq <dbl>

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [414|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [406|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [355|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [331|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [145|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [21 x 6]> <tibble>     <split [0|21]>   <mdl_time_tbl>

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
