Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
10 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,966
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

The last day in the data set is 2022-03-08 23:48:40, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2383.17
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30966         |
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
| r_version     |     20560 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20560 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20560 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2576 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-08 | 2021-09-06 |      471 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1504200.70 | 1864193.22 | 357 | 16923.0 | 271483 | 3247107 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8257.62 |   15935.11 |   1 |   284.5 |   2682 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-08 23:48:40 | 2021-09-06 11:10:00 |    18046 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      0 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [412|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [404|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [353|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [329|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [143|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [19 x 6]> <tibble>     <split [0|19]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.9449052 | 196.492634 | 0.8844487 | 140.550917 | 1.1710775 | 0.1444216 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0714537 |  22.699631 | 0.0668820 |  17.835186 | 0.0983778 | 0.9921571 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7782316 | 173.970824 | 0.7284391 | 119.821799 | 0.9401080 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0880742 |  33.393886 | 0.0824390 |  23.748561 | 0.1178249 | 0.9935419 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2813889 | 115.349270 | 0.2633852 |  51.213395 | 0.3992498 | 0.9072934 |
| healthyR.data |         8 | NNAR                       | Test  | 0.6281843 | 107.044729 | 0.5879921 |  65.195556 | 1.0498332 | 0.6923250 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0891082 |  30.307196 | 0.0834070 |  24.095212 | 0.1246365 | 0.9915310 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3460979 |  63.624859 | 0.3239540 |  61.547995 | 0.4341704 | 0.9908128 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8765888 | 351.895124 | 0.8205033 | 101.540488 | 1.0689467 | 0.4544759 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2053589 | 334.062270 | 1.1282382 | 134.243544 | 1.4405994 | 0.0057203 |
| healthyR.data |        13 | TBATS                      | Test  | 0.7757071 | 172.449429 | 0.7260762 | 125.166600 | 0.9086303 | 0.0635353 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7788733 | 176.166421 | 0.7290397 | 119.418576 | 0.9406782 | 0.0442204 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.7264107 | 260.355363 | 0.8479525 | 147.854117 | 0.8719765 | 0.2880387 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0382110 |  10.593703 | 0.0446043 |  12.497007 | 0.0461531 | 0.9985948 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6531475 | 306.650743 | 0.7624309 | 142.630631 | 0.7784715 | 0.0909038 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0384430 |  13.477039 | 0.0448752 |  13.175416 | 0.0451713 | 0.9986083 |
| healthyR      |         7 | EARTH                      | Test  | 0.0092489 |   2.335634 | 0.0107964 |   2.269049 | 0.0136775 | 0.9997220 |
| healthyR      |         8 | NNAR                       | Test  | 0.0808672 |  48.044377 | 0.0943977 |  32.978091 | 0.1159510 | 0.9888065 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0418828 |  16.621708 | 0.0488905 |  19.656919 | 0.0482011 | 0.9978642 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3385194 | 142.627096 | 0.3951598 | 102.548575 | 0.3902582 | 0.9993555 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0434148 | 704.394002 | 1.2179970 | 119.584910 | 1.2913699 | 0.3884983 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0990987 | 732.259506 | 1.2829979 | 131.331834 | 1.4283925 | 0.0551146 |
| healthyR      |        13 | TBATS                      | Test  | 0.6481699 | 210.608594 | 0.7566205 | 155.396451 | 0.7794897 | 0.0840947 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6259457 | 184.273440 | 0.7306778 | 133.327255 | 0.8122969 | 0.0075440 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.6115309 | 203.179741 | 0.7524199 | 144.924250 | 0.7747022 | 0.1199682 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0529296 |  39.387419 | 0.0651239 |  25.703710 | 0.0710773 | 0.9962127 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7399744 | 594.677134 | 0.9104552 | 113.869817 | 0.9119954 | 0.0005838 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0515772 |  38.107211 | 0.0634599 |  24.969439 | 0.0690746 | 0.9964637 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0112008 |   3.362712 | 0.0137813 |   3.271062 | 0.0130412 | 0.9997077 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1912902 |  81.525499 | 0.2353610 |  62.352700 | 0.2184993 | 0.9765399 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0537109 |  27.402246 | 0.0660852 |  21.431932 | 0.0681430 | 0.9948729 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4340870 | 210.478485 | 0.5340951 | 119.380265 | 0.4861355 | 0.9975870 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9263469 | 405.272832 | 1.1397655 | 118.293571 | 1.2734909 | 0.1596732 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3110410 | 830.151757 | 1.6130883 | 131.439015 | 1.5954088 | 0.0000918 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6409804 | 200.736132 | 0.7886542 | 145.137875 | 0.8396934 | 0.0050739 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7504595 | 622.407031 | 0.9233558 | 113.116460 | 0.8904810 | 0.0220610 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5601382 |  97.954685 | 0.5803842 | 121.348953 | 0.7143823 | 0.3116950 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.0712594 | 484.868754 | 1.1099796 | 119.928563 | 1.3534532 | 0.0049466 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0510769 |  16.516044 | 0.0529230 |  13.864135 | 0.0607248 | 0.9991216 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0171571 |   2.626247 | 0.0177773 |   2.636548 | 0.0288803 | 0.9988668 |
| healthyverse  |         8 | NNAR                       | Test  | 0.0492567 |  11.680743 | 0.0510371 |  10.472229 | 0.0676896 | 0.9965378 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0536998 |  14.674505 | 0.0556407 |  15.234314 | 0.0639290 | 0.9985578 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2543049 |  44.253309 | 0.2634966 |  60.799254 | 0.3061501 | 0.9971737 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2804212 | 508.808574 | 1.3267015 | 133.303474 | 1.6597781 | 0.0032064 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6073990 | 137.403293 | 0.6293532 | 127.412611 | 0.7991522 | 0.1049750 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.1045522 | 508.643928 | 1.1444758 | 122.433575 | 1.3469479 | 0.0474057 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6165640 | 245.728197 | 0.8022324 | 138.021399 | 0.7391436 | 0.4134212 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0577920 |  25.791841 | 0.0751952 |  23.634614 | 0.0730072 | 0.9989715 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.5855765 | 209.057350 | 0.7619136 | 133.375525 | 0.7453512 | 0.0447160 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0723241 |  39.487608 | 0.0941033 |  30.658451 | 0.0901269 | 0.9988659 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0116641 |   2.793975 | 0.0151766 |   2.746704 | 0.0140621 | 0.9996367 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.1028480 |  53.604723 | 0.1338190 |  34.656343 | 0.1312787 | 0.9769824 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0755680 |  42.174002 | 0.0983242 |  35.752985 | 0.0910266 | 0.9977830 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4309554 | 255.636738 | 0.5607308 | 119.318777 | 0.4727929 | 0.9873010 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9290898 | 451.461410 | 1.2088705 | 103.977703 | 1.1902727 | 0.3362631 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0281441 | 492.066225 | 1.3377534 | 143.553645 | 1.2838102 | 0.0567782 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.5554035 | 211.592250 | 0.7226545 | 135.729301 | 0.7156372 | 0.0963195 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5896724 | 207.348982 | 0.7672430 | 136.298396 | 0.7350579 | 0.0278162 |
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
    ## 1 healthyR.data         2 REGRESSION  Test   0.0715  22.7   0.0669 17.8   0.0984
    ## 2 healthyR              7 EARTH       Test   0.00925  2.34  0.0108  2.27  0.0137
    ## 3 healthyR.ts           7 EARTH       Test   0.0112   3.36  0.0138  3.27  0.0130
    ## 4 healthyverse          7 EARTH       Test   0.0172   2.63  0.0178  2.64  0.0289
    ## 5 healthyR.ai           7 EARTH       Test   0.0117   2.79  0.0152  2.75  0.0141
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [412|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [404|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [353|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [329|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [143|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [19 x 6]> <tibble>     <split [0|19]>   <mdl_time_tbl>

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
