Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
25 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,705
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

The last day in the data set is 2022-02-23 20:51:50, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2068.22
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29705          |
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
| r\_version     |      19746 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19746 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19746 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2482 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-23 | 2021-08-28 |       458 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1511725.99 | 1869627.27 | 357 | 17597 | 271097 | 3247945 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8260.03 |   15829.41 |   1 |   253 |   2742 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-23 20:51:50 | 2021-08-28 20:44:32 |     17298 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 59M 13S |        60 |

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
    ## 1 healthyR.data <tibble>         <tibble>     <split [399|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [391|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [340|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [316|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [130|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [6 x 6]> <tibble>     <split [0|6]>    <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8960392 | 229.306967 | 0.7697784 | 129.178460 | 1.1366668 | 0.5538391 |
| healthyR.data |          2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8695930 | 135.705490 | 0.7470588 | 146.697058 | 1.0092718 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0678788 |  20.095579 | 0.0583140 |  20.334114 | 0.0973976 | 0.9932525 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0860085 |  29.701829 | 0.0738891 |  24.376548 | 0.1498913 | 0.9830099 |
| healthyR.data |          8 | NNAR                       | Test  | 0.5600221 |  78.130320 | 0.4811095 |  56.210846 | 0.8634804 | 0.7830890 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0736994 |  19.478670 | 0.0633144 |  17.946185 | 0.1084112 | 0.9919607 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4142009 |  93.416198 | 0.3558359 |  75.603367 | 0.5319734 | 0.9925811 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0165837 | 387.433620 | 0.8733370 | 106.526688 | 1.2234926 | 0.4776710 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2619209 | 450.718261 | 1.0841037 | 138.533335 | 1.4453288 | 0.0388004 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8069851 | 143.337317 | 0.6932729 | 145.105602 | 0.9658660 | 0.1390939 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8693494 | 136.600559 | 0.7468494 | 146.065055 | 1.0091694 | 0.0279901 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8047945 | 173.572902 | 0.8479824 | 148.531188 | 0.9559621 | 0.4243397 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0590118 |   8.509071 | 0.0621786 |   7.986066 | 0.0717388 | 0.9952614 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6488285 | 131.267632 | 0.6836468 | 111.339154 | 0.8784630 | 0.1403838 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0566313 |   9.339746 | 0.0596703 |   8.748376 | 0.0689962 | 0.9952730 |
| healthyR      |          7 | EARTH                      | Test  | 0.0332304 |   4.315763 | 0.0350137 |   4.333260 | 0.0730629 | 0.9940892 |
| healthyR      |          8 | NNAR                       | Test  | 0.3831582 | 114.964626 | 0.4037197 |  56.640459 | 0.4683782 | 0.9310514 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0609922 |  12.994053 | 0.0642653 |  14.944857 | 0.0745892 | 0.9947250 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4900779 | 133.825113 | 0.5163771 | 103.917399 | 0.5888124 | 0.9755624 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8658407 | 294.742626 | 0.9123045 |  88.623242 | 1.1573123 | 0.4830711 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0662457 | 376.664393 | 1.1234639 | 123.194815 | 1.3524769 | 0.0702738 |
| healthyR      |         13 | TBATS                      | Test  | 0.6617369 | 101.370859 | 0.6972479 | 120.315841 | 0.8739085 | 0.1535380 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7553434 | 141.607249 | 0.7958776 | 135.469856 | 0.9453502 | 0.0610842 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0611666 | 399.522144 | 1.2392069 | 160.360033 | 1.2773233 | 0.4580697 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0765631 |  23.908967 | 0.0894087 |  21.793594 | 0.0927297 | 0.9906421 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.6610420 | 141.613381 | 0.7719502 | 132.809753 | 0.8826820 | 0.1349776 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0702720 |  22.819446 | 0.0820621 |  22.338805 | 0.0858493 | 0.9909627 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0140036 |   3.191901 | 0.0163531 |   3.107594 | 0.0204592 | 0.9995410 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0138342 |  17.944278 | 0.0161552 |   8.873907 | 0.0278005 | 0.9992297 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0789741 |  21.623707 | 0.0922242 |  22.411198 | 0.0951764 | 0.9893550 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7195610 | 364.687473 | 0.8402875 | 141.842023 | 0.8141014 | 0.9767017 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9003622 | 261.405871 | 1.0514231 |  91.165586 | 1.3183340 | 0.5100804 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9637718 | 285.135566 | 1.1254715 | 129.226660 | 1.3624737 | 0.1387581 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7968289 | 171.836742 | 0.9305192 | 145.868470 | 1.0237955 | 0.1007851 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.7233427 | 136.533400 | 0.8447037 | 163.836714 | 0.9428953 | 0.0344075 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9752805 | 122.422276 | 0.9415342 | 163.863157 | 1.1687279 | 0.8053361 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0670246 |   8.547864 | 0.0647054 |   8.515952 | 0.0902137 | 0.9927064 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8445664 | 147.442911 | 0.8153431 | 109.156372 | 1.0168766 | 0.0436563 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0731215 |  10.050649 | 0.0705914 |  10.436319 | 0.0967558 | 0.9926799 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0247480 |   2.208647 | 0.0238917 |   2.200034 | 0.0473180 | 0.9983659 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0240860 |   2.565507 | 0.0232526 |   2.562561 | 0.0474105 | 0.9980898 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0694903 |   8.982741 | 0.0670858 |   8.757197 | 0.0930122 | 0.9921505 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5379238 |  71.371107 | 0.5193108 |  93.538174 | 0.6713652 | 0.9758584 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7094402 | 180.474000 | 0.6848925 |  83.832077 | 0.8508634 | 0.6329722 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0893338 | 243.372422 | 1.0516411 | 134.753087 | 1.3141518 | 0.0303699 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8816429 | 100.136825 | 0.8511367 | 129.647692 | 1.1211222 | 0.0708890 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8631403 | 133.860910 | 0.8332743 | 109.075440 | 1.0501780 | 0.3469439 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.7856604 | 198.808463 | 0.8577340 | 149.177027 | 0.9719785 | 0.4092004 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0701650 |  17.430869 | 0.0766017 |  19.725286 | 0.0821180 | 0.9936022 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.6449437 | 130.528330 | 0.7041085 | 135.248299 | 0.8720632 | 0.1126948 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0703901 |  18.208795 | 0.0768475 |  19.261158 | 0.0917384 | 0.9930812 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0171256 |   2.467812 | 0.0186966 |   2.438383 | 0.0289537 | 0.9990440 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0158610 |   2.478186 | 0.0173160 |   2.502392 | 0.0334751 | 0.9989082 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0701776 |  18.948910 | 0.0766155 |  23.318411 | 0.0910164 | 0.9919049 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8450839 | 373.670051 | 0.9226088 | 151.586248 | 0.9237488 | 0.9376897 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8956740 | 303.118221 | 0.9778399 |  98.361784 | 1.2429353 | 0.4910769 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0285916 | 340.428482 | 1.1229509 | 130.540508 | 1.3293226 | 0.1261676 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.6481638 | 124.179763 | 0.7076240 | 133.770173 | 0.8638689 | 0.1278106 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.7184129 | 100.062370 | 0.7843175 | 168.894331 | 0.9260509 | 0.1148366 |
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
    ## 1 health~         6 LM          Test   0.0679 20.1   0.0583 20.3   0.0974  0.993
    ## 2 health~         6 LM          Test   0.0566  9.34  0.0597  8.75  0.0690  0.995
    ## 3 health~         7 EARTH       Test   0.0140  3.19  0.0164  3.11  0.0205  1.00 
    ## 4 health~         7 EARTH       Test   0.0247  2.21  0.0239  2.20  0.0473  0.998
    ## 5 health~         7 EARTH       Test   0.0171  2.47  0.0187  2.44  0.0290  0.999
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
    ## 1 healthyR.data <tibble>         <tibble>     <split [399|28]> <mdl_time_tbl>   
    ## 2 healthyR      <tibble>         <tibble>     <split [391|28]> <mdl_time_tbl>   
    ## 3 healthyR.ts   <tibble>         <tibble>     <split [340|28]> <mdl_time_tbl>   
    ## 4 healthyverse  <tibble>         <tibble>     <split [316|28]> <mdl_time_tbl>   
    ## 5 healthyR.ai   <tibble>         <tibble>     <split [130|28]> <mdl_time_tbl>   
    ## 6 TidyDensity   <tibble [6 x 6]> <tibble>     <split [0|6]>    <mdl_time_tbl>

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
