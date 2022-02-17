Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
17 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,155
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

The last day in the data set is 2022-02-15 23:21:54, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1878.72
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29155          |
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
| r\_version     |      19373 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19373 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19373 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2418 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-15 | 2021-08-25 |       450 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1519181.74 | 1871761.35 | 357 | 23389 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8255.81 |   15777.31 |   1 |   338 |   2782 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-15 23:21:54 | 2021-08-25 21:02:29 |     17026 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 57M 18S |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [392|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [383|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [332|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [308|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [122|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0163403 | 279.548521 | 0.7980887 | 135.857390 | 1.2940656 | 0.0228395 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0634579 |  30.344696 | 0.0498308 |  18.138078 | 0.0832468 | 0.9939887 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8937737 | 119.541312 | 0.7018423 | 186.243503 | 1.0239368 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0667241 |  28.783154 | 0.0523956 |  19.121272 | 0.0953117 | 0.9932512 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0702268 |  31.548994 | 0.0551461 |  19.769740 | 0.1414618 | 0.9813767 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0168287 |   5.525472 | 0.0132149 |   8.735028 | 0.0223911 | 0.9994734 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0711352 |  26.933069 | 0.0558594 |  20.009611 | 0.1049378 | 0.9915867 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4932765 | 185.944874 | 0.3873490 |  79.007267 | 0.6198931 | 0.9888897 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1378191 | 648.825420 | 0.8934808 | 103.437125 | 1.4166026 | 0.3622532 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5299648 | 819.879821 | 1.2014160 | 143.739260 | 1.7767966 | 0.0018441 |
| healthyR.data |         13 | BATS                       | Test  | 0.9805235 | 213.969460 | 0.7699633 | 141.723770 | 1.2337881 | 0.0178061 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8936718 | 121.075275 | 0.7017624 | 185.474950 | 1.0229274 | 0.0051897 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8805927 | 354.754412 | 0.9706864 | 156.541358 | 1.0586371 | 0.3871586 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0635546 |  21.722739 | 0.0700569 |  16.446755 | 0.0747080 | 0.9949049 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6816064 | 181.191250 | 0.7513417 | 125.463320 | 0.8671436 | 0.2996729 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0592968 |  19.399533 | 0.0653635 |  17.115929 | 0.0706966 | 0.9949207 |
| healthyR      |          7 | EARTH                      | Test  | 0.0332506 |   6.420243 | 0.0366525 |   7.182820 | 0.0730975 | 0.9940552 |
| healthyR      |          8 | NNAR                       | Test  | 0.0096937 |   2.079717 | 0.0106855 |   1.921452 | 0.0193798 | 0.9996666 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0651815 |  27.576668 | 0.0718502 |  22.316847 | 0.0780791 | 0.9936839 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4867123 | 248.038098 | 0.5365079 | 104.693730 | 0.5887185 | 0.9721083 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9972821 | 239.223762 | 1.0993142 |  95.754691 | 1.3149976 | 0.5700128 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0602493 | 286.600913 | 1.1687237 | 127.129052 | 1.3429786 | 0.1588278 |
| healthyR      |         13 | TBATS                      | Test  | 0.6513310 | 120.782669 | 0.7179689 | 114.062683 | 0.8615504 | 0.2372625 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7761421 | 178.951300 | 0.8555494 | 156.102986 | 0.9636840 | 0.0339716 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2686206 | 688.990943 | 1.4500819 | 162.748651 | 1.4824975 | 0.1407487 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0747071 |  46.861095 | 0.0853930 |  19.288418 | 0.0932407 | 0.9902442 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1246762 | 640.822527 | 1.2855480 | 163.059659 | 1.3440537 | 0.2789076 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0707099 |  43.713950 | 0.0808241 |  21.382273 | 0.0864259 | 0.9905530 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0144264 |   2.979243 | 0.0164900 |   2.911256 | 0.0207854 | 0.9995264 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0138661 |   3.323759 | 0.0158495 |   3.600398 | 0.0269519 | 0.9994280 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0822446 |  50.737834 | 0.0940087 |  24.946398 | 0.1004391 | 0.9882536 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7469067 | 491.460948 | 0.8537430 | 142.341598 | 0.8308417 | 0.9719036 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0603013 | 392.781588 | 1.2119650 |  93.583455 | 1.4361983 | 0.5915787 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2499255 | 871.069851 | 1.4287126 | 129.209100 | 1.7409228 | 0.2602572 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9798267 | 404.109030 | 1.1199795 | 152.721469 | 1.2139132 | 0.1159136 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0916018 | 516.052090 | 1.2477427 | 157.605284 | 1.3598858 | 0.0564898 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1603402 | 112.681580 | 1.1605163 | 158.854532 | 1.3822513 | 0.0804221 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0640858 |   7.292261 | 0.0640956 |   7.033568 | 0.0875042 | 0.9904040 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8353001 | 129.196504 | 0.8354269 | 120.855708 | 0.9848973 | 0.0282213 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0604188 |   6.378584 | 0.0604280 |   6.186564 | 0.0883871 | 0.9903910 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0216262 |   2.002884 | 0.0216295 |   2.009664 | 0.0430497 | 0.9986091 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0145026 |   1.178294 | 0.0145048 |   1.202885 | 0.0320190 | 0.9991610 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0784612 |  12.641783 | 0.0784731 |  11.391313 | 0.0960211 | 0.9900595 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6052978 |  73.571144 | 0.6053897 |  90.239248 | 0.7182097 | 0.9628205 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9648755 | 185.686602 | 0.9650220 |  94.062168 | 1.1461444 | 0.3547259 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3024437 | 243.234460 | 1.3026414 | 131.965670 | 1.5822677 | 0.0209697 |
| healthyverse  |         13 | TBATS                      | Test  | 1.0718061 | 134.223838 | 1.0719688 | 151.404185 | 1.2393578 | 0.0091526 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7917172 | 115.653789 | 0.7918374 |  98.830779 | 0.9810502 | 0.0164660 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0313443 | 638.452997 | 1.0936310 | 156.713708 | 1.2121658 | 0.4394209 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0776004 |  42.338684 | 0.0822870 |  27.570920 | 0.0907126 | 0.9923462 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8022838 | 322.034634 | 0.8507367 | 160.176827 | 0.9760479 | 0.1584431 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0671632 |  26.151282 | 0.0712195 |  21.740606 | 0.0841077 | 0.9924531 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0163040 |   2.412869 | 0.0172887 |   2.384590 | 0.0293419 | 0.9990105 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0338208 |  10.938811 | 0.0358633 |   8.648542 | 0.0706277 | 0.9950525 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0766907 |  49.062696 | 0.0813224 |  30.309098 | 0.0936969 | 0.9904219 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7209149 | 606.208021 | 0.7644536 | 145.077672 | 0.8017619 | 0.9643395 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0487167 | 624.480873 | 1.1120526 | 126.026554 | 1.3240412 | 0.5308036 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1085557 | 471.890042 | 1.1755055 | 134.482882 | 1.4963578 | 0.1460931 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8479750 | 429.558387 | 0.8991874 | 153.218146 | 1.0060165 | 0.1877977 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8538710 | 278.539060 | 0.9054395 | 156.988325 | 1.0798109 | 0.0170439 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ##   package    .model_id .model_desc .type     mae  mape   mase smape   rmse   rsq
    ##   <chr>          <int> <chr>       <chr>   <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR.~         8 NNAR        Test  0.0168   5.53 0.0132  8.74 0.0224 0.999
    ## 2 healthyR           8 NNAR        Test  0.00969  2.08 0.0107  1.92 0.0194 1.00 
    ## 3 healthyR.~         7 EARTH       Test  0.0144   2.98 0.0165  2.91 0.0208 1.00 
    ## 4 healthyve~         8 NNAR        Test  0.0145   1.18 0.0145  1.20 0.0320 0.999
    ## 5 healthyR.~         7 EARTH       Test  0.0163   2.41 0.0173  2.38 0.0293 0.999

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [392|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [383|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [332|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [308|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [122|28]> <mdl_time_tbl>

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
