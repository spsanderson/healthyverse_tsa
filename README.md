Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
06 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 30,554
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

The last day in the data set is 2022-03-04 23:20:55, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2286.71
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 30554         |
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
| r_version     |     20297 |          0.34 |   5 |   5 |     0 |       29 |          0 |
| r_arch        |     20297 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20297 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2543 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-04 | 2021-09-02 |      467 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1506608.73 | 1866258.12 | 357 | 16923 | 271098 | 3247728 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8305.48 |   15976.33 |   1 |   245 |   2687 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-04 23:20:55 | 2021-09-02 22:16:08 |    17781 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     12 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [400|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [349|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [325|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [139|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [15 x 6]> <tibble>     <split [0|15]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 0.7977588 | 268.232690 | 0.6739846 | 128.640448 | 0.9907844 | 0.3248304 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0736686 |  25.078440 | 0.0622388 |  23.960424 | 0.0983752 | 0.9907742 |
| healthyR.data |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7673805 | 248.781437 | 0.6483195 | 120.351131 | 0.9358369 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.1021881 |  40.652551 | 0.0863334 |  29.422264 | 0.1323987 | 0.9919924 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2459860 | 108.938851 | 0.2078207 |  53.016915 | 0.3765487 | 0.9006941 |
| healthyR.data |         8 | NNAR                       | Test  | 0.4415173 | 141.183732 | 0.3730148 |  64.950672 | 0.6829016 | 0.7553415 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0914946 |  31.288621 | 0.0772990 |  25.459536 | 0.1264467 | 0.9895139 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3424044 | 102.459277 | 0.2892795 |  75.089414 | 0.4240880 | 0.9945077 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8032092 | 419.325016 | 0.6785893 |  92.882920 | 1.0718404 | 0.4413773 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1840316 | 462.048352 | 1.0003262 | 128.155579 | 1.4349486 | 0.0103541 |
| healthyR.data |        13 | BATS                       | Test  | 0.6899736 | 187.365899 | 0.5829225 | 117.850348 | 0.8563942 | 0.2355073 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7692203 | 251.671549 | 0.6498739 | 120.104688 | 0.9382987 | 0.0000866 |
| healthyR.data |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6486671 | 199.537772 | 0.7364014 | 138.339573 | 0.8012105 | 0.1984505 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0470987 |  13.205352 | 0.0534690 |  16.683417 | 0.0550536 | 0.9979652 |
| healthyR      |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6757909 | 362.941516 | 0.7671939 | 115.657940 | 0.9114345 | 0.0858880 |
| healthyR      |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0432259 |   9.929810 | 0.0490723 |  10.550970 | 0.0510228 | 0.9979725 |
| healthyR      |         7 | EARTH                      | Test  | 0.0221679 |   5.270476 | 0.0251662 |   5.230883 | 0.0335490 | 0.9989638 |
| healthyR      |         8 | NNAR                       | Test  | 0.0897087 |  26.694906 | 0.1018422 |  28.411829 | 0.1157708 | 0.9869850 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0469076 |  13.812374 | 0.0532520 |  15.958822 | 0.0558234 | 0.9969751 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3526184 | 125.990169 | 0.4003113 | 101.689861 | 0.4022552 | 0.9994513 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8985406 | 576.958537 | 1.0200712 |  98.543208 | 1.2158324 | 0.4368674 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1544275 | 753.911955 | 1.3105677 | 120.713026 | 1.4337296 | 0.0511249 |
| healthyR      |        13 | TBATS                      | Test  | 0.6368229 | 235.064097 | 0.7229554 | 118.503321 | 0.8590718 | 0.0333553 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6731611 | 287.459241 | 0.7642083 | 111.620948 | 0.9055120 | 0.0540185 |
| healthyR      |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7336552 | 176.589851 | 0.8469349 | 150.079076 | 0.9362553 | 0.2197816 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0585106 |  28.784331 | 0.0675449 |  21.607115 | 0.0743395 | 0.9929644 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6432617 | 389.402960 | 0.7425843 | 119.400677 | 0.8158425 | 0.0148987 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0571615 |  26.952609 | 0.0659875 |  20.489956 | 0.0727473 | 0.9931530 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0121188 |   3.129946 | 0.0139900 |   3.051520 | 0.0158073 | 0.9995844 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1321651 |  54.968677 | 0.1525720 |  38.658901 | 0.1790106 | 0.9793411 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0605871 |  16.912954 | 0.0699420 |  15.869712 | 0.0755255 | 0.9918505 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5309182 | 230.411494 | 0.6128944 | 125.552257 | 0.6060793 | 0.9860457 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9580358 | 335.631685 | 1.1059609 | 117.803391 | 1.3057489 | 0.2334002 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2391941 | 580.889330 | 1.4305314 | 126.955080 | 1.5452985 | 0.0035363 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.6836549 | 141.731659 | 0.7892144 | 135.295346 | 0.8991465 | 0.0812650 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6194906 | 302.515248 | 0.7151428 | 114.709581 | 0.7738638 | 0.0569127 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.6429641 | 106.341580 | 0.6244157 | 134.235315 | 0.8115989 | 0.6174342 |
| healthyverse  |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 1.1051518 | 551.790195 | 1.0732701 | 116.251379 | 1.3622259 | 0.0160706 |
| healthyverse  |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0541024 |  18.331787 | 0.0525416 |  15.705825 | 0.0682380 | 0.9957617 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0215761 |   3.102137 | 0.0209536 |   3.101071 | 0.0389929 | 0.9984207 |
| healthyverse  |         8 | NNAR                       | Test  | 0.4235407 | 231.775460 | 0.4113223 |  85.809996 | 0.5692721 | 0.9688698 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0577977 |  17.447368 | 0.0561303 |  18.218073 | 0.0717098 | 0.9953904 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3161608 |  48.383336 | 0.3070401 |  65.875622 | 0.4164062 | 0.9901606 |
| healthyverse  |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2799095 | 562.922673 | 1.2429864 | 128.968702 | 1.6984978 | 0.0203275 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7393730 | 250.755900 | 0.7180434 | 115.056768 | 0.9201367 | 0.1233339 |
| healthyverse  |        14 | THETA METHOD               | Test  | 1.0844646 | 546.331923 | 1.0531797 | 116.886452 | 1.3081229 | 0.1564326 |
| healthyverse  |        15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.5940480 | 210.364646 | 0.7223903 | 129.787179 | 0.7138763 | 0.3461039 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.7186596 | 340.300546 | 0.8739238 | 125.785611 | 0.9532695 | 0.0523691 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0653723 |  31.583526 | 0.0794958 |  26.165152 | 0.0829165 | 0.9964803 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0151624 |   2.558506 | 0.0184382 |   2.524858 | 0.0264721 | 0.9991035 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0732994 |  16.319013 | 0.0891355 |  17.308549 | 0.1121432 | 0.9870607 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0660443 |  32.528592 | 0.0803129 |  29.620996 | 0.0832470 | 0.9952825 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3672959 | 162.711832 | 0.4466490 | 107.245313 | 0.4083678 | 0.9971316 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1029359 | 546.564278 | 1.3412219 | 138.342770 | 1.3715308 | 0.0684763 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.7309255 | 321.867702 | 0.8888398 | 125.811829 | 0.9493275 | 0.0584691 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6377655 | 272.404577 | 0.7755528 | 115.455378 | 0.8794628 | 0.0318815 |
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
    ## 1 health~         2 REGRESSION  Test   0.0737 25.1   0.0622 24.0   0.0984  0.991
    ## 2 health~         7 EARTH       Test   0.0222  5.27  0.0252  5.23  0.0335  0.999
    ## 3 health~         7 EARTH       Test   0.0121  3.13  0.0140  3.05  0.0158  1.00 
    ## 4 health~         7 EARTH       Test   0.0216  3.10  0.0210  3.10  0.0390  0.998
    ## 5 health~         7 EARTH       Test   0.0152  2.56  0.0184  2.52  0.0265  0.999
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [400|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [349|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [325|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [139|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [15 x 6]> <tibble>     <split [0|15]>   <mdl_time_tbl>

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
