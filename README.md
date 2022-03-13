Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
13 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,420
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

The last day in the data set is 2022-03-11 23:29:54, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2454.86
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31420         |
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
| r_version     |     20858 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     20858 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     20858 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2610 |          0.92 |   2 |   2 |     0 |      101 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-11 | 2021-09-09 |      474 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1497898.55 | 1861334.76 | 357 | 16906 | 271098 | 3246657 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8319.04 |   16037.13 |   1 |   322 |   2660 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-11 23:29:54 | 2021-09-09 18:26:46 |    18296 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     43 |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [415|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [407|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [356|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [332|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [146|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [22 x 6]> <tibble>     <split [0|22]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.0063199 |  263.486120 | 0.9011301 | 149.327155 | 1.2139364 | 0.0915195 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0595449 |   14.366552 | 0.0533207 |  12.474343 | 0.0739797 | 0.9933986 |
| healthyR.data |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7043704 |  168.407968 | 0.6307432 | 116.937678 | 0.8746408 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0554476 |   23.284457 | 0.0496517 |  16.323483 | 0.0708832 | 0.9954441 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2364941 |  125.497376 | 0.2117735 |  45.353622 | 0.3479998 | 0.9081480 |
| healthyR.data |         8 | NNAR                       | Test  | 0.2033999 |   61.854962 | 0.1821387 |  53.439418 | 0.2856759 | 0.9239095 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0537287 |   17.209594 | 0.0481125 |  14.978188 | 0.0698383 | 0.9949526 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3233414 |   74.181246 | 0.2895428 |  61.671130 | 0.3913795 | 0.9933272 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8679433 |  424.423250 | 0.7772179 | 108.955601 | 1.0064895 | 0.4255258 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1397077 |  419.606588 | 1.0205750 | 132.278635 | 1.4041842 | 0.0013151 |
| healthyR.data |        13 | BATS                       | Test  | 0.6885072 |  142.394618 | 0.6165381 | 123.701187 | 0.8515794 | 0.1008082 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7037551 |  170.781027 | 0.6301922 | 116.178054 | 0.8746871 | 0.0192878 |
| healthyR.data |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.8302894 |  332.812053 | 0.9724619 | 157.350782 | 0.9605890 | 0.1852286 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0422588 |   16.504530 | 0.0494949 |  19.220350 | 0.0503949 | 0.9983600 |
| healthyR      |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.6782668 |  290.579006 | 0.7944081 | 147.668367 | 0.8065090 | 0.0771209 |
| healthyR      |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0373127 |   10.815784 | 0.0437019 |  12.457556 | 0.0449777 | 0.9983731 |
| healthyR      |         7 | EARTH                      | Test  | 0.0169333 |    6.095367 | 0.0198328 |   6.259075 | 0.0200810 | 0.9995251 |
| healthyR      |         8 | NNAR                       | Test  | 0.0288805 |   13.401816 | 0.0338258 |  15.926946 | 0.0372523 | 0.9977422 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0434207 |   16.060275 | 0.0508557 |  18.568271 | 0.0513519 | 0.9971412 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3481096 |  172.662514 | 0.4077173 | 106.398528 | 0.4006325 | 0.9990623 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1198208 |  837.893442 | 1.3115706 | 135.963684 | 1.3138659 | 0.4075966 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0848541 |  802.209312 | 1.2706164 | 128.434652 | 1.4240105 | 0.0695732 |
| healthyR      |        13 | TBATS                      | Test  | 0.6848867 |  223.962216 | 0.8021616 | 157.274703 | 0.8186194 | 0.0258256 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6306790 |  120.855913 | 0.7386718 | 177.280698 | 0.8063335 | 0.0247437 |
| healthyR      |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7500939 |  297.249737 | 0.8196851 | 160.065238 | 0.8958643 | 0.3820285 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0488981 |   25.439387 | 0.0534347 |  18.528697 | 0.0593574 | 0.9969837 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.6955960 |  399.535857 | 0.7601311 | 127.072186 | 0.8827497 | 0.0000265 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0481104 |   26.033171 | 0.0525740 |  18.464171 | 0.0587230 | 0.9970548 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0149471 |    3.143418 | 0.0163338 |   3.087332 | 0.0274673 | 0.9990150 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1567609 |   82.769806 | 0.1713047 |  37.576751 | 0.1936896 | 0.9797871 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0526085 |   15.013397 | 0.0574893 |  16.809162 | 0.0663983 | 0.9955219 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4443553 |  261.104565 | 0.4855811 | 119.420341 | 0.4951469 | 0.9979418 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0092136 |  931.879016 | 1.1028450 | 115.903202 | 1.3069383 | 0.1792726 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.2933372 | 1061.953929 | 1.4133287 | 137.381652 | 1.6187909 | 0.0012665 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7249546 |  274.517132 | 0.7922134 | 146.553843 | 0.9257295 | 0.0016611 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.6213663 |  366.773393 | 0.6790145 | 116.178511 | 0.8088633 | 0.0851027 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.5451402 |  130.234172 | 0.5988579 | 128.263354 | 0.6977541 | 0.3102520 |
| healthyverse  |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.8920916 |  455.071320 | 0.9799976 | 119.216880 | 1.1447873 | 0.0229455 |
| healthyverse  |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0525254 |   21.072108 | 0.0577012 |  16.088704 | 0.0619498 | 0.9990258 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0212561 |    3.726443 | 0.0233506 |   3.804806 | 0.0402416 | 0.9978811 |
| healthyverse  |         8 | NNAR                       | Test  | 0.0936356 |   23.634752 | 0.1028624 |  23.504590 | 0.1283277 | 0.9933214 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0583409 |   17.699426 | 0.0640898 |  16.872657 | 0.0690998 | 0.9982299 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2460164 |   44.453303 | 0.2702587 |  60.152675 | 0.3023245 | 0.9972119 |
| healthyverse  |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0550954 |  342.740952 | 1.1590637 | 129.297221 | 1.4655715 | 0.0116544 |
| healthyverse  |        13 | TBATS                      | Test  | 0.6263814 |  172.845634 | 0.6881046 | 127.681992 | 0.8229346 | 0.0419005 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9069687 |  469.257203 | 0.9963407 | 121.007448 | 1.1295149 | 0.0162844 |
| healthyverse  |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.7013057 |  361.042403 | 0.9297888 | 145.883602 | 0.8289410 | 0.3309711 |
| healthyR.ai   |         2 | REGRESSION                 | Test  | 0.0486328 |   20.664356 | 0.0644772 |  15.637003 | 0.0556985 | 0.9990613 |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.5910885 |  309.335098 | 0.7836632 | 146.937290 | 0.7490024 | 0.0063808 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0497151 |   37.603647 | 0.0659121 |  21.038938 | 0.0631091 | 0.9988670 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0109103 |    3.159373 | 0.0144648 |   3.087279 | 0.0123258 | 0.9996795 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0618642 |   55.658334 | 0.0820194 |  31.324791 | 0.0709984 | 0.9947571 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0556318 |   35.308728 | 0.0737565 |  23.526070 | 0.0675589 | 0.9978482 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4002580 |  289.198682 | 0.5306607 | 111.297459 | 0.4400531 | 0.9936913 |
| healthyR.ai   |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9148836 |  936.772994 | 1.2129498 | 101.744991 | 1.1877757 | 0.2629182 |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0673495 |  947.974414 | 1.4150886 | 147.354749 | 1.3104582 | 0.0209704 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.5612559 |  260.308291 | 0.7441113 | 144.706774 | 0.7305611 | 0.0073729 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.5549612 |  128.128998 | 0.7357658 | 178.266232 | 0.6736769 | 0.0007980 |
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
    ## 1 health~         9 PROPHET W ~ Test   0.0537 17.2   0.0481 15.0   0.0698  0.995
    ## 2 health~         7 EARTH       Test   0.0169  6.10  0.0198  6.26  0.0201  1.00 
    ## 3 health~         7 EARTH       Test   0.0149  3.14  0.0163  3.09  0.0275  0.999
    ## 4 health~         7 EARTH       Test   0.0213  3.73  0.0234  3.80  0.0402  0.998
    ## 5 health~         7 EARTH       Test   0.0109  3.16  0.0145  3.09  0.0123  1.00 
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [415|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [407|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [356|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [332|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [146|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [22 x 6]> <tibble>     <split [0|22]>   <mdl_time_tbl>

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
