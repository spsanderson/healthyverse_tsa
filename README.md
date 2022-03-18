Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 March, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 31,827
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

The last day in the data set is 2022-03-16 23:54:09, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -2575.26
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 31827         |
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
| r_version     |     21115 |          0.34 |   5 |   5 |     0 |       30 |          0 |
| r_arch        |     21115 |          0.34 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     21115 |          0.34 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       18 |          0 |
| country       |      2635 |          0.92 |   2 |   2 |     0 |      102 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-03-16 | 2021-09-12 |      479 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |        sd |  p0 |   p25 |    p50 |       p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|----------:|----:|------:|-------:|----------:|--------:|:------|
| size          |         0 |             1 | 1493446.52 | 1859085.5 | 357 | 16879 | 271098 | 3246529.0 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8368.42 |   16081.1 |   1 |   364 |   2687 |    8422.5 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-03-16 23:54:09 | 2021-09-12 13:18:56 |    18526 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 0M 12S |       60 |

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
    ## 1 healthyR.data <tibble>          <tibble>     <split [420|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [412|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [361|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [337|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [151|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [27 x 6]> <tibble>     <split [0|27]>   <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc                | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA                      | Test  | 1.0468875 |  243.248920 | 0.8132504 | 147.938504 | 1.2659753 | 0.2506695 |
| healthyR.data |         2 | REGRESSION                 | Test  | 0.0655905 |   20.843779 | 0.0509525 |  16.127207 | 0.0803419 | 0.9937036 |
| healthyR.data |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | ETSANN                     | Test  | 0.7875485 |  138.125073 | 0.6117889 | 134.933110 | 0.9453453 |        NA |
| healthyR.data |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |         6 | LM                         | Test  | 0.0472320 |    9.501564 | 0.0366911 |   8.440034 | 0.0593849 | 0.9962163 |
| healthyR.data |         7 | EARTH                      | Test  | 0.2043099 |   99.423749 | 0.1587134 |  36.173026 | 0.3137376 | 0.9294348 |
| healthyR.data |         8 | NNAR                       | Test  | 0.0931622 |   22.683113 | 0.0723709 |  21.076914 | 0.1193300 | 0.9870052 |
| healthyR.data |         9 | PROPHET W REGRESSORS       | Test  | 0.0529974 |   10.093285 | 0.0411698 |  10.770355 | 0.0656629 | 0.9952518 |
| healthyR.data |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3620291 |   86.329431 | 0.2812340 |  60.335567 | 0.4463790 | 0.9931261 |
| healthyR.data |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.9644482 |  405.747946 | 0.7492093 | 120.426873 | 1.1451922 | 0.3104469 |
| healthyR.data |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3136373 |  450.804436 | 1.0204689 | 154.776234 | 1.5435231 | 0.0015746 |
| healthyR.data |        13 | TBATS                      | Test  | 0.8201664 |  131.052163 | 0.6371274 | 152.204839 | 0.9600319 | 0.0000621 |
| healthyR.data |        14 | THETA METHOD               | Test  | 0.7855387 |  139.521427 | 0.6102277 | 133.397012 | 0.9437901 | 0.0003723 |
| healthyR.data |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         1 | ARIMA                      | Test  | 0.6905160 |  160.864384 | 0.6122557 | 144.507097 | 0.9035779 | 0.2794839 |
| healthyR      |         2 | REGRESSION                 | Test  | 0.0457799 |   13.348726 | 0.0405914 |  16.278319 | 0.0660985 | 0.9948642 |
| healthyR      |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | ETSANA                     | Test  | 0.7367652 |  294.274310 | 0.6532632 | 159.113078 | 0.8879343 | 0.0589156 |
| healthyR      |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |         6 | LM                         | Test  | 0.0436053 |   10.593734 | 0.0386632 |  10.545307 | 0.0656298 | 0.9948807 |
| healthyR      |         7 | EARTH                      | Test  | 0.0113735 |    2.884269 | 0.0100845 |   2.753738 | 0.0234280 | 0.9994122 |
| healthyR      |         8 | NNAR                       | Test  | 0.0319537 |    7.206522 | 0.0283322 |   7.706964 | 0.0490817 | 0.9971921 |
| healthyR      |         9 | PROPHET W REGRESSORS       | Test  | 0.0518012 |   14.540358 | 0.0459302 |  15.386095 | 0.0741512 | 0.9934299 |
| healthyR      |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3585054 |  133.413623 | 0.3178739 |  94.259603 | 0.4709279 | 0.9828927 |
| healthyR      |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.1247845 |  763.522258 | 0.9973060 | 133.430676 | 1.3311304 | 0.4646109 |
| healthyR      |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1456433 |  875.014724 | 1.0158007 | 129.637003 | 1.4024509 | 0.0981435 |
| healthyR      |        13 | TBATS                      | Test  | 0.7238778 |  273.877115 | 0.6418364 | 159.220606 | 0.8874594 | 0.0562807 |
| healthyR      |        14 | THETA METHOD               | Test  | 0.6884711 |  167.465441 | 0.6104426 | 141.751716 | 0.9154977 | 0.0000027 |
| healthyR      |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA                      | Test  | 0.7374668 |  280.931032 | 0.6774044 | 145.852559 | 0.9264820 | 0.2821605 |
| healthyR.ts   |         2 | REGRESSION                 | Test  | 0.0539249 |   32.327043 | 0.0495330 |  20.116594 | 0.0677178 | 0.9966741 |
| healthyR.ts   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | ETSANA                     | Test  | 0.7880824 |  405.479462 | 0.7238976 | 134.494212 | 0.9677491 | 0.0077875 |
| healthyR.ts   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         6 | LM                         | Test  | 0.0533480 |   33.120733 | 0.0490031 |  20.108349 | 0.0678679 | 0.9967877 |
| healthyR.ts   |         7 | EARTH                      | Test  | 0.0163318 |    3.708521 | 0.0150017 |   3.602097 | 0.0279690 | 0.9990610 |
| healthyR.ts   |         8 | NNAR                       | Test  | 0.1067823 |   42.458405 | 0.0980855 |  25.085596 | 0.1529933 | 0.9899678 |
| healthyR.ts   |         9 | PROPHET W REGRESSORS       | Test  | 0.0565859 |   19.774342 | 0.0519773 |  16.716302 | 0.0722629 | 0.9954667 |
| healthyR.ts   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3693496 |  184.073291 | 0.3392681 |  97.454415 | 0.4395215 | 0.9989937 |
| healthyR.ts   |        11 | SEASONAL DECOMP REGRESSION | Test  | 1.0326811 |  877.683146 | 0.9485751 | 121.308707 | 1.3369422 | 0.2108142 |
| healthyR.ts   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.3999283 | 1010.301507 | 1.2859122 | 146.696538 | 1.6971072 | 0.0010031 |
| healthyR.ts   |        13 | TBATS                      | Test  | 0.7733832 |  380.133376 | 0.7103955 | 158.656043 | 0.9523898 | 0.0008852 |
| healthyR.ts   |        14 | THETA METHOD               | Test  | 0.7634128 |  483.039796 | 0.7012372 | 121.461562 | 0.9534543 | 0.0086582 |
| healthyR.ts   |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         1 | ARIMA                      | Test  | 0.6170906 |  118.833265 | 0.5221692 | 132.991199 | 0.8411226 | 0.2613898 |
| healthyverse  |         2 | REGRESSION                 | Test  | 0.0649468 |   25.713736 | 0.0549566 |  19.900109 | 0.0814219 | 0.9948634 |
| healthyverse  |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | ETSANA                     | Test  | 0.9649870 |  386.352146 | 0.8165519 | 128.745969 | 1.1837626 | 0.0358029 |
| healthyverse  |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |         6 | LM                         | Test  | 0.0654244 |   25.945340 | 0.0553607 |  20.192732 | 0.0825865 | 0.9948122 |
| healthyverse  |         7 | EARTH                      | Test  | 0.0202730 |    3.136656 | 0.0171546 |   3.195541 | 0.0386789 | 0.9983274 |
| healthyverse  |         8 | NNAR                       | Test  | 0.0543259 |   20.099069 | 0.0459694 |  20.340779 | 0.0681744 | 0.9974370 |
| healthyverse  |         9 | PROPHET W REGRESSORS       | Test  | 0.0726151 |   21.740596 | 0.0614454 |  20.710230 | 0.0914049 | 0.9932119 |
| healthyverse  |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.2725792 |   42.881890 | 0.2306509 |  56.745775 | 0.3743475 | 0.9904026 |
| healthyverse  |        11 | SEASONAL DECOMP REGRESSION | Test  | 0.8066595 |  325.630811 | 0.6825785 | 116.053732 | 0.9367791 | 0.4227900 |
| healthyverse  |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.1141578 |  260.211812 | 0.9427772 | 129.263116 | 1.4707306 | 0.0034843 |
| healthyverse  |        13 | TBATS                      | Test  | 0.7545260 |  167.980636 | 0.6384642 | 126.829804 | 0.9926159 | 0.0006512 |
| healthyverse  |        14 | THETA METHOD               | Test  | 0.9773088 |  413.772633 | 0.8269784 | 130.178654 | 1.1677096 | 0.1163873 |
| healthyverse  |        15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         1 | ARIMA                      | Test  | 0.6756610 |  275.117707 | 0.7101017 | 141.230817 | 0.8672457 | 0.1260050 |
| healthyR.ai   |         2 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | ETSANA                     | Test  | 0.6703593 |  327.815534 | 0.7045298 | 152.834829 | 0.8181912 | 0.0090121 |
| healthyR.ai   |         5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         6 | LM                         | Test  | 0.0545549 |   40.636495 | 0.0573357 |  22.113032 | 0.0691136 | 0.9975947 |
| healthyR.ai   |         7 | EARTH                      | Test  | 0.0121206 |    3.338021 | 0.0127385 |   3.251787 | 0.0142738 | 0.9996937 |
| healthyR.ai   |         8 | NNAR                       | Test  | 0.0163291 |   11.790274 | 0.0171615 |  10.095878 | 0.0205146 | 0.9993558 |
| healthyR.ai   |         9 | PROPHET W REGRESSORS       | Test  | 0.0586119 |   31.531407 | 0.0615996 |  20.953943 | 0.0712872 | 0.9959914 |
| healthyR.ai   |        10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3740270 |  223.536609 | 0.3930924 |  95.506308 | 0.4368520 | 0.9936902 |
| healthyR.ai   |        11 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |        12 | SEASONAL DECOMP ETSANN     | Test  | 1.0838120 |  980.560149 | 1.1390576 | 139.960150 | 1.3501792 | 0.0269159 |
| healthyR.ai   |        13 | TBATS                      | Test  | 0.6361556 |  268.096930 | 0.6685826 | 142.528080 | 0.7970956 | 0.0066041 |
| healthyR.ai   |        14 | THETA METHOD               | Test  | 0.6161615 |  215.421991 | 0.6475693 | 154.440610 | 0.7537452 | 0.0486329 |
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
    ## 1 health~         6 LM          Test   0.0472  9.50  0.0367  8.44  0.0594  0.996
    ## 2 health~         7 EARTH       Test   0.0114  2.88  0.0101  2.75  0.0234  0.999
    ## 3 health~         7 EARTH       Test   0.0163  3.71  0.0150  3.60  0.0280  0.999
    ## 4 health~         7 EARTH       Test   0.0203  3.14  0.0172  3.20  0.0387  0.998
    ## 5 health~         7 EARTH       Test   0.0121  3.34  0.0127  3.25  0.0143  1.00 
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
    ## 1 healthyR.data <tibble>          <tibble>     <split [420|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [412|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [361|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [337|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [151|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [27 x 6]> <tibble>     <split [0|27]>   <mdl_time_tbl>

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
