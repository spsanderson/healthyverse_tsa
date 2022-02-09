Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
09 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,547
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

The last day in the data set is 2022-02-07 22:02:49, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1685.41
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28547          |
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
| r\_version     |      18899 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18899 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18899 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2394 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-07 | 2021-08-23 |       442 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1526819.8 | 1874643.24 | 357 | 26403.5 | 271098 | 3247952 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8266.6 |   15781.85 |   1 |   277.0 |   2806 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-07 22:02:49 | 2021-08-23 13:19:57 |     16747 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 53S |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [385|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [375|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [325|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [300|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [114|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1671844 |  789.7903641 | 0.7592130 | 142.004683 | 1.4189895 | 0.1644677 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0531404 |   74.1507900 | 0.0345660 |  13.355294 | 0.0643363 | 0.9963083 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0873359 |  512.2078552 | 0.7072743 | 146.925477 | 1.3163086 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0531711 |   59.1143016 | 0.0345860 |  12.822810 | 0.0685927 | 0.9958377 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0340263 |   35.8527806 | 0.0221329 |  10.502795 | 0.0457459 | 0.9982100 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0116403 |    2.9102221 | 0.0075716 |   2.600518 | 0.0201046 | 0.9995893 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0623561 |   55.6073486 | 0.0405605 |  14.681939 | 0.0838009 | 0.9939645 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6635773 |  689.0064426 | 0.4316340 |  98.764943 | 0.7628117 | 0.9926332 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0669032 | 2475.3815754 | 0.6939835 |  86.517839 | 1.4108070 | 0.4137035 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5181763 | 2382.4934592 | 0.9875211 | 141.159644 | 1.8759086 | 0.0204467 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0731059 |  463.3256282 | 0.6980182 | 137.536601 | 1.3359290 | 0.1907510 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0867353 |  509.4999918 | 0.7068837 | 147.022745 | 1.3152548 | 0.0475240 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8161270 | 1508.1662368 | 0.9362138 | 168.595479 | 1.0055048 | 0.5085198 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0563773 |   68.6634364 | 0.0646728 |  25.057110 | 0.0702070 | 0.9936046 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6020534 |  818.8356552 | 0.6906410 | 127.028270 | 0.7762130 | 0.2140495 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0522586 |   63.1187265 | 0.0599480 |  23.061400 | 0.0662339 | 0.9936641 |
| healthyR      |          7 | EARTH                      | Test  | 0.0290836 |   23.9209561 | 0.0333630 |  24.857832 | 0.0691798 | 0.9932872 |
| healthyR      |          8 | NNAR                       | Test  | 0.0051538 |    7.9671455 | 0.0059122 |   8.698654 | 0.0097783 | 0.9998872 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0598377 |   96.6750850 | 0.0686424 |  23.533669 | 0.0741262 | 0.9922843 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5049802 | 1175.9606379 | 0.5792842 | 123.055301 | 0.5970136 | 0.9667541 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1137559 | 2046.3300446 | 1.2776366 | 110.390376 | 1.3668523 | 0.5832947 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 0.9854753 | 2266.8766909 | 1.1304804 | 122.823187 | 1.2412010 | 0.2710686 |
| healthyR      |         13 | TBATS                      | Test  | 0.5497941 |  380.2041050 | 0.6306921 | 118.844544 | 0.7336560 | 0.3620840 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6511078 |  723.1176218 | 0.7469133 | 140.826403 | 0.8372149 | 0.1011538 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1724980 | 1034.0452760 | 0.9566960 | 140.886642 | 1.4382746 | 0.6590086 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0793038 |  135.7609430 | 0.0647077 |  15.613211 | 0.0983900 | 0.9939995 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1792469 | 1027.3504160 | 0.9622027 | 150.035611 | 1.4083060 | 0.2042934 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0756442 |   95.5649102 | 0.0617217 |  16.090448 | 0.0911983 | 0.9941712 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0293022 |    3.6227444 | 0.0239090 |   3.453384 | 0.0676685 | 0.9970062 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0495734 |    6.6635862 | 0.0404493 |   5.346197 | 0.1868735 | 0.9818133 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0832542 |  128.6567036 | 0.0679310 |  16.635714 | 0.1030017 | 0.9929539 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6280242 |  670.0342166 | 0.5124343 | 111.959232 | 0.7478428 | 0.9824204 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1292387 |  629.7166440 | 0.9213987 | 107.227578 | 1.4109241 | 0.6288879 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3828342 | 1700.0591623 | 1.1283191 | 126.862540 | 1.7276885 | 0.2301083 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9944110 |  685.1497624 | 0.8113865 | 139.367161 | 1.2402133 | 0.4458733 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1568777 |  822.7189003 | 0.9439506 | 147.900888 | 1.4264915 | 0.3910011 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1663050 |  124.0964713 | 1.0417253 | 164.706114 | 1.3914703 | 0.3545044 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0701417 |    8.4846521 | 0.0626494 |   8.090523 | 0.0909653 | 0.9909344 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9317397 |  122.2779657 | 0.8322153 | 134.682441 | 1.1118536 | 0.0430966 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0658769 |    7.8873921 | 0.0588402 |   7.497601 | 0.0919635 | 0.9909257 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0216547 |    1.9440138 | 0.0193416 |   1.962702 | 0.0439368 | 0.9988315 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0130229 |    0.7081256 | 0.0116319 |   0.716150 | 0.0323927 | 0.9993103 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1147744 |   22.5905356 | 0.1025147 |  24.553518 | 0.1251356 | 0.9894918 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6669603 |   84.5492994 | 0.5957184 | 100.161922 | 0.7837399 | 0.9650096 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2255934 |  281.1368122 | 1.0946807 | 105.694581 | 1.3728841 | 0.3775582 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4191134 |  278.5751204 | 1.2675298 | 140.074453 | 1.6616889 | 0.0481005 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9829404 |  102.9903782 | 0.8779469 | 140.721576 | 1.2022978 | 0.0441191 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8918490 |  107.9204095 | 0.7965855 | 115.081594 | 1.1054632 | 0.2438711 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2035627 | 2378.8759345 | 0.9943333 | 160.702036 | 1.3796188 | 0.4327292 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0661138 |  118.0066944 | 0.0546205 |  25.591984 | 0.0821358 | 0.9939350 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 1.0070423 | 1282.2331728 | 0.8319764 | 164.634142 | 1.1464657 | 0.0889488 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0568315 |   56.4974885 | 0.0469518 |  22.656122 | 0.0734292 | 0.9943307 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0130502 |    6.4840730 | 0.0107816 |   8.863490 | 0.0185949 | 0.9995993 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0623505 |   57.5863877 | 0.0515114 |  28.708019 | 0.0900542 | 0.9944322 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0710901 |  149.6708915 | 0.0587317 |  28.210370 | 0.0889919 | 0.9923773 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7028667 | 1757.6008551 | 0.5806791 | 138.424918 | 0.7973482 | 0.9557716 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1969108 |  853.8308376 | 0.9888378 | 133.322625 | 1.4562976 | 0.4491243 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3409558 | 1865.6975060 | 1.1078418 | 137.211066 | 1.6973757 | 0.1129124 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9742995 | 1786.4732767 | 0.8049256 | 165.622480 | 1.1149954 | 0.2648527 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0205593 | 1147.1040094 | 0.8431435 | 163.107904 | 1.2263592 | 0.1750586 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |

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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0116  2.91  0.00757 2.60  0.0201  1.00 
    ## 2 healthyR         8 NNAR        Test  0.00515 7.97  0.00591 8.70  0.00978 1.00 
    ## 3 healthy~         7 EARTH       Test  0.0293  3.62  0.0239  3.45  0.0677  0.997
    ## 4 healthy~         8 NNAR        Test  0.0130  0.708 0.0116  0.716 0.0324  0.999
    ## 5 healthy~         7 EARTH       Test  0.0131  6.48  0.0108  8.86  0.0186  1.00

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [385|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [375|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [325|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [300|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [114|28]> <mdl_time_tbl>

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
