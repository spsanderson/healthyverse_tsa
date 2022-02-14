Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
14 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,059
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

The last day in the data set is 2022-02-12 21:22:25, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1804.73
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29059          |
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
| r\_version     |      19293 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19293 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19293 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2417 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-12 | 2021-08-25 |       447 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1520924.30 | 1872419.74 | 357 | 23405.5 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8242.48 |   15707.66 |   1 |   328.0 |   2802 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-12 21:22:25 | 2021-08-25 16:31:43 |     16971 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 56M 40S |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [390|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [380|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [330|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [305|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [119|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9422655 |  2149.112665 | 0.7150113 | 126.812730 | 1.2328909 | 0.0607545 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0620633 |   257.143992 | 0.0470949 |  16.991880 | 0.0840258 | 0.9935864 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8929628 |    99.525297 | 0.6775993 | 194.340214 | 1.0363981 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0643478 |   210.073805 | 0.0488285 |  16.677076 | 0.0947189 | 0.9927751 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0644502 |   136.093408 | 0.0489062 |  17.106242 | 0.1397907 | 0.9813494 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0198911 |   134.699556 | 0.0150938 |   9.602515 | 0.0313801 | 0.9990407 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0714816 |   193.108161 | 0.0542418 |  18.133417 | 0.1051844 | 0.9908451 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5001916 |  1828.798418 | 0.3795561 |  77.313012 | 0.6274170 | 0.9888265 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2449814 |  8186.802685 | 0.9447187 | 117.619914 | 1.5136572 | 0.3032151 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6128912 |  9690.411450 | 1.2238965 | 145.274944 | 1.8744816 | 0.0000139 |
| healthyR.data |         13 | BATS                       | Test  | 0.9257395 |  1637.294677 | 0.7024710 | 129.858776 | 1.2049717 | 0.0028961 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8931596 |   110.160301 | 0.6777487 | 199.779975 | 1.0355035 | 0.0142070 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7314103 |  2391.304283 | 0.8235674 | 140.939610 | 0.9516650 | 0.3871771 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0592817 |   203.350892 | 0.0667512 |  22.151055 | 0.0725603 | 0.9949022 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5974457 |   872.378690 | 0.6727234 | 107.339778 | 0.8464429 | 0.2183929 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0563202 |   155.267466 | 0.0634165 |  20.691931 | 0.0697655 | 0.9948847 |
| healthyR      |          7 | EARTH                      | Test  | 0.0346220 |    58.575094 | 0.0389843 |  17.930719 | 0.0796169 | 0.9930101 |
| healthyR      |          8 | NNAR                       | Test  | 0.0098679 |     8.945875 | 0.0111112 |   9.531493 | 0.0208408 | 0.9996897 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0644757 |   336.416053 | 0.0725996 |  22.546433 | 0.0780910 | 0.9936708 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4778580 |  2560.291702 | 0.5380678 | 109.577919 | 0.5851349 | 0.9747725 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0624891 |  1257.408527 | 1.1963619 | 107.747252 | 1.3678905 | 0.5259244 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1119120 |  1835.628067 | 1.2520120 | 131.209563 | 1.3914862 | 0.1258672 |
| healthyR      |         13 | TBATS                      | Test  | 0.6007546 |   795.222515 | 0.6764492 | 106.265569 | 0.8459375 | 0.2710692 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7370570 |  1483.720080 | 0.8299256 | 146.853774 | 0.9479787 | 0.0566629 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.3468875 |  7710.765690 | 1.4006579 | 162.107257 | 1.5531459 | 0.3712684 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0765595 |   551.923090 | 0.0796159 |  22.874921 | 0.0959935 | 0.9909435 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.3550519 |  8116.872963 | 1.4091483 | 165.198428 | 1.5572247 | 0.1721624 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0727337 |   492.360916 | 0.0756374 |  23.549511 | 0.0891484 | 0.9912406 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0152274 |    14.281463 | 0.0158353 |   8.807668 | 0.0216070 | 0.9994899 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0175627 |   103.768000 | 0.0182638 |  16.556576 | 0.0351467 | 0.9992518 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0810558 |   528.420289 | 0.0842917 |  24.752793 | 0.1000625 | 0.9894896 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7635589 |  5066.844359 | 0.7940417 | 138.324574 | 0.8558994 | 0.9736011 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0516926 |  2650.080326 | 1.0936782 | 100.117722 | 1.4262651 | 0.5302631 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5029973 | 11225.650077 | 1.5629999 | 140.969985 | 1.9285260 | 0.1747451 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0133960 |  4832.402884 | 1.0538528 | 144.789522 | 1.2768825 | 0.2242259 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.3437986 |  7575.523177 | 1.3974457 | 157.633746 | 1.6191476 | 0.0230261 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9253103 |   128.312050 | 0.9042212 | 135.716759 | 1.1801816 | 0.1064687 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0635993 |     8.178904 | 0.0621498 |   7.959655 | 0.0888134 | 0.9907892 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7927257 |   198.956643 | 0.7746584 | 108.227480 | 0.9250916 | 0.0037546 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0608875 |     8.294630 | 0.0594998 |   8.138866 | 0.0905431 | 0.9907654 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0198382 |     1.983672 | 0.0193861 |   1.990744 | 0.0435344 | 0.9987932 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0184564 |     4.482933 | 0.0180357 |   3.276725 | 0.0418594 | 0.9986518 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0994691 |    21.957292 | 0.0972021 |  20.888001 | 0.1109885 | 0.9905881 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5626661 |    79.558066 | 0.5498421 |  93.753664 | 0.6909087 | 0.9667532 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1403879 |   287.525630 | 1.1143969 | 108.048165 | 1.3690947 | 0.2715097 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3599496 |   344.150485 | 1.3289544 | 135.962325 | 1.6431886 | 0.0016811 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9571608 |   138.983540 | 0.9353457 | 143.138724 | 1.1759874 | 0.0102696 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7147459 |   166.167451 | 0.6984559 |  92.262143 | 0.9067843 | 0.0382756 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8634045 |  3208.453869 | 0.8022346 | 153.991307 | 1.0425140 | 0.3901220 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0704975 |   415.555685 | 0.0655030 |  26.812045 | 0.0871000 | 0.9932330 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9380670 |  3522.194130 | 0.8716074 | 156.684446 | 1.1218128 | 0.0810450 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0638541 |   206.472263 | 0.0593302 |  26.734640 | 0.0834790 | 0.9931520 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0168950 |     6.531469 | 0.0156980 |   8.348745 | 0.0299857 | 0.9990850 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0362461 |   108.423506 | 0.0336781 |  18.877114 | 0.0687722 | 0.9964132 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0791340 |   672.468933 | 0.0735276 |  30.120751 | 0.0974372 | 0.9915440 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.1144850 |  9447.836751 | 1.0355267 | 162.312974 | 1.2065054 | 0.8346724 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1117550 |  2543.952093 | 1.0329901 | 127.925984 | 1.4366732 | 0.4317684 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2390016 |  2150.194945 | 1.1512217 | 142.542459 | 1.6706136 | 0.0989639 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9164634 |  3806.523223 | 0.8515345 | 156.222996 | 1.0848009 | 0.1285084 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9787778 |  3926.150267 | 0.9094340 | 153.460883 | 1.2213321 | 0.0642741 |
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
    ##   package   .model_id .model_desc .type     mae   mape   mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.0199  135.   0.0151  9.60 0.0314 0.999
    ## 2 healthyR          8 NNAR        Test  0.00987   8.95 0.0111  9.53 0.0208 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0152   14.3  0.0158  8.81 0.0216 0.999
    ## 4 healthyv~         8 NNAR        Test  0.0185    4.48 0.0180  3.28 0.0419 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0169    6.53 0.0157  8.35 0.0300 0.999

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [390|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [380|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [330|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [305|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [119|28]> <mdl_time_tbl>

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
