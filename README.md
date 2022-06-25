Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
25 June, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 39,971
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2022-06-23 22:22:44, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4949.74
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 39971         |
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
| r_version     |     26861 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     26861 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     26861 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       23 |          0 |
| country       |      3167 |          0.92 |   2 |   2 |     0 |      111 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-06-23 | 2021-11-02 |      578 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1434349.85 | 1808344.52 | 357 | 16872 | 274998 | 2989502 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8740.81 |   16795.27 |   1 |   174 |   2659 |    8628 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-06-23 22:22:44 | 2021-11-02 04:00:02 |    23253 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 18M 22S |       60 |

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

``` r
recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

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
  add_recipe(recipe = recipe_date) %>%
  add_model(model_spec_glm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [518|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [511|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [459|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [435|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [250|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [98|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0449175 |  13.9858469 | 0.0381983 |  11.3122207 | 0.0548202 | 0.9974758 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4892654 |  76.7122066 | 0.4160758 |  99.8320193 | 0.6066284 | 0.9994330 |
| healthyR.data |         3 | LM          | Test  | 0.0504176 |  16.0359539 | 0.0428756 |  12.4232962 | 0.0615320 | 0.9969074 |
| healthyR.data |         4 | EARTH       | Test  | 0.0248662 |   6.8120802 | 0.0211464 |   6.5212157 | 0.0342145 | 0.9987580 |
| healthyR.data |         5 | NNAR        | Test  | 0.0194103 |   3.5631217 | 0.0165067 |   3.5924920 | 0.0303697 | 0.9989435 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0468301 |   8.1134599 | 0.0338283 |   7.6668206 | 0.0632542 | 0.9966391 |
| healthyR      |         2 | GLMNET      | Test  | 0.5597765 |  69.9428962 | 0.4043611 | 105.4000319 | 0.6957701 | 0.9963814 |
| healthyR      |         3 | LM          | Test  | 0.0473558 |   8.1005006 | 0.0342080 |   7.5908990 | 0.0643118 | 0.9966248 |
| healthyR      |         4 | EARTH       | Test  | 0.0327942 |   4.4896221 | 0.0236893 |   4.3979940 | 0.0642450 | 0.9963814 |
| healthyR      |         5 | NNAR        | Test  | 0.0105000 |   2.8763762 | 0.0075848 |   2.6188077 | 0.0244924 | 0.9994366 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0493797 |  11.8658928 | 0.0349646 |   9.8748718 | 0.0592482 | 0.9984549 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5653588 |  68.2136125 | 0.4003175 | 103.6892242 | 0.6580832 | 0.9989124 |
| healthyR.ts   |         3 | LM          | Test  | 0.0508171 |  10.8732603 | 0.0359824 |   9.6340683 | 0.0613922 | 0.9984898 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0181671 |   2.4641718 | 0.0128637 |   2.4575753 | 0.0244427 | 0.9994769 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0084247 |   2.6361517 | 0.0059653 |   2.4809727 | 0.0109243 | 0.9999088 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0485696 |  13.4961217 | 0.0390754 |  11.2816925 | 0.0598991 | 0.9955154 |
| healthyverse  |         2 | GLMNET      | Test  | 0.5047070 | 108.1057274 | 0.4060488 | 116.7571181 | 0.6186933 | 0.9952413 |
| healthyverse  |         3 | LM          | Test  | 0.0485021 |  14.1662655 | 0.0390211 |  12.2475712 | 0.0604182 | 0.9954739 |
| healthyverse  |         4 | EARTH       | Test  | 0.0101783 |   2.1610286 | 0.0081887 |   2.1239864 | 0.0162487 | 0.9996780 |
| healthyverse  |         5 | NNAR        | Test  | 0.0026939 |   0.6621562 | 0.0021673 |   0.6682281 | 0.0040202 | 0.9999789 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0445163 |  12.1465928 | 0.0363939 |  12.0352171 | 0.0526116 | 0.9981647 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4934732 |  63.8160276 | 0.4034348 |  95.2913350 | 0.6017226 | 0.9981567 |
| healthyR.ai   |         3 | LM          | Test  | 0.0453946 |  12.0409357 | 0.0371120 |  12.6453962 | 0.0544071 | 0.9981599 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0149630 |   2.2316334 | 0.0122329 |   2.2134442 | 0.0180228 | 0.9997587 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0037686 |   0.5767535 | 0.0030810 |   0.5785632 | 0.0060330 | 0.9999600 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0385933 |   7.2567800 | 0.0352203 |   6.8704850 | 0.0448529 | 0.9984157 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4786718 |  63.2009983 | 0.4368365 |  92.4382846 | 0.5864344 | 0.9984644 |
| TidyDensity   |         3 | LM          | Test  | 0.0364586 |   6.5965448 | 0.0332722 |   6.3718707 | 0.0418813 | 0.9984011 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0093026 |   1.6553418 | 0.0084896 |   1.6314211 | 0.0131646 | 0.9998800 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0206028 |   4.6719916 | 0.0188021 |   4.8017420 | 0.0281872 | 0.9993991 |

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
    ##   # A tibble: 6 × 10
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy…         5 NNAR        Test  0.0194  3.56  0.0165  3.59  0.0304  0.999
    ## 2 healthyR         5 NNAR        Test  0.0105  2.88  0.00758 2.62  0.0245  0.999
    ## 3 healthy…         5 NNAR        Test  0.00842 2.64  0.00597 2.48  0.0109  1.00 
    ## 4 healthy…         5 NNAR        Test  0.00269 0.662 0.00217 0.668 0.00402 1.00 
    ## 5 healthy…         5 NNAR        Test  0.00377 0.577 0.00308 0.579 0.00603 1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00930 1.66  0.00849 1.63  0.0132  1.00

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
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = FALSE
    )
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [518|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [511|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [459|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [435|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [250|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [98|28]>  <mdl_time_tbl>

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
