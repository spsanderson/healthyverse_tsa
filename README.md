Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 June, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 38,553
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

The last day in the data set is 2022-06-03 23:43:47, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4471.09
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 38553         |
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
| r_version     |     25911 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25911 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25911 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3075 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-06-03 | 2021-10-27 |      558 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1443878.35 | 1820168.40 | 357 | 16757 | 271502 | 3200887 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8719.49 |   16732.76 |   1 |   186 |   2659 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-06-03 23:43:47 | 2021-10-27 16:07:39 |    22426 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 13M 52S |       60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [499|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [491|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [440|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [416|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [230|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [78|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|------------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0478997 | 13.0260245 | 0.0310146 |  12.6545641 | 0.0585536 | 0.9969109 |
| healthyR.data |         2 | GLMNET      | Test  | 0.5519340 | 85.0996551 | 0.3573724 | 110.0474540 | 0.6695646 | 0.9982085 |
| healthyR.data |         3 | LM          | Test  | 0.0530786 | 16.9162848 | 0.0343679 |  12.5947247 | 0.0657772 | 0.9957201 |
| healthyR.data |         4 | EARTH       | Test  | 0.0185547 |  4.2132254 | 0.0120140 |   4.3578646 | 0.0328975 | 0.9990335 |
| healthyR.data |         5 | NNAR        | Test  | 0.0564839 |  3.9481092 | 0.0365728 |   3.6528000 | 0.1957255 | 0.9646905 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0483108 | 13.1922579 | 0.0337416 |  13.5706938 | 0.0652328 | 0.9962659 |
| healthyR      |         2 | GLMNET      | Test  | 0.5767215 | 73.0304137 | 0.4027992 | 111.6226034 | 0.7420524 | 0.9958823 |
| healthyR      |         3 | LM          | Test  | 0.0487400 | 13.5444947 | 0.0340415 |  14.2029395 | 0.0653852 | 0.9962732 |
| healthyR      |         4 | EARTH       | Test  | 0.0274918 |  2.9477121 | 0.0192011 |   2.8744110 | 0.0703713 | 0.9964399 |
| healthyR      |         5 | NNAR        | Test  | 0.0223726 |  5.4699593 | 0.0156257 |   5.8443709 | 0.0517600 | 0.9981976 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0402849 |  7.1723434 | 0.0263051 |   6.5644505 | 0.0511323 | 0.9983225 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5738582 | 62.9976191 | 0.3747165 |  93.0925186 | 0.6929707 | 0.9980388 |
| healthyR.ts   |         3 | LM          | Test  | 0.0407337 |  7.1902550 | 0.0265982 |   6.7057455 | 0.0507011 | 0.9983346 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0173100 |  2.4426378 | 0.0113031 |   2.4087742 | 0.0251011 | 0.9995585 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0113144 |  2.2941262 | 0.0073880 |   2.1866213 | 0.0194407 | 0.9997067 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0422836 |  7.8709967 | 0.0338240 |   7.3504216 | 0.0511226 | 0.9977417 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4663077 | 89.0604168 | 0.3730153 |  98.5937796 | 0.5771772 | 0.9978768 |
| healthyverse  |         3 | LM          | Test  | 0.0411441 |  7.5175959 | 0.0329126 |   7.1365103 | 0.0493327 | 0.9977519 |
| healthyverse  |         4 | EARTH       | Test  | 0.0114319 |  2.7126017 | 0.0091448 |   2.6866681 | 0.0160639 | 0.9996487 |
| healthyverse  |         5 | NNAR        | Test  | 0.0072451 |  1.0707884 | 0.0057956 |   1.0708234 | 0.0175856 | 0.9996263 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0372371 |  8.2085088 | 0.0253314 |   8.0225848 | 0.0454755 | 0.9986097 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.5237677 | 62.0215263 | 0.3563062 |  91.5257362 | 0.6495657 | 0.9986446 |
| healthyR.ai   |         3 | LM          | Test  | 0.0377591 |  8.5670263 | 0.0256866 |   8.4908843 | 0.0457851 | 0.9986109 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0169872 |  2.2519951 | 0.0115560 |   2.2252269 | 0.0224801 | 0.9996826 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0039674 |  0.6545476 | 0.0026989 |   0.6559614 | 0.0069972 | 0.9999561 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0398011 | 13.7704011 | 0.0391331 |  12.0964353 | 0.0497525 | 0.9991918 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.3509714 | 60.2451129 | 0.3450805 |  87.8729901 | 0.4291672 | 0.9994516 |
| TidyDensity   |         3 | LM          | Test  | 0.0378008 | 12.5597334 | 0.0371663 |  10.9260956 | 0.0461973 | 0.9991173 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0070311 |  1.1011375 | 0.0069131 |   1.0983046 | 0.0124582 | 0.9996866 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0490329 | 15.1805113 | 0.0482099 |  13.4335214 | 0.0640643 | 0.9921820 |

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
    ## 1 healthy…         4 EARTH       Test  0.0186  4.21  0.0120  4.36  0.0329  0.999
    ## 2 healthyR         5 NNAR        Test  0.0224  5.47  0.0156  5.84  0.0518  0.998
    ## 3 healthy…         5 NNAR        Test  0.0113  2.29  0.00739 2.19  0.0194  1.00 
    ## 4 healthy…         4 EARTH       Test  0.0114  2.71  0.00914 2.69  0.0161  1.00 
    ## 5 healthy…         5 NNAR        Test  0.00397 0.655 0.00270 0.656 0.00700 1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00703 1.10  0.00691 1.10  0.0125  1.00

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [499|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [491|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [440|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [416|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [230|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [78|28]>  <mdl_time_tbl>

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
