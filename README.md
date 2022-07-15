Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,674
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

The last day in the data set is 2022-07-13 22:10:04, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -258.2 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41674         |
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
| r_version     |     27996 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27996 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27996 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3307 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-13 | 2021-11-08 |      598 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1419033.81 | 1792425.88 | 357 | 16873 | 289680.0 | 2974403 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8742.53 |   16740.67 |   1 |   174 |   2635.5 |    8827 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-13 22:10:04 | 2021-11-08 05:44:37 |    24347 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     51 |       60 |

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
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [270|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [118|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0537363 |  10.830918 | 0.0376986 |  10.029231 | 0.0678654 | 0.9967650 |
| healthyR      |         2 | GLMNET      | Test  | 0.6152082 |  66.687908 | 0.4315982 | 102.522453 | 0.8046674 | 0.9967928 |
| healthyR      |         3 | LM          | Test  | 0.0559115 |  12.922422 | 0.0392246 |  12.356963 | 0.0672096 | 0.9967604 |
| healthyR      |         4 | EARTH       | Test  | 0.0480337 |   4.623488 | 0.0336980 |   4.612783 | 0.0892082 | 0.9967928 |
| healthyR      |         5 | NNAR        | Test  | 0.0240955 |   2.261530 | 0.0169042 |   2.265720 | 0.0692169 | 0.9967275 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0485020 |  27.275852 | 0.0411131 |  15.045534 | 0.0605766 | 0.9966559 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4541660 | 121.377754 | 0.3849770 |  93.510547 | 0.5802836 | 0.9987141 |
| healthyR.data |         3 | LM          | Test  | 0.0475419 |  25.377621 | 0.0402992 |  14.587189 | 0.0594199 | 0.9966588 |
| healthyR.data |         4 | EARTH       | Test  | 0.0221635 |  19.106465 | 0.0187870 |   8.988562 | 0.0304189 | 0.9987141 |
| healthyR.data |         5 | NNAR        | Test  | 0.0091118 |  28.880596 | 0.0077236 |   8.324312 | 0.0132677 | 0.9997878 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0574429 | 139.808722 | 0.0492482 |  31.832074 | 0.0667761 | 0.9974881 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5420663 | 245.527718 | 0.4647363 | 100.558723 | 0.6820269 | 0.9981903 |
| healthyR.ts   |         3 | LM          | Test  | 0.0614097 | 192.675560 | 0.0526491 |  33.436511 | 0.0683759 | 0.9974230 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0288515 |  19.217462 | 0.0247356 |  13.248996 | 0.0487740 | 0.9981903 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0049514 |   7.271241 | 0.0042450 |   7.919819 | 0.0072295 | 0.9999695 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0399660 |  67.017763 | 0.0342735 |  22.485996 | 0.0531273 | 0.9961684 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4643312 | 382.999748 | 0.3981947 | 110.195422 | 0.6224631 | 0.9958807 |
| healthyverse  |         3 | LM          | Test  | 0.0460539 |  80.404784 | 0.0394943 |  22.204066 | 0.0568411 | 0.9962300 |
| healthyverse  |         4 | EARTH       | Test  | 0.0100731 |   9.517502 | 0.0086384 |   7.244085 | 0.0150661 | 0.9997253 |
| healthyverse  |         5 | NNAR        | Test  | 0.0044603 |   2.450464 | 0.0038250 |   2.434564 | 0.0097262 | 0.9998733 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0444947 |  33.281168 | 0.0421446 |  14.424884 | 0.0533749 | 0.9968011 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4798181 | 105.898710 | 0.4544752 |  99.688008 | 0.6061352 | 0.9970110 |
| healthyR.ai   |         3 | LM          | Test  | 0.0447878 |  35.088644 | 0.0424222 |  14.841597 | 0.0533529 | 0.9968036 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0121530 |   4.625014 | 0.0115111 |   5.859241 | 0.0143838 | 0.9997756 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0058913 |   1.565261 | 0.0055802 |   1.518592 | 0.0135054 | 0.9997850 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0282557 |   9.899118 | 0.0230690 |   7.961014 | 0.0331803 | 0.9987682 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.5009785 | 184.331646 | 0.4090174 | 106.058214 | 0.6491734 | 0.9988533 |
| TidyDensity   |         3 | LM          | Test  | 0.0295776 |  20.897951 | 0.0241483 |  12.550079 | 0.0340467 | 0.9987475 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0090861 |   2.783615 | 0.0074182 |   2.645823 | 0.0121882 | 0.9998553 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0066529 |   7.108382 | 0.0054317 |  10.500712 | 0.0097038 | 0.9999184 |

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
    ## 1 healthyR         3 LM          Test  0.0559  12.9  0.0392  12.4  0.0672  0.997
    ## 2 healthy…         5 NNAR        Test  0.00911 28.9  0.00772  8.32 0.0133  1.00 
    ## 3 healthy…         5 NNAR        Test  0.00495  7.27 0.00425  7.92 0.00723 1.00 
    ## 4 healthy…         5 NNAR        Test  0.00446  2.45 0.00383  2.43 0.00973 1.00 
    ## 5 healthy…         5 NNAR        Test  0.00589  1.57 0.00558  1.52 0.0135  1.00 
    ## 6 TidyDen…         5 NNAR        Test  0.00665  7.11 0.00543 10.5  0.00970 1.00

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
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [270|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [118|28]> <mdl_time_tbl>

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
