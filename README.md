Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
09 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,185
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

The last day in the data set is 2022-07-07 23:22:51, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -115.41
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41185         |
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
| r_version     |     27621 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27621 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27621 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3298 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-07 | 2021-11-06 |      592 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1425432.25 | 1797094.11 | 357 | 16873 | 289681 | 2974487 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8739.84 |   16772.55 |   1 |   168 |   2632 |    8671 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-07 23:22:51 | 2021-11-06 20:37:39 |    24038 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 21M 25S |       60 |

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [264|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [112|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0522056 |  11.2608033 | 0.0382544 |  10.1649511 | 0.0658620 | 0.9963578 |
| healthyR      |         2 | GLMNET      | Test  | 0.5662898 |  67.1658894 | 0.4149574 | 104.4445396 | 0.7443813 | 0.9966153 |
| healthyR      |         3 | LM          | Test  | 0.0524734 |  11.8666836 | 0.0384507 |  10.7493786 | 0.0645736 | 0.9963658 |
| healthyR      |         4 | EARTH       | Test  | 0.0397895 |   4.4424881 | 0.0291564 |   4.4062778 | 0.0787006 | 0.9966153 |
| healthyR      |         5 | NNAR        | Test  | 0.0399152 |   3.0200484 | 0.0292485 |   2.7874193 | 0.1633455 | 0.9843051 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0553423 |  47.8346852 | 0.0460094 |  17.0573870 | 0.0653701 | 0.9965383 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4562696 | 112.9111252 | 0.3793245 |  91.5940860 | 0.5667684 | 0.9989928 |
| healthyR.data |         3 | LM          | Test  | 0.0540893 |  45.9445382 | 0.0449677 |  16.6451398 | 0.0641216 | 0.9965467 |
| healthyR.data |         4 | EARTH       | Test  | 0.0205787 |  16.2873459 | 0.0171083 |   8.3700640 | 0.0275511 | 0.9989928 |
| healthyR.data |         5 | NNAR        | Test  | 0.0054855 |   6.5022020 | 0.0045604 |   4.4379037 | 0.0070873 | 0.9999355 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0500077 | 121.4447259 | 0.0387061 |  29.8623305 | 0.0607689 | 0.9968918 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.4992570 | 224.0177960 | 0.3864260 | 113.4593601 | 0.6336536 | 0.9976474 |
| healthyR.ts   |         3 | LM          | Test  | 0.0620952 | 171.7811743 | 0.0480618 |  33.0248947 | 0.0710245 | 0.9967307 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0269241 |  17.3543048 | 0.0208393 |  12.8002530 | 0.0478476 | 0.9976474 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0058085 |   6.5994978 | 0.0044958 |   9.6596666 | 0.0084396 | 0.9999300 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0379250 |  34.6997956 | 0.0317542 |  12.1361304 | 0.0458408 | 0.9970626 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4598919 | 148.0459924 | 0.3850628 |  98.5802047 | 0.5900448 | 0.9965076 |
| healthyverse  |         3 | LM          | Test  | 0.0496926 |  54.4319946 | 0.0416071 |  15.1535867 | 0.0572517 | 0.9970601 |
| healthyverse  |         4 | EARTH       | Test  | 0.0093593 |   3.6588245 | 0.0078365 |   3.0932341 | 0.0134031 | 0.9997329 |
| healthyverse  |         5 | NNAR        | Test  | 0.0033271 |   0.5926883 | 0.0027857 |   0.5943462 | 0.0052257 | 0.9999734 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0446002 |  29.5416387 | 0.0443357 |  14.6529463 | 0.0532864 | 0.9964356 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4567617 | 103.8079890 | 0.4540525 | 109.6503576 | 0.5794586 | 0.9966083 |
| healthyR.ai   |         3 | LM          | Test  | 0.0452169 |  31.6728404 | 0.0449487 |  15.2299782 | 0.0534291 | 0.9964323 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0116615 |   4.1061906 | 0.0115923 |   4.9452931 | 0.0141346 | 0.9997447 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0027088 |   1.8052443 | 0.0026927 |   2.1003543 | 0.0044334 | 0.9999824 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0277827 |  12.3558291 | 0.0259907 |   9.6765683 | 0.0325906 | 0.9987611 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4467015 | 156.1181084 | 0.4178879 | 102.9859971 | 0.5881705 | 0.9989044 |
| TidyDensity   |         3 | LM          | Test  | 0.0290113 |  22.8940894 | 0.0271400 |  13.2317333 | 0.0341013 | 0.9987211 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0074363 |   2.5384756 | 0.0069566 |   2.4259646 | 0.0106329 | 0.9998958 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0056031 |   4.8975576 | 0.0052417 |   6.8868643 | 0.0081130 | 0.9999093 |

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
    ##   package      .model_id .model_desc .type     mae   mape    mase  smape    rmse
    ##   <chr>            <int> <chr>       <chr>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
    ## 1 healthyR             3 LM          Test  0.0525  11.9   0.0385  10.7   0.0646 
    ## 2 healthyR.da…         5 NNAR        Test  0.00549  6.50  0.00456  4.44  0.00709
    ## 3 healthyR.ts          5 NNAR        Test  0.00581  6.60  0.00450  9.66  0.00844
    ## 4 healthyverse         5 NNAR        Test  0.00333  0.593 0.00279  0.594 0.00523
    ## 5 healthyR.ai          5 NNAR        Test  0.00271  1.81  0.00269  2.10  0.00443
    ## 6 TidyDensity          5 NNAR        Test  0.00560  4.90  0.00524  6.89  0.00811
    ## # … with 1 more variable: rsq <dbl>

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [264|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [112|28]> <mdl_time_tbl>

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
