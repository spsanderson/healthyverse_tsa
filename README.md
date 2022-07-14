Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
13 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,471
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

The last day in the data set is 2022-07-11 23:55:02, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -211.95
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41471         |
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
| r_version     |     27819 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27819 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27819 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3304 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-11 | 2021-11-07 |      596 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1422535.2 | 1794023.9 | 357 | 16873 | 289681 | 2974477 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8733.4 |   16752.9 |   1 |   168 |   2618 |    8671 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-11 23:55:02 | 2021-11-07 18:10:25 |    24258 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 21M 53S |       60 |

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [268|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [116|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0563744 |  11.267776 | 0.0392687 |  10.336279 | 0.0700341 | 0.9962907 |
| healthyR      |         2 | GLMNET      | Test  | 0.5917130 |  66.402314 | 0.4121692 | 102.025421 | 0.7842126 | 0.9965981 |
| healthyR      |         3 | LM          | Test  | 0.0580414 |  12.896857 | 0.0404299 |  12.102561 | 0.0686186 | 0.9962901 |
| healthyR      |         4 | EARTH       | Test  | 0.0472645 |   4.640204 | 0.0329230 |   4.626920 | 0.0884751 | 0.9965981 |
| healthyR      |         5 | NNAR        | Test  | 0.0279508 |   2.451998 | 0.0194697 |   2.406169 | 0.0927512 | 0.9938810 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0542315 | 130.709827 | 0.0445936 |  16.761752 | 0.0655900 | 0.9963138 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4639742 | 262.951130 | 0.3815177 |  86.136153 | 0.5848477 | 0.9987669 |
| healthyR.data |         3 | LM          | Test  | 0.0537638 | 127.904035 | 0.0442090 |  16.599825 | 0.0650745 | 0.9963150 |
| healthyR.data |         4 | EARTH       | Test  | 0.0214015 |  51.499188 | 0.0175981 |   9.679780 | 0.0300776 | 0.9987669 |
| healthyR.data |         5 | NNAR        | Test  | 0.0060287 |  10.583110 | 0.0049573 |   5.157970 | 0.0134402 | 0.9997373 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0531917 | 428.289732 | 0.0427014 |  35.040421 | 0.0625732 | 0.9971347 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.4899958 | 666.012895 | 0.3933606 | 104.178767 | 0.6324359 | 0.9978941 |
| healthyR.ts   |         3 | LM          | Test  | 0.0580334 | 606.771877 | 0.0465883 |  35.494850 | 0.0657695 | 0.9970296 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0271286 |  52.553268 | 0.0217784 |  21.270208 | 0.0475094 | 0.9978941 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0076250 |  18.569618 | 0.0061212 |  17.238900 | 0.0173378 | 0.9997151 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0418169 | 134.304520 | 0.0342768 |  18.320629 | 0.0543045 | 0.9960078 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4763110 | 763.228236 | 0.3904272 | 105.984466 | 0.6270603 | 0.9957789 |
| healthyverse  |         3 | LM          | Test  | 0.0500525 | 243.215462 | 0.0410275 |  21.623076 | 0.0590082 | 0.9960674 |
| healthyverse  |         4 | EARTH       | Test  | 0.0100989 |  18.555414 | 0.0082780 |   9.110775 | 0.0149882 | 0.9997267 |
| healthyverse  |         5 | NNAR        | Test  | 0.0059901 |   6.845784 | 0.0049100 |  11.255161 | 0.0104370 | 0.9998659 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0427874 |  87.212343 | 0.0398249 |  15.402561 | 0.0517579 | 0.9966805 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4632147 | 207.489688 | 0.4311430 | 102.947067 | 0.5939511 | 0.9968536 |
| healthyR.ai   |         3 | LM          | Test  | 0.0431346 |  91.059500 | 0.0401481 |  15.693873 | 0.0518600 | 0.9966772 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0128008 |  11.774905 | 0.0119145 |   9.463863 | 0.0150560 | 0.9997447 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0028684 |   5.933576 | 0.0026698 |   7.688668 | 0.0043081 | 0.9999845 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0267922 |  21.590076 | 0.0219438 |  11.114196 | 0.0319598 | 0.9988648 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4989822 | 440.041311 | 0.4086852 | 104.088651 | 0.6491360 | 0.9988788 |
| TidyDensity   |         3 | LM          | Test  | 0.0283542 |  49.211790 | 0.0232232 |  15.643971 | 0.0328320 | 0.9988467 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0090659 |   2.818502 | 0.0074253 |   2.689795 | 0.0120190 | 0.9998596 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0074763 |  23.232180 | 0.0061234 |  16.384187 | 0.0115610 | 0.9998657 |

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
    ## 1 healthyR         3 LM          Test  0.0580  12.9  0.0404  12.1  0.0686  0.996
    ## 2 healthy…         5 NNAR        Test  0.00603 10.6  0.00496  5.16 0.0134  1.00 
    ## 3 healthy…         5 NNAR        Test  0.00762 18.6  0.00612 17.2  0.0173  1.00 
    ## 4 healthy…         5 NNAR        Test  0.00599  6.85 0.00491 11.3  0.0104  1.00 
    ## 5 healthy…         5 NNAR        Test  0.00287  5.93 0.00267  7.69 0.00431 1.00 
    ## 6 TidyDen…         5 NNAR        Test  0.00748 23.2  0.00612 16.4  0.0116  1.00

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [268|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [116|28]> <mdl_time_tbl>

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
