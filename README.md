Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
11 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,409
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

The last day in the data set is 2022-07-09 22:20:26, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -162.37
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41409         |
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
| r_version     |     27790 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     27790 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     27790 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3298 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-09 | 2021-11-07 |      594 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1423031.82 | 1794960.66 | 357 | 16873 | 289681 | 2974477 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8732.91 |   16751.77 |   1 |   174 |   2636 |    8671 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-09 22:20:26 | 2021-11-07 11:49:45 |    24202 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 22M 1S |       60 |

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [266|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [114|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0548969 |  11.149661 | 0.0405737 |  10.282116 | 0.0683371 | 0.9960829 |
| healthyR      |         2 | GLMNET      | Test  | 0.5840414 |  66.084920 | 0.4316587 | 101.929443 | 0.7549643 | 0.9964985 |
| healthyR      |         3 | LM          | Test  | 0.0567698 |  12.821568 | 0.0419580 |  12.161951 | 0.0676587 | 0.9960731 |
| healthyR      |         4 | EARTH       | Test  | 0.0402273 |   4.263914 | 0.0297316 |   4.237397 | 0.0789840 | 0.9964985 |
| healthyR      |         5 | NNAR        | Test  | 0.0180908 |   1.941866 | 0.0133707 |   1.959072 | 0.0330411 | 0.9992292 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0520831 |  34.575035 | 0.0443524 |  15.499987 | 0.0647470 | 0.9970927 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4556493 |  98.537959 | 0.3880173 |  90.515921 | 0.5704547 | 0.9990182 |
| healthyR.data |         3 | LM          | Test  | 0.0520571 |  34.545067 | 0.0443302 |  15.491520 | 0.0647163 | 0.9970930 |
| healthyR.data |         4 | EARTH       | Test  | 0.0202834 |  13.563071 | 0.0172728 |   7.970695 | 0.0271059 | 0.9990182 |
| healthyR.data |         5 | NNAR        | Test  | 0.0088992 |   3.337046 | 0.0075783 |   3.024040 | 0.0126295 | 0.9997848 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0520584 |  93.664924 | 0.0407879 |  28.733897 | 0.0621538 | 0.9969739 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5065336 | 182.078628 | 0.3968707 | 108.312239 | 0.6381806 | 0.9977054 |
| healthyR.ts   |         3 | LM          | Test  | 0.0635251 | 134.566991 | 0.0497721 |  32.327637 | 0.0718747 | 0.9968476 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0271719 |  14.119833 | 0.0212893 |  11.093768 | 0.0480092 | 0.9977054 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0112339 |   6.117959 | 0.0088018 |   9.450742 | 0.0290739 | 0.9991474 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0368679 |  32.431315 | 0.0318398 |  14.095130 | 0.0457309 | 0.9969232 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4404959 | 194.266239 | 0.3804208 |  97.189441 | 0.5786237 | 0.9964251 |
| healthyverse  |         3 | LM          | Test  | 0.0484198 |  59.108782 | 0.0418163 |  19.062270 | 0.0572811 | 0.9968880 |
| healthyverse  |         4 | EARTH       | Test  | 0.0094692 |   5.044947 | 0.0081778 |   4.284905 | 0.0134426 | 0.9997201 |
| healthyverse  |         5 | NNAR        | Test  | 0.0061536 |   2.949375 | 0.0053144 |   2.601159 | 0.0119118 | 0.9997807 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0451164 |  24.735461 | 0.0427515 |  14.263858 | 0.0535987 | 0.9964919 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4636519 |  90.549119 | 0.4393484 | 101.716054 | 0.5878932 | 0.9966149 |
| healthyR.ai   |         3 | LM          | Test  | 0.0455959 |  26.059085 | 0.0432059 |  14.762551 | 0.0537105 | 0.9964886 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0112832 |   3.442853 | 0.0106917 |   3.746359 | 0.0136987 | 0.9997603 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0043650 |   1.442644 | 0.0041362 |   1.375289 | 0.0093065 | 0.9999019 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0287872 |  10.173659 | 0.0255024 |   8.510029 | 0.0338662 | 0.9987048 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4892274 | 134.908784 | 0.4334041 | 103.412309 | 0.6379020 | 0.9987583 |
| TidyDensity   |         3 | LM          | Test  | 0.0307849 |  18.157263 | 0.0272722 |  11.965335 | 0.0362907 | 0.9986430 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0078043 |   2.641240 | 0.0069138 |   2.487853 | 0.0108725 | 0.9999067 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0057277 |   2.490735 | 0.0050742 |   2.724791 | 0.0086290 | 0.9999211 |

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
    ## 1 healthyR         5 NNAR        Test  0.0181   1.94 0.0134   1.96 0.0330  0.999
    ## 2 healthy…         5 NNAR        Test  0.00890  3.34 0.00758  3.02 0.0126  1.00 
    ## 3 healthy…         5 NNAR        Test  0.0112   6.12 0.00880  9.45 0.0291  0.999
    ## 4 healthy…         5 NNAR        Test  0.00615  2.95 0.00531  2.60 0.0119  1.00 
    ## 5 healthy…         5 NNAR        Test  0.00437  1.44 0.00414  1.38 0.00931 1.00 
    ## 6 TidyDen…         5 NNAR        Test  0.00573  2.49 0.00507  2.72 0.00863 1.00

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
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [266|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [114|28]> <mdl_time_tbl>

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
