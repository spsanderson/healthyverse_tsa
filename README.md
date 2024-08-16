Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
16 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,867
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

The last day in the data set is 2024-08-14 20:51:15, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -177.26
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110867        |
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
| r_version     |     77632 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77632 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77632 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9525 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-14 | 2023-01-23 |     1361 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |         sd |  p0 |   p25 |    p50 |       p75 |    p100 | hist  |
|:--------------|----------:|--------------:|--------:|-----------:|----:|------:|-------:|----------:|--------:|:------|
| size          |         0 |             1 | 1181767 | 1561249.59 | 355 | 14701 | 289680 | 2373526.0 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10321 |   18059.68 |   1 |   317 |   3091 |   11473.5 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-14 20:51:15 | 2023-01-23 10:00:51 |    67134 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 1S |       60 |

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

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,332 × 2]> <tibble [28 × 2]> <split [1304|28]>
    ## 2 healthyR      <tibble [1,324 × 2]> <tibble [28 × 2]> <split [1296|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,270 × 2]> <tibble [28 × 2]> <split [1242|28]>
    ## 5 healthyverse  <tibble [1,241 × 2]> <tibble [28 × 2]> <split [1213|28]>
    ## 6 healthyR.ai   <tibble [1,067 × 2]> <tibble [28 × 2]> <split [1039|28]>
    ## 7 TidyDensity   <tibble [921 × 2]>   <tibble [28 × 2]> <split [893|28]> 
    ## 8 tidyAML       <tibble [537 × 2]>   <tibble [28 × 2]> <split [509|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

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
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.5464091 |  127.60295 | 0.5576023 | 128.0169 | 0.7535842 | 0.2398840 |
| healthyR.data |         2 | LM          | Test  | 0.7756976 |  337.87509 | 0.7915878 | 152.4446 | 0.9107114 | 0.0193713 |
| healthyR.data |         3 | EARTH       | Test  | 0.6297316 |  191.79431 | 0.6426317 | 127.1292 | 0.8637087 | 0.0193713 |
| healthyR.data |         4 | NNAR        | Test  | 0.6214553 |  124.11151 | 0.6341858 | 145.8915 | 0.8373149 | 0.0029836 |
| healthyR      |         1 | ARIMA       | Test  | 0.6546422 |  121.49466 | 0.8830051 | 147.6635 | 0.7890304 | 0.0643624 |
| healthyR      |         2 | LM          | Test  | 0.7089820 |  114.45922 | 0.9563005 | 162.3271 | 0.8952465 | 0.0630428 |
| healthyR      |         3 | EARTH       | Test  | 0.6636675 |  148.03850 | 0.8951788 | 140.1192 | 0.7919607 | 0.0630428 |
| healthyR      |         4 | NNAR        | Test  | 0.6874107 |  123.67551 | 0.9272044 | 166.4193 | 0.8430620 | 0.0735488 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 2.1696969 | 1235.94133 | 1.8948836 | 146.9873 | 2.3908064 | 0.0737964 |
| healthyR.ts   |         2 | LM          | Test  | 0.8762596 |  240.92469 | 0.7652728 | 139.2975 | 1.0803831 | 0.0627552 |
| healthyR.ts   |         3 | EARTH       | Test  | 4.0898008 | 2526.90394 | 3.5717875 | 164.9625 | 4.3656908 | 0.0627552 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9263804 |  140.54640 | 0.8090453 | 184.4653 | 1.1226772 | 0.0011173 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7339684 |  397.31621 | 1.0029962 | 113.2856 | 0.8644220 | 0.0339296 |
| healthyverse  |         2 | LM          | Test  | 0.7199040 |  517.66902 | 0.9837766 |  99.8905 | 0.8753733 | 0.0051789 |
| healthyverse  |         3 | EARTH       | Test  | 0.7446509 |  364.21640 | 1.0175942 | 115.4572 | 0.8823392 | 0.0051789 |
| healthyverse  |         4 | NNAR        | Test  | 0.7992679 |  249.78616 | 1.0922304 | 142.5543 | 0.9586116 | 0.0014782 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.0102636 |  115.23172 | 0.8963133 | 141.9146 | 1.4793456 | 0.0135182 |
| healthyR.ai   |         2 | LM          | Test  | 1.0134694 |  137.50313 | 0.8991575 | 130.9723 | 1.4862582 | 0.1022220 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.0108499 |  132.13051 | 0.8968334 | 135.2244 | 1.4722076 | 0.1022220 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.9809656 |  113.06432 | 0.8703198 | 145.6263 | 1.4282317 | 0.0059492 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6344633 |  204.84914 | 0.8463905 | 106.4217 | 0.7662615 | 0.4832609 |
| TidyDensity   |         2 | LM          | Test  | 0.6776033 |  209.00575 | 0.9039405 | 108.7201 | 0.8233606 | 0.0358155 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7173832 |  128.42654 | 0.9570078 | 176.3870 | 0.8631992 | 0.0358155 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5743641 |  122.27881 | 0.7662166 | 141.8039 | 0.6690482 | 0.2453203 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6262777 |   83.42895 | 0.7879188 | 106.4073 | 0.8220761 | 0.2402962 |
| tidyAML       |         2 | LM          | Test  | 0.7332827 |  122.16732 | 0.9225416 | 117.9027 | 0.9143411 | 0.0819339 |
| tidyAML       |         3 | EARTH       | Test  | 2.8930522 |  689.94705 | 3.6397432 | 163.8960 | 3.3521352 | 0.0819339 |
| tidyAML       |         4 | NNAR        | Test  | 0.6444724 |  122.94898 | 0.8108094 | 101.2436 | 0.8176264 | 0.2031437 |

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
    ##   # A tibble: 7 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         1 ARIMA       Test  0.546  128. 0.558  128. 0.754 0.240  
    ## 2 healthyR             1 ARIMA       Test  0.655  121. 0.883  148. 0.789 0.0644 
    ## 3 healthyR.ts          2 LM          Test  0.876  241. 0.765  139. 1.08  0.0628 
    ## 4 healthyverse         1 ARIMA       Test  0.734  397. 1.00   113. 0.864 0.0339 
    ## 5 healthyR.ai          4 NNAR        Test  0.981  113. 0.870  146. 1.43  0.00595
    ## 6 TidyDensity          4 NNAR        Test  0.574  122. 0.766  142. 0.669 0.245  
    ## 7 tidyAML              4 NNAR        Test  0.644  123. 0.811  101. 0.818 0.203

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
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
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1304|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1296|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1242|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1213|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1039|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [893|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [509|28]>  <mdl_tm_t [1 × 5]>

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
