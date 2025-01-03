Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
03 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 125,775
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

The last day in the data set is 2025-01-01 23:32:11, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3539.94
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 125775        |
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
| r_version     |     89355 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89355 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89355 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10753 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-01 | 2023-04-14 | 1501 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144589.30 | 1534339.50 | 355 | 14701 | 258758 | 2367918 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10316.82 | 17945.93 | 1 | 317 | 3098 | 11821 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-01 23:32:11 | 2023-04-14 12:12:19 | 76074 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 54S |       60 |

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

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -156.07  -35.17   -9.89   27.13  802.89 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.894e+02  7.962e+01
    ## date                                                1.130e-02  4.218e-03
    ## lag(value, 1)                                       1.294e-01  2.551e-02
    ## lag(value, 7)                                       9.234e-02  2.646e-02
    ## lag(value, 14)                                      1.061e-01  2.648e-02
    ## lag(value, 21)                                      4.858e-02  2.660e-02
    ## lag(value, 28)                                      7.016e-02  2.643e-02
    ## lag(value, 35)                                      7.178e-02  2.655e-02
    ## lag(value, 42)                                      4.831e-02  2.661e-02
    ## lag(value, 49)                                      9.899e-02  2.651e-02
    ## month(date, label = TRUE).L                        -1.183e+01  5.463e+00
    ## month(date, label = TRUE).Q                         2.681e+00  5.288e+00
    ## month(date, label = TRUE).C                        -1.190e+01  5.350e+00
    ## month(date, label = TRUE)^4                        -7.164e+00  5.351e+00
    ## month(date, label = TRUE)^5                        -1.313e+01  5.316e+00
    ## month(date, label = TRUE)^6                        -7.537e-01  5.386e+00
    ## month(date, label = TRUE)^7                        -9.140e+00  5.280e+00
    ## month(date, label = TRUE)^8                        -2.180e+00  5.275e+00
    ## month(date, label = TRUE)^9                         4.193e+00  5.265e+00
    ## month(date, label = TRUE)^10                        5.163e+00  5.265e+00
    ## month(date, label = TRUE)^11                       -6.248e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.433e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.137e+00  2.548e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.379 0.017497 *  
    ## date                                                 2.678 0.007482 ** 
    ## lag(value, 1)                                        5.072 4.45e-07 ***
    ## lag(value, 7)                                        3.490 0.000499 ***
    ## lag(value, 14)                                       4.007 6.45e-05 ***
    ## lag(value, 21)                                       1.826 0.068024 .  
    ## lag(value, 28)                                       2.655 0.008028 ** 
    ## lag(value, 35)                                       2.703 0.006951 ** 
    ## lag(value, 42)                                       1.815 0.069689 .  
    ## lag(value, 49)                                       3.735 0.000195 ***
    ## month(date, label = TRUE).L                         -2.166 0.030444 *  
    ## month(date, label = TRUE).Q                          0.507 0.612249    
    ## month(date, label = TRUE).C                         -2.225 0.026248 *  
    ## month(date, label = TRUE)^4                         -1.339 0.180809    
    ## month(date, label = TRUE)^5                         -2.469 0.013667 *  
    ## month(date, label = TRUE)^6                         -0.140 0.888727    
    ## month(date, label = TRUE)^7                         -1.731 0.083639 .  
    ## month(date, label = TRUE)^8                         -0.413 0.679527    
    ## month(date, label = TRUE)^9                          0.796 0.425960    
    ## month(date, label = TRUE)^10                         0.981 0.326960    
    ## month(date, label = TRUE)^11                        -1.184 0.236743    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.778 1.95e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.801 0.005167 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.91 on 1429 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2605, Adjusted R-squared:  0.2491 
    ## F-statistic: 22.88 on 22 and 1429 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,466 × 2]> <tibble [28 × 2]> <split [1438|28]>
    ## 2 healthyR      <tibble [1,459 × 2]> <tibble [28 × 2]> <split [1431|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,405 × 2]> <tibble [28 × 2]> <split [1377|28]>
    ## 5 healthyverse  <tibble [1,376 × 2]> <tibble [28 × 2]> <split [1348|28]>
    ## 6 healthyR.ai   <tibble [1,202 × 2]> <tibble [28 × 2]> <split [1174|28]>
    ## 7 TidyDensity   <tibble [1,056 × 2]> <tibble [28 × 2]> <split [1028|28]>
    ## 8 tidyAML       <tibble [672 × 2]>   <tibble [28 × 2]> <split [644|28]> 
    ## 9 RandomWalker  <tibble [106 × 2]>   <tibble [28 × 2]> <split [78|28]>

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
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7826970 | 265.66898 | 0.6106291 | 145.57052 | 0.9180702 | 0.1122626 |
| healthyR.data | 2 | LM | Test | 0.7918351 | 275.70428 | 0.6177582 | 139.14928 | 0.9525458 | 0.0047838 |
| healthyR.data | 3 | EARTH | Test | 0.8648769 | 421.75312 | 0.6747426 | 133.26385 | 0.9855395 | 0.0047838 |
| healthyR.data | 4 | NNAR | Test | 0.8260387 | 145.92749 | 0.6444425 | 168.13946 | 1.0716914 | 0.0024501 |
| healthyR | 1 | ARIMA | Test | 0.7318171 | 151.92681 | 0.6830891 | 173.15469 | 0.8580843 | 0.0139983 |
| healthyR | 2 | LM | Test | 0.6963221 | 102.55732 | 0.6499576 | 183.40497 | 0.8571278 | 0.0000734 |
| healthyR | 3 | EARTH | Test | 0.6900977 | 97.26213 | 0.6441476 | 174.07316 | 0.8576826 | 0.0000734 |
| healthyR | 4 | NNAR | Test | 0.6959560 | 116.49709 | 0.6496158 | 149.97537 | 0.8794670 | 0.0372272 |
| healthyR.ts | 1 | ARIMA | Test | 0.9212467 | 143.12127 | 0.7449942 | 133.64323 | 1.1080277 | 0.0079546 |
| healthyR.ts | 2 | LM | Test | 0.9382367 | 163.66467 | 0.7587337 | 130.80604 | 1.1126590 | 0.0063981 |
| healthyR.ts | 3 | EARTH | Test | 0.9411174 | 166.73829 | 0.7610633 | 130.47574 | 1.1139487 | 0.0063981 |
| healthyR.ts | 4 | NNAR | Test | 0.9085089 | 97.32289 | 0.7346934 | 185.88889 | 1.1214374 | 0.1062536 |
| healthyverse | 1 | ARIMA | Test | 0.4902196 | 132.60300 | 0.7162357 | 88.75271 | 0.6269293 | 0.3245312 |
| healthyverse | 2 | LM | Test | 0.5678044 | 223.22746 | 0.8295909 | 86.78555 | 0.7099406 | 0.0159358 |
| healthyverse | 3 | EARTH | Test | 0.6073919 | 247.57401 | 0.8874303 | 87.94392 | 0.7519000 | 0.0159358 |
| healthyverse | 4 | NNAR | Test | 0.6002510 | 147.92421 | 0.8769971 | 107.33544 | 0.7495184 | 0.0719146 |
| healthyR.ai | 1 | ARIMA | Test | 0.7135386 | 95.75355 | 0.8466457 | 173.26737 | 0.8099544 | 0.2206162 |
| healthyR.ai | 2 | LM | Test | 0.7170022 | 94.62648 | 0.8507554 | 149.77732 | 0.8397774 | 0.0005233 |
| healthyR.ai | 3 | EARTH | Test | 0.6872905 | 92.58058 | 0.8155012 | 128.38608 | 0.8475125 | 0.0005233 |
| healthyR.ai | 4 | NNAR | Test | 0.7055360 | 89.11831 | 0.8371503 | 157.12094 | 0.8169346 | 0.1461916 |
| TidyDensity | 1 | ARIMA | Test | 0.7433540 | 196.44424 | 0.8713293 | 129.20251 | 0.8849229 | 0.0034023 |
| TidyDensity | 2 | LM | Test | 0.7869097 | 323.97954 | 0.9223836 | 122.37002 | 0.8770581 | 0.0592751 |
| TidyDensity | 3 | EARTH | Test | 0.7255409 | 207.17908 | 0.8504496 | 129.38834 | 0.8627768 | 0.0592751 |
| TidyDensity | 4 | NNAR | Test | 0.7136177 | 109.59566 | 0.8364736 | 145.01232 | 0.9112887 | 0.0474483 |
| tidyAML | 1 | ARIMA | Test | 0.8784126 | 101.95331 | 0.9075871 | 111.57769 | 1.0432977 | 0.2259030 |
| tidyAML | 2 | LM | Test | 0.9441071 | 124.12205 | 0.9754634 | 113.36748 | 1.0763699 | 0.0365167 |
| tidyAML | 3 | EARTH | Test | 0.8829893 | 155.01873 | 0.9123158 | 99.41666 | 0.9690570 | 0.0365167 |
| tidyAML | 4 | NNAR | Test | 0.8967263 | 139.91213 | 0.9265090 | 105.02795 | 0.9896942 | 0.0977086 |
| RandomWalker | 1 | ARIMA | Test | 0.8536026 | 57.48815 | 0.3718757 | 89.70446 | 1.0766050 | 0.5469387 |
| RandomWalker | 2 | LM | Test | 1.3595475 | 95.84175 | 0.5922928 | 181.78306 | 1.4746843 | 0.0056898 |
| RandomWalker | 3 | EARTH | Test | 1.2782170 | 117.84373 | 0.5568608 | 106.05402 | 1.6344143 | 0.0056898 |
| RandomWalker | 4 | NNAR | Test | 4.0526358 | 578.29091 | 1.7655484 | 167.30676 | 5.3107351 | 0.0684194 |

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
    ## 

    ## # A tibble: 8 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         1 ARIMA       Test  0.783 266.  0.611 146.  0.918 1.12e-1
    ## 2 healthyR             2 LM          Test  0.696 103.  0.650 183.  0.857 7.34e-5
    ## 3 healthyR.ts          1 ARIMA       Test  0.921 143.  0.745 134.  1.11  7.95e-3
    ## 4 healthyverse         1 ARIMA       Test  0.490 133.  0.716  88.8 0.627 3.25e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.714  95.8 0.847 173.  0.810 2.21e-1
    ## 6 TidyDensity          3 EARTH       Test  0.726 207.  0.850 129.  0.863 5.93e-2
    ## 7 tidyAML              3 EARTH       Test  0.883 155.  0.912  99.4 0.969 3.65e-2
    ## 8 RandomWalker         1 ARIMA       Test  0.854  57.5 0.372  89.7 1.08  5.47e-1

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
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1438|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1431|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1377|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1348|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1174|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1028|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [644|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [78|28]>   <mdl_tm_t [1 × 5]>

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
