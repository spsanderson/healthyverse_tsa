Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,859
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

The last day in the data set is 2025-01-11 21:56:48, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3778.35
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126859        |
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
| r_version     |     90267 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     90267 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     90267 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10787 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-11 | 2023-04-19 | 1511 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1143861.06 | 1532993.2 | 355 | 14701 | 260377 | 2367914 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10340.05 | 18022.6 | 1 | 317 | 3091 | 11827 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-11 21:56:48 | 2023-04-19 15:16:33 | 76775 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 40S |       60 |

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
    ## -154.98  -35.17   -9.88   27.17  806.09 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.871e+02  7.891e+01
    ## date                                                1.122e-02  4.181e-03
    ## lag(value, 1)                                       1.255e-01  2.540e-02
    ## lag(value, 7)                                       9.043e-02  2.633e-02
    ## lag(value, 14)                                      1.069e-01  2.645e-02
    ## lag(value, 21)                                      4.899e-02  2.655e-02
    ## lag(value, 28)                                      6.661e-02  2.640e-02
    ## lag(value, 35)                                      7.202e-02  2.653e-02
    ## lag(value, 42)                                      5.148e-02  2.660e-02
    ## lag(value, 49)                                      9.361e-02  2.644e-02
    ## month(date, label = TRUE).L                        -1.119e+01  5.397e+00
    ## month(date, label = TRUE).Q                         2.114e+00  5.243e+00
    ## month(date, label = TRUE).C                        -1.132e+01  5.314e+00
    ## month(date, label = TRUE)^4                        -7.593e+00  5.328e+00
    ## month(date, label = TRUE)^5                        -1.292e+01  5.310e+00
    ## month(date, label = TRUE)^6                        -9.452e-01  5.387e+00
    ## month(date, label = TRUE)^7                        -9.087e+00  5.286e+00
    ## month(date, label = TRUE)^8                        -2.219e+00  5.282e+00
    ## month(date, label = TRUE)^9                         4.160e+00  5.273e+00
    ## month(date, label = TRUE)^10                        5.186e+00  5.273e+00
    ## month(date, label = TRUE)^11                       -6.267e+00  5.286e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.170e+01  2.429e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.298e+00  2.545e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.371 0.017855 *  
    ## date                                                 2.683 0.007379 ** 
    ## lag(value, 1)                                        4.940 8.74e-07 ***
    ## lag(value, 7)                                        3.434 0.000611 ***
    ## lag(value, 14)                                       4.041 5.60e-05 ***
    ## lag(value, 21)                                       1.845 0.065245 .  
    ## lag(value, 28)                                       2.523 0.011746 *  
    ## lag(value, 35)                                       2.715 0.006704 ** 
    ## lag(value, 42)                                       1.935 0.053133 .  
    ## lag(value, 49)                                       3.541 0.000412 ***
    ## month(date, label = TRUE).L                         -2.073 0.038309 *  
    ## month(date, label = TRUE).Q                          0.403 0.686924    
    ## month(date, label = TRUE).C                         -2.130 0.033317 *  
    ## month(date, label = TRUE)^4                         -1.425 0.154303    
    ## month(date, label = TRUE)^5                         -2.433 0.015105 *  
    ## month(date, label = TRUE)^6                         -0.175 0.860756    
    ## month(date, label = TRUE)^7                         -1.719 0.085796 .  
    ## month(date, label = TRUE)^8                         -0.420 0.674413    
    ## month(date, label = TRUE)^9                          0.789 0.430227    
    ## month(date, label = TRUE)^10                         0.984 0.325509    
    ## month(date, label = TRUE)^11                        -1.186 0.235965    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.816 1.62e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.867 0.004203 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58 on 1439 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2582, Adjusted R-squared:  0.2469 
    ## F-statistic: 22.77 on 22 and 1439 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,476 × 2]> <tibble [28 × 2]> <split [1448|28]>
    ## 2 healthyR      <tibble [1,469 × 2]> <tibble [28 × 2]> <split [1441|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,415 × 2]> <tibble [28 × 2]> <split [1387|28]>
    ## 5 healthyverse  <tibble [1,386 × 2]> <tibble [28 × 2]> <split [1358|28]>
    ## 6 healthyR.ai   <tibble [1,212 × 2]> <tibble [28 × 2]> <split [1184|28]>
    ## 7 TidyDensity   <tibble [1,066 × 2]> <tibble [28 × 2]> <split [1038|28]>
    ## 8 tidyAML       <tibble [682 × 2]>   <tibble [28 × 2]> <split [654|28]> 
    ## 9 RandomWalker  <tibble [116 × 2]>   <tibble [28 × 2]> <split [88|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.9177545 | 276.04302 | 0.6978024 | 160.37534 | 1.0375362 | 0.0003435 |
| healthyR.data | 2 | LM | Test | 0.8850359 | 284.26474 | 0.6729253 | 154.89117 | 1.0231474 | 0.0000783 |
| healthyR.data | 3 | EARTH | Test | 1.0109757 | 458.42642 | 0.7686820 | 147.72303 | 1.1104135 | 0.0000783 |
| healthyR.data | 4 | NNAR | Test | 0.8324964 | 127.51140 | 0.6329776 | 165.92395 | 1.1012437 | 0.0768769 |
| healthyR | 1 | ARIMA | Test | 0.6923447 | 140.51165 | 0.6725035 | 164.14654 | 0.8781946 | 0.0308164 |
| healthyR | 2 | LM | Test | 0.6817845 | 102.62538 | 0.6622459 | 182.14207 | 0.8905374 | 0.0041808 |
| healthyR | 3 | EARTH | Test | 0.6802239 | 107.06143 | 0.6607300 | 164.67305 | 0.8920383 | 0.0041808 |
| healthyR | 4 | NNAR | Test | 0.7030462 | 156.02666 | 0.6828983 | 154.37918 | 0.8966574 | 0.0073013 |
| healthyR.ts | 1 | ARIMA | Test | 0.8488055 | 136.29035 | 0.6245675 | 121.21509 | 1.0849222 | 0.0106836 |
| healthyR.ts | 2 | LM | Test | 0.8621492 | 152.87235 | 0.6343861 | 118.84853 | 1.0907612 | 0.0080471 |
| healthyR.ts | 3 | EARTH | Test | 0.8649804 | 155.75595 | 0.6364693 | 118.59524 | 1.0922587 | 0.0080471 |
| healthyR.ts | 4 | NNAR | Test | 0.8993757 | 104.16994 | 0.6617781 | 182.46264 | 1.1162788 | 0.0123008 |
| healthyverse | 1 | ARIMA | Test | 0.5998285 | 209.91924 | 0.7472453 | 93.93681 | 0.7507566 | 0.1581189 |
| healthyverse | 2 | LM | Test | 0.6403416 | 202.79222 | 0.7977151 | 94.52354 | 0.7987204 | 0.0055420 |
| healthyverse | 3 | EARTH | Test | 0.6766503 | 251.60329 | 0.8429472 | 91.20861 | 0.8645959 | 0.0055420 |
| healthyverse | 4 | NNAR | Test | 0.6418797 | 128.36495 | 0.7996312 | 117.83478 | 0.7739283 | 0.0519691 |
| healthyR.ai | 1 | ARIMA | Test | 0.6898253 | 100.78287 | 0.7040626 | 176.31592 | 0.8105793 | 0.0252507 |
| healthyR.ai | 2 | LM | Test | 0.6692322 | 107.01859 | 0.6830444 | 143.85042 | 0.8196802 | 0.0001121 |
| healthyR.ai | 3 | EARTH | Test | 0.6484629 | 129.20710 | 0.6618465 | 118.31635 | 0.8416875 | 0.0001121 |
| healthyR.ai | 4 | NNAR | Test | 0.6917307 | 106.70456 | 0.7060073 | 153.34951 | 0.8279850 | 0.0033299 |
| TidyDensity | 1 | ARIMA | Test | 0.8236341 | 199.38878 | 0.7456347 | 123.57122 | 0.9543428 | 0.0400578 |
| TidyDensity | 2 | LM | Test | 0.8373077 | 216.10604 | 0.7580134 | 120.99455 | 0.9774425 | 0.0054369 |
| TidyDensity | 3 | EARTH | Test | 0.7716380 | 158.48886 | 0.6985627 | 126.58746 | 0.9174988 | 0.0054369 |
| TidyDensity | 4 | NNAR | Test | 0.7544632 | 122.91955 | 0.6830144 | 148.04823 | 0.8952036 | 0.0539335 |
| tidyAML | 1 | ARIMA | Test | 0.8874827 | 229.50610 | 0.8126876 | 109.39776 | 1.0008884 | 0.1078869 |
| tidyAML | 2 | LM | Test | 0.9494532 | 207.87414 | 0.8694353 | 118.72937 | 1.0819834 | 0.0095287 |
| tidyAML | 3 | EARTH | Test | 0.8942234 | 273.87905 | 0.8188601 | 103.80110 | 1.0029156 | 0.0095287 |
| tidyAML | 4 | NNAR | Test | 0.8878008 | 246.68903 | 0.8129788 | 106.04925 | 1.0072121 | 0.0284873 |
| RandomWalker | 1 | ARIMA | Test | 0.9876270 | 135.69632 | 0.4458945 | 103.26039 | 1.2610662 | 0.2799449 |
| RandomWalker | 2 | LM | Test | 1.3077437 | 101.86119 | 0.5904210 | 162.68231 | 1.4762539 | 0.0002293 |
| RandomWalker | 3 | EARTH | Test | 1.3073042 | 99.71527 | 0.5902226 | 164.67739 | 1.4747929 | NA |
| RandomWalker | 4 | NNAR | Test | 2.0884038 | 523.62852 | 0.9428740 | 137.19905 | 3.4093398 | 0.0189747 |

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
    ## 1 healthyR.da…         2 LM          Test  0.885  284. 0.673 155.  1.02  7.83e-5
    ## 2 healthyR             1 ARIMA       Test  0.692  141. 0.673 164.  0.878 3.08e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.849  136. 0.625 121.  1.08  1.07e-2
    ## 4 healthyverse         1 ARIMA       Test  0.600  210. 0.747  93.9 0.751 1.58e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.690  101. 0.704 176.  0.811 2.53e-2
    ## 6 TidyDensity          4 NNAR        Test  0.754  123. 0.683 148.  0.895 5.39e-2
    ## 7 tidyAML              1 ARIMA       Test  0.887  230. 0.813 109.  1.00  1.08e-1
    ## 8 RandomWalker         1 ARIMA       Test  0.988  136. 0.446 103.  1.26  2.80e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1448|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1441|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1387|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1358|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1184|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1038|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [654|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [88|28]>   <mdl_tm_t [1 × 5]>

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
