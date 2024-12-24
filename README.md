Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
24 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,783
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

The last day in the data set is 2024-12-22 22:46:35, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3299.18
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124783        |
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
| r_version     |     88483 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88483 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88483 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10712 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-22 | 2023-04-09 | 1491 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1148724.43 | 1536531.31 | 355 | 14701 | 260378 | 2368012.0 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10360.15 | 17990.23 | 1 | 334 | 3100 | 11927.5 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-22 22:46:35 | 2023-04-09 07:16:55 | 75597 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 13S |       60 |

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
    ## -155.73  -34.61   -9.64   26.87  803.41 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.828e+02  8.025e+01
    ## date                                                1.095e-02  4.252e-03
    ## lag(value, 1)                                       1.343e-01  2.555e-02
    ## lag(value, 7)                                       9.182e-02  2.648e-02
    ## lag(value, 14)                                      1.067e-01  2.652e-02
    ## lag(value, 21)                                      4.397e-02  2.664e-02
    ## lag(value, 28)                                      7.092e-02  2.646e-02
    ## lag(value, 35)                                      6.978e-02  2.660e-02
    ## lag(value, 42)                                      4.420e-02  2.676e-02
    ## lag(value, 49)                                      1.036e-01  2.659e-02
    ## month(date, label = TRUE).L                        -1.076e+01  5.492e+00
    ## month(date, label = TRUE).Q                         2.319e+00  5.335e+00
    ## month(date, label = TRUE).C                        -1.090e+01  5.401e+00
    ## month(date, label = TRUE)^4                        -7.427e+00  5.374e+00
    ## month(date, label = TRUE)^5                        -1.254e+01  5.325e+00
    ## month(date, label = TRUE)^6                        -9.140e-01  5.386e+00
    ## month(date, label = TRUE)^7                        -8.913e+00  5.275e+00
    ## month(date, label = TRUE)^8                        -2.267e+00  5.269e+00
    ## month(date, label = TRUE)^9                         4.233e+00  5.259e+00
    ## month(date, label = TRUE)^10                        5.150e+00  5.258e+00
    ## month(date, label = TRUE)^11                       -6.196e+00  5.272e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.440e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.946e+00  2.551e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.278 0.022871 *  
    ## date                                                 2.576 0.010086 *  
    ## lag(value, 1)                                        5.257 1.69e-07 ***
    ## lag(value, 7)                                        3.467 0.000542 ***
    ## lag(value, 14)                                       4.023 6.06e-05 ***
    ## lag(value, 21)                                       1.651 0.099013 .  
    ## lag(value, 28)                                       2.680 0.007439 ** 
    ## lag(value, 35)                                       2.623 0.008800 ** 
    ## lag(value, 42)                                       1.652 0.098818 .  
    ## lag(value, 49)                                       3.897 0.000102 ***
    ## month(date, label = TRUE).L                         -1.958 0.050377 .  
    ## month(date, label = TRUE).Q                          0.435 0.663884    
    ## month(date, label = TRUE).C                         -2.019 0.043673 *  
    ## month(date, label = TRUE)^4                         -1.382 0.167211    
    ## month(date, label = TRUE)^5                         -2.356 0.018624 *  
    ## month(date, label = TRUE)^6                         -0.170 0.865255    
    ## month(date, label = TRUE)^7                         -1.690 0.091302 .  
    ## month(date, label = TRUE)^8                         -0.430 0.667070    
    ## month(date, label = TRUE)^9                          0.805 0.420942    
    ## month(date, label = TRUE)^10                         0.979 0.327529    
    ## month(date, label = TRUE)^11                        -1.175 0.240069    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.768 2.05e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.723 0.006543 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.83 on 1419 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2594, Adjusted R-squared:  0.2479 
    ## F-statistic: 22.59 on 22 and 1419 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,457 × 2]> <tibble [28 × 2]> <split [1429|28]>
    ## 2 healthyR      <tibble [1,450 × 2]> <tibble [28 × 2]> <split [1422|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,396 × 2]> <tibble [28 × 2]> <split [1368|28]>
    ## 5 healthyverse  <tibble [1,367 × 2]> <tibble [28 × 2]> <split [1339|28]>
    ## 6 healthyR.ai   <tibble [1,193 × 2]> <tibble [28 × 2]> <split [1165|28]>
    ## 7 TidyDensity   <tibble [1,047 × 2]> <tibble [28 × 2]> <split [1019|28]>
    ## 8 tidyAML       <tibble [663 × 2]>   <tibble [28 × 2]> <split [635|28]> 
    ## 9 RandomWalker  <tibble [97 × 2]>    <tibble [28 × 2]> <split [69|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7069514 | 226.63275 | 0.6108120 | 139.72059 | 0.8855776 | 0.1034385 |
| healthyR.data | 2 | LM | Test | 0.7517657 | 289.27313 | 0.6495319 | 138.71773 | 0.9076054 | 0.0035976 |
| healthyR.data | 3 | EARTH | Test | 0.7852993 | 363.32868 | 0.6785052 | 136.20367 | 0.9117815 | 0.0035976 |
| healthyR.data | 4 | NNAR | Test | 0.7749022 | 129.21463 | 0.6695220 | 161.16441 | 1.0387267 | 0.0203062 |
| healthyR | 1 | ARIMA | Test | 0.6014750 | 111.47171 | 0.6743956 | 150.76957 | 0.7341128 | 0.1020452 |
| healthyR | 2 | LM | Test | 0.6211649 | 105.81678 | 0.6964725 | 187.62582 | 0.7474582 | 0.0000454 |
| healthyR | 3 | EARTH | Test | 0.6785636 | 193.63702 | 0.7608301 | 142.80425 | 0.8141230 | 0.0000454 |
| healthyR | 4 | NNAR | Test | 0.5722804 | 118.62045 | 0.6416615 | 145.29874 | 0.7228345 | 0.0666906 |
| healthyR.ts | 1 | ARIMA | Test | 0.8061262 | 109.90561 | 0.8277587 | 118.13058 | 0.9537230 | 0.0118970 |
| healthyR.ts | 2 | LM | Test | 0.8045831 | 107.92979 | 0.8261743 | 119.14742 | 0.9504058 | 0.0118970 |
| healthyR.ts | 3 | EARTH | Test | 0.8055865 | 109.23587 | 0.8272046 | 118.48774 | 0.9523861 | 0.0118970 |
| healthyR.ts | 4 | NNAR | Test | 0.7706279 | 91.29170 | 0.7913079 | 172.48334 | 0.9350570 | 0.0719734 |
| healthyverse | 1 | ARIMA | Test | 0.4450707 | 182.22399 | 0.8212745 | 86.03869 | 0.5625036 | 0.4582053 |
| healthyverse | 2 | LM | Test | 0.5118291 | 331.68250 | 0.9444616 | 84.41812 | 0.6166507 | 0.0322534 |
| healthyverse | 3 | EARTH | Test | 0.5516598 | 193.97445 | 1.0179599 | 104.81682 | 0.6813420 | 0.0322534 |
| healthyverse | 4 | NNAR | Test | 0.5269461 | 198.74693 | 0.9723566 | 101.76820 | 0.6524729 | 0.0624657 |
| healthyR.ai | 1 | ARIMA | Test | 0.6750117 | 92.43882 | 0.8870607 | 159.10510 | 0.7786788 | 0.2138510 |
| healthyR.ai | 2 | LM | Test | 0.7102719 | 122.22461 | 0.9333977 | 164.53041 | 0.7929696 | 0.0035766 |
| healthyR.ai | 3 | EARTH | Test | 0.7602140 | 134.86853 | 0.9990286 | 147.26023 | 0.8986493 | 0.0035766 |
| healthyR.ai | 4 | NNAR | Test | 0.6609908 | 115.23584 | 0.8686354 | 152.58484 | 0.7562590 | 0.1114253 |
| TidyDensity | 1 | ARIMA | Test | 0.6300311 | 206.86052 | 0.7352939 | 110.53542 | 0.7663903 | 0.2131047 |
| TidyDensity | 2 | LM | Test | 0.7185344 | 292.21066 | 0.8385839 | 109.01320 | 0.8476893 | 0.0133328 |
| TidyDensity | 3 | EARTH | Test | 0.6781592 | 207.28221 | 0.7914630 | 115.15427 | 0.8215938 | 0.0133328 |
| TidyDensity | 4 | NNAR | Test | 0.6744571 | 122.22439 | 0.7871423 | 144.61719 | 0.8486001 | 0.0650539 |
| tidyAML | 1 | ARIMA | Test | 0.7081969 | 108.23769 | 0.9652217 | 102.52648 | 0.8691108 | 0.2154771 |
| tidyAML | 2 | LM | Test | 0.7391676 | 119.13456 | 1.0074326 | 96.46124 | 0.9116895 | 0.0009271 |
| tidyAML | 3 | EARTH | Test | 0.7221829 | 159.07156 | 0.9842837 | 90.18683 | 0.8279810 | 0.0009271 |
| tidyAML | 4 | NNAR | Test | 0.7470499 | 149.89099 | 1.0181756 | 98.43391 | 0.8552842 | 0.0047523 |
| RandomWalker | 1 | ARIMA | Test | 1.3999348 | 100.00000 | 0.6274977 | 200.00000 | 1.4981629 | NA |
| RandomWalker | 2 | LM | Test | 1.4233980 | 102.35026 | 0.6380147 | 192.24072 | 1.5164417 | 0.0003893 |
| RandomWalker | 3 | EARTH | Test | 1.2476257 | 89.32990 | 0.5592276 | 110.06510 | 1.5516018 | 0.0003893 |
| RandomWalker | 4 | NNAR | Test | 3.9247831 | 353.98667 | 1.7592193 | 138.34558 | 6.2173949 | 0.1777868 |

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
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         1 ARIMA       Test  0.707 227.  0.611 140.  0.886  1.03e-1
    ## 2 healthyR            4 NNAR        Test  0.572 119.  0.642 145.  0.723  6.67e-2
    ## 3 healthyR.ts         4 NNAR        Test  0.771  91.3 0.791 172.  0.935  7.20e-2
    ## 4 healthyver…         1 ARIMA       Test  0.445 182.  0.821  86.0 0.563  4.58e-1
    ## 5 healthyR.ai         4 NNAR        Test  0.661 115.  0.869 153.  0.756  1.11e-1
    ## 6 TidyDensity         1 ARIMA       Test  0.630 207.  0.735 111.  0.766  2.13e-1
    ## 7 tidyAML             3 EARTH       Test  0.722 159.  0.984  90.2 0.828  9.27e-4
    ## 8 RandomWalk…         1 ARIMA       Test  1.40  100   0.627 200   1.50  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1429|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1422|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1368|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1339|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1165|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1019|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [635|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [69|28]>   <mdl_tm_t [1 × 5]>

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
