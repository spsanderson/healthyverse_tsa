Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 123,010
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

The last day in the data set is 2024-12-04 23:58:39, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -2868.38
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 123010        |
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
| r_version     |     87062 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     87062 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     87062 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10560 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-04 | 2023-03-27 | 1473 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1153904.16 | 1540088.7 | 355 | 14701 | 260378 | 2368021 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10352.49 | 17992.4 | 1 | 319 | 3100 | 11871 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-04 23:58:39 | 2023-03-27 14:12:59 | 74500 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   21.5 |       60 |

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
    ## -156.13  -34.57   -9.23   26.81  801.18 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.692e+02  8.165e+01
    ## date                                                1.023e-02  4.327e-03
    ## lag(value, 1)                                       1.361e-01  2.568e-02
    ## lag(value, 7)                                       9.629e-02  2.661e-02
    ## lag(value, 14)                                      1.055e-01  2.665e-02
    ## lag(value, 21)                                      3.699e-02  2.685e-02
    ## lag(value, 28)                                      7.870e-02  2.673e-02
    ## lag(value, 35)                                      6.886e-02  2.691e-02
    ## lag(value, 42)                                      3.520e-02  2.702e-02
    ## lag(value, 49)                                      1.116e-01  2.680e-02
    ## month(date, label = TRUE).L                        -1.145e+01  5.571e+00
    ## month(date, label = TRUE).Q                         1.307e+00  5.469e+00
    ## month(date, label = TRUE).C                        -1.183e+01  5.533e+00
    ## month(date, label = TRUE)^4                        -8.189e+00  5.453e+00
    ## month(date, label = TRUE)^5                        -1.303e+01  5.370e+00
    ## month(date, label = TRUE)^6                        -1.258e+00  5.406e+00
    ## month(date, label = TRUE)^7                        -9.085e+00  5.286e+00
    ## month(date, label = TRUE)^8                        -2.388e+00  5.275e+00
    ## month(date, label = TRUE)^9                         4.238e+00  5.264e+00
    ## month(date, label = TRUE)^10                        5.132e+00  5.264e+00
    ## month(date, label = TRUE)^11                       -6.159e+00  5.277e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.173e+01  2.453e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.657e+00  2.563e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.072 0.038402 *  
    ## date                                                 2.363 0.018241 *  
    ## lag(value, 1)                                        5.298 1.36e-07 ***
    ## lag(value, 7)                                        3.619 0.000307 ***
    ## lag(value, 14)                                       3.959 7.91e-05 ***
    ## lag(value, 21)                                       1.378 0.168544    
    ## lag(value, 28)                                       2.945 0.003286 ** 
    ## lag(value, 35)                                       2.559 0.010611 *  
    ## lag(value, 42)                                       1.303 0.192932    
    ## lag(value, 49)                                       4.166 3.30e-05 ***
    ## month(date, label = TRUE).L                         -2.056 0.040006 *  
    ## month(date, label = TRUE).Q                          0.239 0.811147    
    ## month(date, label = TRUE).C                         -2.138 0.032669 *  
    ## month(date, label = TRUE)^4                         -1.502 0.133401    
    ## month(date, label = TRUE)^5                         -2.426 0.015401 *  
    ## month(date, label = TRUE)^6                         -0.233 0.816075    
    ## month(date, label = TRUE)^7                         -1.719 0.085888 .  
    ## month(date, label = TRUE)^8                         -0.453 0.650911    
    ## month(date, label = TRUE)^9                          0.805 0.420985    
    ## month(date, label = TRUE)^10                         0.975 0.329742    
    ## month(date, label = TRUE)^11                        -1.167 0.243359    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.783 1.90e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.597 0.009499 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.9 on 1401 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2603, Adjusted R-squared:  0.2486 
    ## F-statistic: 22.41 on 22 and 1401 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,439 × 2]> <tibble [28 × 2]> <split [1411|28]>
    ## 2 healthyR      <tibble [1,432 × 2]> <tibble [28 × 2]> <split [1404|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,378 × 2]> <tibble [28 × 2]> <split [1350|28]>
    ## 5 healthyverse  <tibble [1,349 × 2]> <tibble [28 × 2]> <split [1321|28]>
    ## 6 healthyR.ai   <tibble [1,175 × 2]> <tibble [28 × 2]> <split [1147|28]>
    ## 7 TidyDensity   <tibble [1,029 × 2]> <tibble [28 × 2]> <split [1001|28]>
    ## 8 tidyAML       <tibble [645 × 2]>   <tibble [28 × 2]> <split [617|28]> 
    ## 9 RandomWalker  <tibble [79 × 2]>    <tibble [28 × 2]> <split [51|28]>

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

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7953330 | 117.9838 | 0.6486528 | 173.79994 | 0.9925508 | 0.0104761 |
| healthyR.data | 2 | LM | Test | 0.8071162 | 200.2470 | 0.6582628 | 142.08977 | 0.9300555 | 0.0000188 |
| healthyR.data | 3 | EARTH | Test | 0.7975036 | 184.9279 | 0.6504230 | 144.04560 | 0.9391170 | 0.0000188 |
| healthyR.data | 4 | NNAR | Test | 0.8346279 | 107.6474 | 0.6807006 | 158.32724 | 1.0766715 | 0.0198736 |
| healthyR | 1 | ARIMA | Test | 0.6743449 | 200.6899 | 0.7899513 | 150.56148 | 0.7931034 | 0.1534647 |
| healthyR | 2 | LM | Test | 0.6753260 | 112.4584 | 0.7911006 | 185.75795 | 0.8068757 | 0.0055832 |
| healthyR | 3 | EARTH | Test | 0.7094652 | 222.5021 | 0.8310924 | 150.64585 | 0.8314252 | 0.0055832 |
| healthyR | 4 | NNAR | Test | 0.6683911 | 153.5695 | 0.7829768 | 164.62426 | 0.7927710 | 0.0415133 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 1.0466478 | 162.2045 | 0.8203471 | 125.35797 | 1.2242742 | 0.0051740 |
| healthyR.ts | 2 | LM | Test | 0.9907322 | 129.5285 | 0.7765212 | 135.97280 | 1.1353240 | 0.0051740 |
| healthyR.ts | 3 | EARTH | Test | 1.1726375 | 210.6495 | 0.9190959 | 119.91059 | 1.4292672 | 0.0051740 |
| healthyR.ts | 4 | NNAR | Test | 1.0498428 | 140.6416 | 0.8228512 | 193.22337 | 1.1662576 | 0.0061469 |
| healthyverse | 1 | ARIMA | Test | 0.4829072 | 138.8801 | 0.8989921 | 103.48022 | 0.5668561 | 0.2932968 |
| healthyverse | 2 | LM | Test | 0.4817149 | 259.8594 | 0.8967726 | 82.05342 | 0.5881476 | 0.0125360 |
| healthyverse | 3 | EARTH | Test | 0.4874038 | 251.4613 | 0.9073631 | 86.65017 | 0.5708858 | 0.0125360 |
| healthyverse | 4 | NNAR | Test | 0.5533582 | 160.0280 | 1.0301454 | 112.67268 | 0.6275647 | 0.0091265 |
| healthyR.ai | 1 | ARIMA | Test | 0.8091738 | 130.8551 | 0.9249631 | 160.82654 | 0.9258440 | 0.0194515 |
| healthyR.ai | 2 | LM | Test | 0.7534868 | 117.3024 | 0.8613076 | 158.95463 | 0.8515158 | 0.0031296 |
| healthyR.ai | 3 | EARTH | Test | 0.8310481 | 151.5167 | 0.9499675 | 153.11254 | 0.9587464 | 0.0031296 |
| healthyR.ai | 4 | NNAR | Test | 0.8267295 | 119.7375 | 0.9450310 | 178.00356 | 0.9544383 | 0.1334999 |
| TidyDensity | 1 | ARIMA | Test | 0.8148520 | 199.8975 | 0.7263729 | 125.64510 | 0.9506944 | 0.0045632 |
| TidyDensity | 2 | LM | Test | 0.8413843 | 203.8405 | 0.7500242 | 125.32557 | 0.9787609 | 0.0078707 |
| TidyDensity | 3 | EARTH | Test | 0.8021502 | 181.2006 | 0.7150503 | 127.36408 | 0.9314390 | 0.0078707 |
| TidyDensity | 4 | NNAR | Test | 0.8019512 | 165.2960 | 0.7148729 | 153.64365 | 0.9338911 | 0.0028520 |
| tidyAML | 1 | ARIMA | Test | 0.6067034 | 137.9072 | 0.6705349 | 95.36810 | 0.7696412 | 0.2993483 |
| tidyAML | 2 | LM | Test | 0.6228429 | 161.4556 | 0.6883724 | 90.36966 | 0.7861128 | 0.0164343 |
| tidyAML | 3 | EARTH | Test | 0.6048189 | 214.9185 | 0.6684521 | 81.83106 | 0.7490727 | 0.0164343 |
| tidyAML | 4 | NNAR | Test | 0.5878172 | 165.3617 | 0.6496617 | 87.07486 | 0.7335519 | 0.1428902 |
| RandomWalker | 1 | ARIMA | Test | 1.2920475 | 105.3943 | 0.6038073 | 191.70642 | 1.4322380 | 0.0024889 |
| RandomWalker | 2 | LM | Test | 1.2832923 | 101.3069 | 0.5997158 | 187.04517 | 1.4361322 | 0.0027898 |
| RandomWalker | 3 | EARTH | Test | 1.7503771 | 218.0365 | 0.8179967 | 142.01330 | 2.1025146 | 0.0027898 |
| RandomWalker | 4 | NNAR | Test | 1.4516159 | 150.6165 | 0.6783778 | 145.87223 | 1.7145910 | 0.0007377 |

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
    ## 1 healthyR.da…         2 LM          Test  0.807  200. 0.658 142.  0.930 1.88e-5
    ## 2 healthyR             4 NNAR        Test  0.668  154. 0.783 165.  0.793 4.15e-2
    ## 3 healthyR.ts          2 LM          Test  0.991  130. 0.777 136.  1.14  5.17e-3
    ## 4 healthyverse         1 ARIMA       Test  0.483  139. 0.899 103.  0.567 2.93e-1
    ## 5 healthyR.ai          2 LM          Test  0.753  117. 0.861 159.  0.852 3.13e-3
    ## 6 TidyDensity          3 EARTH       Test  0.802  181. 0.715 127.  0.931 7.87e-3
    ## 7 tidyAML              4 NNAR        Test  0.588  165. 0.650  87.1 0.734 1.43e-1
    ## 8 RandomWalker         1 ARIMA       Test  1.29   105. 0.604 192.  1.43  2.49e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1411|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1404|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1350|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1321|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1147|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1001|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [617|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [51|28]>   <mdl_tm_t [1 × 5]>

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
