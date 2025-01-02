Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
02 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 125,482
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

The last day in the data set is 2024-12-31 19:58:01, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.1904^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 125482        |
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
| r_version     |     89083 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89083 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89083 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10753 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-31 | 2023-04-13 | 1500 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1146303.96 | 1534997.06 | 355 | 14701 | 259000.5 | 2367964 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10327.52 | 17964.18 | 1 | 315 | 3098.0 | 11854 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-31 19:58:01 | 2023-04-13 08:26:36 | 75973 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     33 |       60 |

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
    ## -156.01  -34.94   -9.71   27.06  803.94 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.788e+02  7.956e+01
    ## date                                                1.074e-02  4.215e-03
    ## lag(value, 1)                                       1.325e-01  2.549e-02
    ## lag(value, 7)                                       8.979e-02  2.643e-02
    ## lag(value, 14)                                      1.086e-01  2.644e-02
    ## lag(value, 21)                                      4.606e-02  2.656e-02
    ## lag(value, 28)                                      6.931e-02  2.638e-02
    ## lag(value, 35)                                      7.239e-02  2.650e-02
    ## lag(value, 42)                                      4.548e-02  2.658e-02
    ## lag(value, 49)                                      1.022e-01  2.648e-02
    ## month(date, label = TRUE).L                        -1.102e+01  5.461e+00
    ## month(date, label = TRUE).Q                         2.030e+00  5.283e+00
    ## month(date, label = TRUE).C                        -1.126e+01  5.345e+00
    ## month(date, label = TRUE)^4                        -7.655e+00  5.343e+00
    ## month(date, label = TRUE)^5                        -1.275e+01  5.307e+00
    ## month(date, label = TRUE)^6                        -1.003e+00  5.376e+00
    ## month(date, label = TRUE)^7                        -9.013e+00  5.269e+00
    ## month(date, label = TRUE)^8                        -2.265e+00  5.264e+00
    ## month(date, label = TRUE)^9                         4.222e+00  5.255e+00
    ## month(date, label = TRUE)^10                        5.144e+00  5.254e+00
    ## month(date, label = TRUE)^11                       -6.236e+00  5.268e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.154e+01  2.428e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.954e+00  2.544e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.248 0.024731 *  
    ## date                                                 2.549 0.010903 *  
    ## lag(value, 1)                                        5.198 2.30e-07 ***
    ## lag(value, 7)                                        3.398 0.000698 ***
    ## lag(value, 14)                                       4.107 4.23e-05 ***
    ## lag(value, 21)                                       1.734 0.083151 .  
    ## lag(value, 28)                                       2.627 0.008697 ** 
    ## lag(value, 35)                                       2.732 0.006379 ** 
    ## lag(value, 42)                                       1.711 0.087243 .  
    ## lag(value, 49)                                       3.858 0.000119 ***
    ## month(date, label = TRUE).L                         -2.018 0.043744 *  
    ## month(date, label = TRUE).Q                          0.384 0.700807    
    ## month(date, label = TRUE).C                         -2.107 0.035262 *  
    ## month(date, label = TRUE)^4                         -1.433 0.152139    
    ## month(date, label = TRUE)^5                         -2.403 0.016399 *  
    ## month(date, label = TRUE)^6                         -0.187 0.851966    
    ## month(date, label = TRUE)^7                         -1.710 0.087412 .  
    ## month(date, label = TRUE)^8                         -0.430 0.667117    
    ## month(date, label = TRUE)^9                          0.803 0.421851    
    ## month(date, label = TRUE)^10                         0.979 0.327765    
    ## month(date, label = TRUE)^11                        -1.184 0.236682    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.751 2.22e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.733 0.006346 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.79 on 1428 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2591, Adjusted R-squared:  0.2477 
    ## F-statistic:  22.7 on 22 and 1428 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,465 × 2]> <tibble [28 × 2]> <split [1437|28]>
    ## 2 healthyR      <tibble [1,458 × 2]> <tibble [28 × 2]> <split [1430|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,404 × 2]> <tibble [28 × 2]> <split [1376|28]>
    ## 5 healthyverse  <tibble [1,375 × 2]> <tibble [28 × 2]> <split [1347|28]>
    ## 6 healthyR.ai   <tibble [1,201 × 2]> <tibble [28 × 2]> <split [1173|28]>
    ## 7 TidyDensity   <tibble [1,055 × 2]> <tibble [28 × 2]> <split [1027|28]>
    ## 8 tidyAML       <tibble [671 × 2]>   <tibble [28 × 2]> <split [643|28]> 
    ## 9 RandomWalker  <tibble [105 × 2]>   <tibble [28 × 2]> <split [77|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7370056 | 300.38516 | 0.6187051 | 143.52714 | 0.8703176 | 0.1506466 |
| healthyR.data | 2 | LM | Test | 0.7509348 | 315.40242 | 0.6303984 | 139.20847 | 0.9088496 | 0.0058560 |
| healthyR.data | 3 | EARTH | Test | 0.8233573 | 491.56226 | 0.6911961 | 132.77194 | 0.9267635 | 0.0058560 |
| healthyR.data | 4 | NNAR | Test | 0.7841979 | 149.64100 | 0.6583223 | 168.81556 | 1.0434876 | 0.0003345 |
| healthyR | 1 | ARIMA | Test | 0.6770870 | 148.18291 | 0.6711201 | 167.99908 | 0.8041554 | 0.0166807 |
| healthyR | 2 | LM | Test | 0.6437598 | 102.55303 | 0.6380866 | 183.48952 | 0.7933364 | 0.0306616 |
| healthyR | 3 | EARTH | Test | 0.6335840 | 97.46632 | 0.6280004 | 170.63543 | 0.7896106 | 0.0306616 |
| healthyR | 4 | NNAR | Test | 0.6338988 | 111.71083 | 0.6283124 | 152.87747 | 0.7976687 | 0.0002838 |
| healthyR.ts | 1 | ARIMA | Test | 0.8521678 | 142.18998 | 0.7402439 | 129.70453 | 1.0335621 | 0.0496763 |
| healthyR.ts | 2 | LM | Test | 0.8744885 | 162.45389 | 0.7596330 | 128.13568 | 1.0454450 | 0.0496763 |
| healthyR.ts | 3 | EARTH | Test | 0.8716303 | 160.02647 | 0.7571502 | 128.22952 | 1.0435462 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8286558 | 97.11618 | 0.7198200 | 182.81969 | 1.0207911 | 0.1410527 |
| healthyverse | 1 | ARIMA | Test | 0.4677785 | 190.95584 | 0.7293668 | 88.63079 | 0.5870362 | 0.3639130 |
| healthyverse | 2 | LM | Test | 0.5415733 | 303.76238 | 0.8444288 | 86.85028 | 0.6662523 | 0.0041130 |
| healthyverse | 3 | EARTH | Test | 0.5874384 | 342.88147 | 0.9159424 | 88.31742 | 0.7061765 | 0.0041130 |
| healthyverse | 4 | NNAR | Test | 0.5619006 | 196.24424 | 0.8761235 | 104.84017 | 0.7055260 | 0.0023792 |
| healthyR.ai | 1 | ARIMA | Test | 0.6672280 | 95.58072 | 0.8576192 | 173.23733 | 0.7520115 | 0.2637347 |
| healthyR.ai | 2 | LM | Test | 0.6732863 | 95.52216 | 0.8654063 | 149.55034 | 0.7765974 | 0.0310352 |
| healthyR.ai | 3 | EARTH | Test | 0.6408357 | 93.50257 | 0.8236960 | 129.59149 | 0.7706286 | 0.0310352 |
| healthyR.ai | 4 | NNAR | Test | 0.6566260 | 92.20792 | 0.8439920 | 153.02170 | 0.7520843 | 0.2091970 |
| TidyDensity | 1 | ARIMA | Test | 0.6993874 | 223.66884 | 0.8686586 | 128.98621 | 0.8032595 | 0.0233926 |
| TidyDensity | 2 | LM | Test | 0.7482606 | 320.37387 | 0.9293605 | 119.60583 | 0.8471560 | 0.1468764 |
| TidyDensity | 3 | EARTH | Test | 0.6767300 | 202.53196 | 0.8405175 | 125.94182 | 0.8139787 | 0.1468764 |
| TidyDensity | 4 | NNAR | Test | 0.6533060 | 107.81873 | 0.8114242 | 140.57920 | 0.8402786 | 0.0847273 |
| tidyAML | 1 | ARIMA | Test | 0.8679634 | 131.38988 | 0.9812169 | 106.87272 | 1.0186511 | 0.1149594 |
| tidyAML | 2 | LM | Test | 0.9233786 | 134.80373 | 1.0438628 | 111.27673 | 1.0616752 | 0.1434433 |
| tidyAML | 3 | EARTH | Test | 0.8635901 | 171.69626 | 0.9762731 | 98.57775 | 0.9494907 | 0.1434433 |
| tidyAML | 4 | NNAR | Test | 0.8738116 | 149.03978 | 0.9878283 | 102.65350 | 0.9743177 | 0.1286960 |
| RandomWalker | 1 | ARIMA | Test | 0.7110807 | 77.37672 | 0.3278126 | 73.70642 | 0.8584861 | 0.7400614 |
| RandomWalker | 2 | LM | Test | 1.2957958 | 95.55808 | 0.5973698 | 176.41771 | 1.4268649 | 0.0018378 |
| RandomWalker | 3 | EARTH | Test | 1.2155241 | 132.48561 | 0.5603640 | 103.57147 | 1.5777446 | 0.0018378 |
| RandomWalker | 4 | NNAR | Test | 1.6592559 | 140.48001 | 0.7649271 | 143.23815 | 2.0175367 | 0.0000001 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.737 300.  0.619 144.  0.870 0.151 
    ## 2 healthyR              3 EARTH       Test  0.634  97.5 0.628 171.  0.790 0.0307
    ## 3 healthyR.ts           4 NNAR        Test  0.829  97.1 0.720 183.  1.02  0.141 
    ## 4 healthyverse          1 ARIMA       Test  0.468 191.  0.729  88.6 0.587 0.364 
    ## 5 healthyR.ai           1 ARIMA       Test  0.667  95.6 0.858 173.  0.752 0.264 
    ## 6 TidyDensity           1 ARIMA       Test  0.699 224.  0.869 129.  0.803 0.0234
    ## 7 tidyAML               3 EARTH       Test  0.864 172.  0.976  98.6 0.949 0.143 
    ## 8 RandomWalker          1 ARIMA       Test  0.711  77.4 0.328  73.7 0.858 0.740

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1437|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1430|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1376|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1347|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1173|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1027|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [643|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [77|28]>   <mdl_tm_t [1 × 5]>

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
