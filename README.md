Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
14 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,923
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

The last day in the data set is 2025-01-12 23:50:04, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3804.24
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126923        |
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
| r_version     |     90304 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     90304 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     90304 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10795 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-12 | 2023-04-19 | 1512 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144099.12 | 1533052.51 | 355 | 14701 | 260378 | 2367916 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10340.85 | 18019.96 | 1 | 317 | 3091 | 11830 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-12 23:50:04 | 2023-04-19 16:41:20 | 76832 |

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
    ## -155.00  -35.16   -9.88   27.08  806.15 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.865e+02  7.877e+01
    ## date                                                1.118e-02  4.174e-03
    ## lag(value, 1)                                       1.256e-01  2.538e-02
    ## lag(value, 7)                                       9.049e-02  2.632e-02
    ## lag(value, 14)                                      1.069e-01  2.644e-02
    ## lag(value, 21)                                      4.898e-02  2.655e-02
    ## lag(value, 28)                                      6.659e-02  2.639e-02
    ## lag(value, 35)                                      7.195e-02  2.651e-02
    ## lag(value, 42)                                      5.156e-02  2.658e-02
    ## lag(value, 49)                                      9.369e-02  2.643e-02
    ## month(date, label = TRUE).L                        -1.115e+01  5.389e+00
    ## month(date, label = TRUE).Q                         2.080e+00  5.237e+00
    ## month(date, label = TRUE).C                        -1.129e+01  5.309e+00
    ## month(date, label = TRUE)^4                        -7.618e+00  5.323e+00
    ## month(date, label = TRUE)^5                        -1.290e+01  5.307e+00
    ## month(date, label = TRUE)^6                        -9.577e-01  5.385e+00
    ## month(date, label = TRUE)^7                        -9.078e+00  5.284e+00
    ## month(date, label = TRUE)^8                        -2.224e+00  5.280e+00
    ## month(date, label = TRUE)^9                         4.163e+00  5.271e+00
    ## month(date, label = TRUE)^10                        5.185e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -6.267e+00  5.284e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.170e+01  2.428e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.304e+00  2.544e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.367 0.018054 *  
    ## date                                                 2.679 0.007458 ** 
    ## lag(value, 1)                                        4.948 8.39e-07 ***
    ## lag(value, 7)                                        3.438 0.000603 ***
    ## lag(value, 14)                                       4.041 5.60e-05 ***
    ## lag(value, 21)                                       1.845 0.065230 .  
    ## lag(value, 28)                                       2.523 0.011746 *  
    ## lag(value, 35)                                       2.714 0.006732 ** 
    ## lag(value, 42)                                       1.940 0.052630 .  
    ## lag(value, 49)                                       3.545 0.000405 ***
    ## month(date, label = TRUE).L                         -2.069 0.038731 *  
    ## month(date, label = TRUE).Q                          0.397 0.691327    
    ## month(date, label = TRUE).C                         -2.127 0.033616 *  
    ## month(date, label = TRUE)^4                         -1.431 0.152614    
    ## month(date, label = TRUE)^5                         -2.430 0.015202 *  
    ## month(date, label = TRUE)^6                         -0.178 0.858866    
    ## month(date, label = TRUE)^7                         -1.718 0.086001 .  
    ## month(date, label = TRUE)^8                         -0.421 0.673637    
    ## month(date, label = TRUE)^9                          0.790 0.429769    
    ## month(date, label = TRUE)^10                         0.984 0.325391    
    ## month(date, label = TRUE)^11                        -1.186 0.235831    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.820 1.59e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.871 0.004156 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.98 on 1440 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2582, Adjusted R-squared:  0.2469 
    ## F-statistic: 22.79 on 22 and 1440 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,477 × 2]> <tibble [28 × 2]> <split [1449|28]>
    ## 2 healthyR      <tibble [1,470 × 2]> <tibble [28 × 2]> <split [1442|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,416 × 2]> <tibble [28 × 2]> <split [1388|28]>
    ## 5 healthyverse  <tibble [1,387 × 2]> <tibble [28 × 2]> <split [1359|28]>
    ## 6 healthyR.ai   <tibble [1,213 × 2]> <tibble [28 × 2]> <split [1185|28]>
    ## 7 TidyDensity   <tibble [1,067 × 2]> <tibble [28 × 2]> <split [1039|28]>
    ## 8 tidyAML       <tibble [683 × 2]>   <tibble [28 × 2]> <split [655|28]> 
    ## 9 RandomWalker  <tibble [117 × 2]>   <tibble [28 × 2]> <split [89|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8849495 | 250.93923 | 0.6914091 | 161.03448 | 1.0247318 | 0.0000755 |
| healthyR.data | 2 | LM | Test | 0.8689510 | 288.30872 | 0.6789095 | 154.99806 | 1.0118675 | 0.0073288 |
| healthyR.data | 3 | EARTH | Test | 0.9616631 | 417.57804 | 0.7513453 | 149.38158 | 1.0665283 | 0.0073288 |
| healthyR.data | 4 | NNAR | Test | 0.7834249 | 117.77764 | 0.6120882 | 158.60688 | 1.0489710 | 0.0240517 |
| healthyR | 1 | ARIMA | Test | 0.7090299 | 115.65693 | 0.6518275 | 165.71382 | 0.8872850 | 0.0304722 |
| healthyR | 2 | LM | Test | 0.6970133 | 99.48535 | 0.6407803 | 181.73820 | 0.8950759 | 0.0094764 |
| healthyR | 3 | EARTH | Test | 0.6924394 | 102.87937 | 0.6365754 | 161.92239 | 0.8952569 | 0.0094764 |
| healthyR | 4 | NNAR | Test | 0.7083791 | 112.75652 | 0.6512292 | 152.97163 | 0.8992429 | 0.0062786 |
| healthyR.ts | 1 | ARIMA | Test | 0.8537365 | 181.05917 | 0.6133924 | 125.06960 | 1.0865843 | 0.0106419 |
| healthyR.ts | 2 | LM | Test | 0.8714157 | 204.93047 | 0.6260945 | 123.91125 | 1.0934920 | 0.0080293 |
| healthyR.ts | 3 | EARTH | Test | 0.8751762 | 209.48882 | 0.6287963 | 123.77408 | 1.0952612 | 0.0080293 |
| healthyR.ts | 4 | NNAR | Test | 0.8903859 | 101.94818 | 0.6397242 | 174.32990 | 1.1180728 | 0.0042924 |
| healthyverse | 1 | ARIMA | Test | 0.6404629 | 230.70631 | 0.7323712 | 108.16905 | 0.7706740 | 0.0189854 |
| healthyverse | 2 | LM | Test | 0.6612904 | 292.29128 | 0.7561875 | 100.91967 | 0.8107789 | 0.0039949 |
| healthyverse | 3 | EARTH | Test | 0.7159526 | 383.90833 | 0.8186938 | 97.91176 | 0.8944623 | 0.0039949 |
| healthyverse | 4 | NNAR | Test | 0.6351780 | 182.47599 | 0.7263278 | 118.14852 | 0.7654400 | 0.0409340 |
| healthyR.ai | 1 | ARIMA | Test | 0.6840957 | 102.66859 | 0.6934236 | 179.08440 | 0.8048618 | 0.0318298 |
| healthyR.ai | 2 | LM | Test | 0.6663314 | 111.76628 | 0.6754170 | 146.80119 | 0.8187417 | 0.0006162 |
| healthyR.ai | 3 | EARTH | Test | 0.6575519 | 135.05976 | 0.6665178 | 125.94525 | 0.8390693 | 0.0006162 |
| healthyR.ai | 4 | NNAR | Test | 0.6901872 | 112.43739 | 0.6995981 | 156.51653 | 0.8243886 | 0.0004034 |
| TidyDensity | 1 | ARIMA | Test | 0.9245581 | 254.53833 | 0.9004132 | 125.14640 | 1.0831912 | 0.0451146 |
| TidyDensity | 2 | LM | Test | 0.8559445 | 221.79871 | 0.8335915 | 124.90749 | 1.0025016 | 0.0029198 |
| TidyDensity | 3 | EARTH | Test | 2.9616386 | 855.47444 | 2.8842954 | 152.68221 | 3.2710584 | 0.0029198 |
| TidyDensity | 4 | NNAR | Test | 0.7968081 | 148.46193 | 0.7759994 | 154.54909 | 0.9217236 | 0.0044920 |
| tidyAML | 1 | ARIMA | Test | 0.9129913 | 234.72377 | 0.8549500 | 108.92544 | 1.0258222 | 0.1095699 |
| tidyAML | 2 | LM | Test | 0.9899690 | 208.42172 | 0.9270340 | 120.43620 | 1.1232644 | 0.0001801 |
| tidyAML | 3 | EARTH | Test | 0.9257166 | 284.59858 | 0.8668663 | 103.81985 | 1.0288564 | 0.0001801 |
| tidyAML | 4 | NNAR | Test | 0.9369304 | 238.97411 | 0.8773672 | 110.17534 | 1.0580836 | 0.0129128 |
| RandomWalker | 1 | ARIMA | Test | 0.9952361 | 136.15756 | 0.4642372 | 104.55143 | 1.2603404 | 0.2532881 |
| RandomWalker | 2 | LM | Test | 1.2749522 | 92.76691 | 0.5947134 | 168.98505 | 1.4461437 | 0.0005634 |
| RandomWalker | 3 | EARTH | Test | 1.2722032 | 97.17848 | 0.5934311 | 164.41761 | 1.4445063 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1663341 | 133.91558 | 0.5440474 | 119.12352 | 1.4493542 | 0.0656691 |

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
    ## 1 healthyR.da…         2 LM          Test  0.869  288. 0.679  155. 1.01  0.00733
    ## 2 healthyR             1 ARIMA       Test  0.709  116. 0.652  166. 0.887 0.0305 
    ## 3 healthyR.ts          1 ARIMA       Test  0.854  181. 0.613  125. 1.09  0.0106 
    ## 4 healthyverse         4 NNAR        Test  0.635  182. 0.726  118. 0.765 0.0409 
    ## 5 healthyR.ai          1 ARIMA       Test  0.684  103. 0.693  179. 0.805 0.0318 
    ## 6 TidyDensity          4 NNAR        Test  0.797  148. 0.776  155. 0.922 0.00449
    ## 7 tidyAML              1 ARIMA       Test  0.913  235. 0.855  109. 1.03  0.110  
    ## 8 RandomWalker         1 ARIMA       Test  0.995  136. 0.464  105. 1.26  0.253

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1449|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1442|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1388|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1359|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1185|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1039|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [655|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [89|28]>   <mdl_tm_t [1 × 5]>

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
