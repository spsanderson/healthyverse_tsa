Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
27 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 125,140
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

The last day in the data set is 2024-12-25 23:17:54, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.176333^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 125140        |
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
| r_version     |     88803 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88803 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88803 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10723 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-25 | 2023-04-12 | 1494 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1147108.14 | 1535628.36 | 355 | 14701 | 258946 | 2367994 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10347.01 | 17981.23 | 1 | 317 | 3098 | 11891 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-25 23:17:54 | 2023-04-12 18:31:32 | 75742 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     30 |       60 |

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
    ## -155.85  -34.81   -9.67   27.10  803.61 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.864e+02  8.008e+01
    ## date                                                1.114e-02  4.243e-03
    ## lag(value, 1)                                       1.327e-01  2.555e-02
    ## lag(value, 7)                                       9.015e-02  2.649e-02
    ## lag(value, 14)                                      1.087e-01  2.650e-02
    ## lag(value, 21)                                      4.489e-02  2.664e-02
    ## lag(value, 28)                                      6.922e-02  2.645e-02
    ## lag(value, 35)                                      7.200e-02  2.658e-02
    ## lag(value, 42)                                      4.433e-02  2.670e-02
    ## lag(value, 49)                                      1.024e-01  2.658e-02
    ## month(date, label = TRUE).L                        -1.058e+01  5.486e+00
    ## month(date, label = TRUE).Q                         2.593e+00  5.321e+00
    ## month(date, label = TRUE).C                        -1.067e+01  5.386e+00
    ## month(date, label = TRUE)^4                        -7.228e+00  5.368e+00
    ## month(date, label = TRUE)^5                        -1.244e+01  5.324e+00
    ## month(date, label = TRUE)^6                        -8.106e-01  5.387e+00
    ## month(date, label = TRUE)^7                        -8.900e+00  5.278e+00
    ## month(date, label = TRUE)^8                        -2.217e+00  5.272e+00
    ## month(date, label = TRUE)^9                         4.238e+00  5.262e+00
    ## month(date, label = TRUE)^10                        5.144e+00  5.261e+00
    ## month(date, label = TRUE)^11                       -6.217e+00  5.275e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.164e+01  2.437e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.023e+00  2.550e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.327 0.020109 *  
    ## date                                                 2.626 0.008725 ** 
    ## lag(value, 1)                                        5.195 2.35e-07 ***
    ## lag(value, 7)                                        3.404 0.000684 ***
    ## lag(value, 14)                                       4.103 4.30e-05 ***
    ## lag(value, 21)                                       1.685 0.092283 .  
    ## lag(value, 28)                                       2.617 0.008975 ** 
    ## lag(value, 35)                                       2.709 0.006837 ** 
    ## lag(value, 42)                                       1.661 0.097009 .  
    ## lag(value, 49)                                       3.852 0.000122 ***
    ## month(date, label = TRUE).L                         -1.929 0.053962 .  
    ## month(date, label = TRUE).Q                          0.487 0.626170    
    ## month(date, label = TRUE).C                         -1.982 0.047691 *  
    ## month(date, label = TRUE)^4                         -1.346 0.178400    
    ## month(date, label = TRUE)^5                         -2.336 0.019610 *  
    ## month(date, label = TRUE)^6                         -0.150 0.880404    
    ## month(date, label = TRUE)^7                         -1.686 0.091945 .  
    ## month(date, label = TRUE)^8                         -0.421 0.674135    
    ## month(date, label = TRUE)^9                          0.805 0.420748    
    ## month(date, label = TRUE)^10                         0.978 0.328396    
    ## month(date, label = TRUE)^11                        -1.179 0.238742    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.777 1.97e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.754 0.005970 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.87 on 1422 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2592, Adjusted R-squared:  0.2477 
    ## F-statistic: 22.62 on 22 and 1422 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,460 × 2]> <tibble [28 × 2]> <split [1432|28]>
    ## 2 healthyR      <tibble [1,453 × 2]> <tibble [28 × 2]> <split [1425|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,399 × 2]> <tibble [28 × 2]> <split [1371|28]>
    ## 5 healthyverse  <tibble [1,370 × 2]> <tibble [28 × 2]> <split [1342|28]>
    ## 6 healthyR.ai   <tibble [1,196 × 2]> <tibble [28 × 2]> <split [1168|28]>
    ## 7 TidyDensity   <tibble [1,050 × 2]> <tibble [28 × 2]> <split [1022|28]>
    ## 8 tidyAML       <tibble [666 × 2]>   <tibble [28 × 2]> <split [638|28]> 
    ## 9 RandomWalker  <tibble [100 × 2]>   <tibble [28 × 2]> <split [72|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7297196 | 276.96363 | 0.6095778 | 147.14196 | 0.8918667 | 0.1009251 |
| healthyR.data | 2 | LM | Test | 0.7659987 | 332.81048 | 0.6398838 | 142.03104 | 0.9185345 | 0.0076648 |
| healthyR.data | 3 | EARTH | Test | 0.8010126 | 422.81900 | 0.6691330 | 138.93289 | 0.9230940 | 0.0076648 |
| healthyR.data | 4 | NNAR | Test | 0.8015130 | 144.75873 | 0.6695510 | 167.39426 | 1.0622469 | 0.0000105 |
| healthyR | 1 | ARIMA | Test | 0.6598436 | 110.32131 | 0.6332850 | 151.21430 | 0.8059703 | 0.0983233 |
| healthyR | 2 | LM | Test | 0.6857162 | 105.34454 | 0.6581161 | 188.24655 | 0.8260869 | 0.0042682 |
| healthyR | 3 | EARTH | Test | 0.7355922 | 190.68061 | 0.7059846 | 145.75362 | 0.8800081 | 0.0042682 |
| healthyR | 4 | NNAR | Test | 0.6618196 | 120.29476 | 0.6351814 | 152.48582 | 0.8205381 | 0.0231837 |
| healthyR.ts | 1 | ARIMA | Test | 0.8672968 | 109.39033 | 0.7736352 | 120.68997 | 1.0434806 | 0.0141107 |
| healthyR.ts | 2 | LM | Test | 0.8659878 | 108.42895 | 0.7724677 | 120.95235 | 1.0423204 | 0.0141107 |
| healthyR.ts | 3 | EARTH | Test | 0.8679398 | 109.92595 | 0.7742088 | 120.53324 | 1.0438837 | 0.0141107 |
| healthyR.ts | 4 | NNAR | Test | 0.8569065 | 92.54113 | 0.7643670 | 176.18049 | 1.0562827 | 0.0567824 |
| healthyverse | 1 | ARIMA | Test | 0.4604434 | 194.14040 | 0.7756870 | 89.39492 | 0.5772379 | 0.3552790 |
| healthyverse | 2 | LM | Test | 0.5236106 | 327.81245 | 0.8821019 | 85.98814 | 0.6461954 | 0.0468700 |
| healthyverse | 3 | EARTH | Test | 0.5738480 | 199.84250 | 0.9667343 | 109.99424 | 0.6884863 | 0.0468700 |
| healthyverse | 4 | NNAR | Test | 0.5672537 | 194.00655 | 0.9556253 | 108.84240 | 0.6882930 | 0.0732862 |
| healthyR.ai | 1 | ARIMA | Test | 0.7011664 | 127.49907 | 0.8705455 | 166.21435 | 0.7992141 | 0.2242960 |
| healthyR.ai | 2 | LM | Test | 0.7168063 | 118.56757 | 0.8899636 | 160.19614 | 0.8152298 | 0.0181647 |
| healthyR.ai | 3 | EARTH | Test | 0.7918215 | 138.35566 | 0.9831000 | 154.89426 | 0.9114537 | 0.0181647 |
| healthyR.ai | 4 | NNAR | Test | 0.6954572 | 126.59087 | 0.8634573 | 155.04098 | 0.8007246 | 0.0514861 |
| TidyDensity | 1 | ARIMA | Test | 0.6808565 | 222.91459 | 0.7818728 | 112.55142 | 0.8206868 | 0.0492524 |
| TidyDensity | 2 | LM | Test | 0.7316442 | 289.99999 | 0.8401957 | 109.44354 | 0.8627670 | 0.0229850 |
| TidyDensity | 3 | EARTH | Test | 0.6921650 | 208.47864 | 0.7948591 | 115.21434 | 0.8375406 | 0.0229850 |
| TidyDensity | 4 | NNAR | Test | 0.7074398 | 109.56110 | 0.8124002 | 147.92897 | 0.8939919 | 0.0291969 |
| tidyAML | 1 | ARIMA | Test | 0.7778331 | 115.45236 | 0.9351013 | 112.04385 | 0.9179138 | 0.1732882 |
| tidyAML | 2 | LM | Test | 0.8086534 | 129.63063 | 0.9721531 | 105.69897 | 0.9518434 | 0.0030100 |
| tidyAML | 3 | EARTH | Test | 0.7737642 | 170.87014 | 0.9302098 | 95.20418 | 0.8634404 | 0.0030100 |
| tidyAML | 4 | NNAR | Test | 0.8120701 | 160.52334 | 0.9762606 | 105.68523 | 0.8955785 | 0.0039305 |
| RandomWalker | 1 | ARIMA | Test | 1.3713555 | 100.00000 | 0.6413126 | 200.00000 | 1.4691616 | NA |
| RandomWalker | 2 | LM | Test | 1.3824488 | 101.46662 | 0.6465004 | 197.50921 | 1.4752794 | 0.0001007 |
| RandomWalker | 3 | EARTH | Test | 1.2081404 | 84.29784 | 0.5649853 | 108.37228 | 1.5196286 | 0.0001007 |
| RandomWalker | 4 | NNAR | Test | 1.3712790 | 108.36522 | 0.6412769 | 135.44838 | 1.5905679 | 0.0310807 |

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
    ## 1 healthyR.d…         1 ARIMA       Test  0.730  277. 0.610 147.  0.892  0.101  
    ## 2 healthyR            1 ARIMA       Test  0.660  110. 0.633 151.  0.806  0.0983 
    ## 3 healthyR.ts         2 LM          Test  0.866  108. 0.772 121.  1.04   0.0141 
    ## 4 healthyver…         1 ARIMA       Test  0.460  194. 0.776  89.4 0.577  0.355  
    ## 5 healthyR.ai         1 ARIMA       Test  0.701  127. 0.871 166.  0.799  0.224  
    ## 6 TidyDensity         1 ARIMA       Test  0.681  223. 0.782 113.  0.821  0.0493 
    ## 7 tidyAML             3 EARTH       Test  0.774  171. 0.930  95.2 0.863  0.00301
    ## 8 RandomWalk…         1 ARIMA       Test  1.37   100  0.641 200   1.47  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1432|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1425|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1371|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1342|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1168|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1022|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [638|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [72|28]>   <mdl_tm_t [1 × 5]>

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
