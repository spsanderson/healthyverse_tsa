Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 130,099
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

The last day in the data set is 2025-02-08 22:35:56, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.284263^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 130099        |
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
| r_version     |     92962 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     92962 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     92962 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10946 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-08 | 2023-04-29 | 1539 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1139986.30 | 1530420.9 | 355 | 14701 | 260378 | 2367841 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10474.77 | 18464.8 | 1 | 336 | 3098 | 11961 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-08 22:35:56 | 2023-04-29 12:27:55 | 78721 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 40S |       60 |

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
    ## -155.30  -35.03   -9.84   27.25  808.60 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.900e+02  7.722e+01
    ## date                                                1.140e-02  4.094e-03
    ## lag(value, 1)                                       1.133e-01  2.522e-02
    ## lag(value, 7)                                       8.923e-02  2.620e-02
    ## lag(value, 14)                                      1.097e-01  2.630e-02
    ## lag(value, 21)                                      5.084e-02  2.645e-02
    ## lag(value, 28)                                      6.249e-02  2.629e-02
    ## lag(value, 35)                                      7.934e-02  2.639e-02
    ## lag(value, 42)                                      5.535e-02  2.655e-02
    ## lag(value, 49)                                      8.841e-02  2.639e-02
    ## month(date, label = TRUE).L                        -1.123e+01  5.273e+00
    ## month(date, label = TRUE).Q                         1.959e+00  5.189e+00
    ## month(date, label = TRUE).C                        -1.124e+01  5.276e+00
    ## month(date, label = TRUE)^4                        -7.981e+00  5.287e+00
    ## month(date, label = TRUE)^5                        -1.286e+01  5.280e+00
    ## month(date, label = TRUE)^6                        -1.202e+00  5.362e+00
    ## month(date, label = TRUE)^7                        -9.072e+00  5.288e+00
    ## month(date, label = TRUE)^8                        -2.284e+00  5.299e+00
    ## month(date, label = TRUE)^9                         4.186e+00  5.300e+00
    ## month(date, label = TRUE)^10                        5.231e+00  5.302e+00
    ## month(date, label = TRUE)^11                       -6.410e+00  5.316e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.157e+01  2.421e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.516e+00  2.546e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.460 0.014004 *  
    ## date                                                 2.784 0.005444 ** 
    ## lag(value, 1)                                        4.492 7.62e-06 ***
    ## lag(value, 7)                                        3.406 0.000677 ***
    ## lag(value, 14)                                       4.170 3.22e-05 ***
    ## lag(value, 21)                                       1.922 0.054800 .  
    ## lag(value, 28)                                       2.376 0.017610 *  
    ## lag(value, 35)                                       3.006 0.002693 ** 
    ## lag(value, 42)                                       2.084 0.037299 *  
    ## lag(value, 49)                                       3.350 0.000829 ***
    ## month(date, label = TRUE).L                         -2.130 0.033340 *  
    ## month(date, label = TRUE).Q                          0.378 0.705814    
    ## month(date, label = TRUE).C                         -2.130 0.033323 *  
    ## month(date, label = TRUE)^4                         -1.510 0.131364    
    ## month(date, label = TRUE)^5                         -2.435 0.015018 *  
    ## month(date, label = TRUE)^6                         -0.224 0.822662    
    ## month(date, label = TRUE)^7                         -1.716 0.086450 .  
    ## month(date, label = TRUE)^8                         -0.431 0.666474    
    ## month(date, label = TRUE)^9                          0.790 0.429732    
    ## month(date, label = TRUE)^10                         0.987 0.323967    
    ## month(date, label = TRUE)^11                        -1.206 0.228078    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.780 1.93e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.952 0.003211 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.33 on 1467 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2578, Adjusted R-squared:  0.2467 
    ## F-statistic: 23.16 on 22 and 1467 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,504 × 2]> <tibble [28 × 2]> <split [1476|28]>
    ## 2 healthyR      <tibble [1,497 × 2]> <tibble [28 × 2]> <split [1469|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,443 × 2]> <tibble [28 × 2]> <split [1415|28]>
    ## 5 healthyverse  <tibble [1,414 × 2]> <tibble [28 × 2]> <split [1386|28]>
    ## 6 healthyR.ai   <tibble [1,240 × 2]> <tibble [28 × 2]> <split [1212|28]>
    ## 7 TidyDensity   <tibble [1,094 × 2]> <tibble [28 × 2]> <split [1066|28]>
    ## 8 tidyAML       <tibble [709 × 2]>   <tibble [28 × 2]> <split [681|28]> 
    ## 9 RandomWalker  <tibble [143 × 2]>   <tibble [28 × 2]> <split [115|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.9037680 | 171.14122 | 0.7357073 | 144.43518 | 1.0814501 | 0.0063954 |
| healthyR.data | 2 | LM | Test | 0.9124370 | 162.09488 | 0.7427642 | 139.36890 | 1.1031349 | 0.1666159 |
| healthyR.data | 3 | EARTH | Test | 1.0063596 | 211.38204 | 0.8192214 | 138.43895 | 1.2117125 | 0.1666159 |
| healthyR.data | 4 | NNAR | Test | 0.8122626 | 129.87737 | 0.6612178 | 180.26567 | 0.9295664 | 0.0462576 |
| healthyR | 1 | ARIMA | Test | 0.8110704 | 93.87821 | 0.6459011 | 166.78849 | 0.9489751 | 0.0979725 |
| healthyR | 2 | LM | Test | 0.8399574 | 100.50042 | 0.6689055 | 190.08347 | 0.9861881 | 0.0266608 |
| healthyR | 3 | EARTH | Test | 0.8331947 | 101.91406 | 0.6635199 | 170.57977 | 0.9888628 | 0.0266608 |
| healthyR | 4 | NNAR | Test | 0.7961677 | 100.02468 | 0.6340333 | 156.52856 | 0.9441309 | 0.1320301 |
| healthyR.ts | 1 | ARIMA | Test | 0.7933528 | 206.52608 | 0.6045248 | 134.84722 | 1.0490340 | 0.0000748 |
| healthyR.ts | 2 | LM | Test | 0.8361485 | 260.31831 | 0.6371345 | 135.19569 | 1.0579847 | 0.0000748 |
| healthyR.ts | 3 | EARTH | Test | 0.8413932 | 266.41921 | 0.6411309 | 135.27035 | 1.0596849 | 0.0000748 |
| healthyR.ts | 4 | NNAR | Test | 0.6894934 | 127.36078 | 0.5253852 | 170.28549 | 0.9744286 | 0.1980329 |
| healthyverse | 1 | ARIMA | Test | 0.5953352 | 271.27122 | 0.6523549 | 100.48900 | 0.7064424 | 0.0893705 |
| healthyverse | 2 | LM | Test | 0.6234422 | 294.00447 | 0.6831539 | 96.83816 | 0.7605179 | 0.0002987 |
| healthyverse | 3 | EARTH | Test | 5.2979982 | 2145.07462 | 5.8054275 | 160.99433 | 5.8143184 | 0.0002987 |
| healthyverse | 4 | NNAR | Test | 0.6066790 | 194.73402 | 0.6647852 | 114.76722 | 0.7120523 | 0.0586097 |
| healthyR.ai | 1 | ARIMA | Test | 0.7378976 | 113.16233 | 0.6798128 | 176.04210 | 0.9145019 | 0.0933437 |
| healthyR.ai | 2 | LM | Test | 0.7607885 | 125.61519 | 0.7009019 | 159.21860 | 0.9569999 | 0.0221163 |
| healthyR.ai | 3 | EARTH | Test | 0.7677819 | 153.70678 | 0.7073448 | 140.19680 | 0.9892479 | 0.0221163 |
| healthyR.ai | 4 | NNAR | Test | 0.7146109 | 128.87070 | 0.6583593 | 156.67502 | 0.8669691 | 0.1994182 |
| TidyDensity | 1 | ARIMA | Test | 0.5325254 | 143.79451 | 0.6049413 | 101.57585 | 0.6600303 | 0.1963432 |
| TidyDensity | 2 | LM | Test | 0.6604103 | 229.56934 | 0.7502167 | 106.32917 | 0.7988679 | 0.0008957 |
| TidyDensity | 3 | EARTH | Test | 0.5761621 | 153.49551 | 0.6545119 | 106.47546 | 0.7280622 | 0.0008957 |
| TidyDensity | 4 | NNAR | Test | 0.5575982 | 107.49969 | 0.6334237 | 133.79184 | 0.6814739 | 0.1919595 |
| tidyAML | 1 | ARIMA | Test | 0.7017861 | 107.69154 | 0.7344468 | 85.76634 | 0.8778134 | 0.3017522 |
| tidyAML | 2 | LM | Test | 0.7662001 | 101.42242 | 0.8018586 | 92.64610 | 0.9587428 | 0.0130727 |
| tidyAML | 3 | EARTH | Test | 0.7702529 | 118.82478 | 0.8061000 | 87.96927 | 0.9700525 | 0.0130727 |
| tidyAML | 4 | NNAR | Test | 0.7459963 | 105.66539 | 0.7807145 | 89.15337 | 0.9458954 | 0.0285243 |
| RandomWalker | 1 | ARIMA | Test | 1.7197068 | 218.53265 | 0.8347899 | 168.52425 | 1.9137511 | 0.1225000 |
| RandomWalker | 2 | LM | Test | 1.2893426 | 98.05338 | 0.6258800 | 190.74944 | 1.4470581 | 0.0021608 |
| RandomWalker | 3 | EARTH | Test | 1.2766637 | 93.40860 | 0.6197254 | 163.42820 | 1.4606717 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2635916 | 139.28257 | 0.6133799 | 144.45048 | 1.4921761 | 0.0288665 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.812 130.  0.661 180.  0.930 0.0463 
    ## 2 healthyR             4 NNAR        Test  0.796 100.  0.634 157.  0.944 0.132  
    ## 3 healthyR.ts          4 NNAR        Test  0.689 127.  0.525 170.  0.974 0.198  
    ## 4 healthyverse         1 ARIMA       Test  0.595 271.  0.652 100.  0.706 0.0894 
    ## 5 healthyR.ai          4 NNAR        Test  0.715 129.  0.658 157.  0.867 0.199  
    ## 6 TidyDensity          1 ARIMA       Test  0.533 144.  0.605 102.  0.660 0.196  
    ## 7 tidyAML              1 ARIMA       Test  0.702 108.  0.734  85.8 0.878 0.302  
    ## 8 RandomWalker         2 LM          Test  1.29   98.1 0.626 191.  1.45  0.00216

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1476|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1469|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1415|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1386|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1212|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1066|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [681|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [115|28]>  <mdl_tm_t [1 × 5]>

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
