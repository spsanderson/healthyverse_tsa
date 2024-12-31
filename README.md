Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
31 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 125,414
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

The last day in the data set is 2024-12-29 14:17:19, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3458.69
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 125414        |
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
| r_version     |     89048 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89048 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89048 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10738 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-29 | 2023-04-13 | 1498 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1146249.27 | 1535082.20 | 355 | 14701.00 | 258975 | 2367958.8 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10328.62 | 17967.12 | 1 | 316.25 | 3098 | 11849.5 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-29 14:17:19 | 2023-04-13 04:02:31 | 75917 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   28.5 |       60 |

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
    ## -155.80  -34.84   -9.77   27.15  803.88 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.846e+02  7.976e+01
    ## date                                                1.105e-02  4.226e-03
    ## lag(value, 1)                                       1.322e-01  2.550e-02
    ## lag(value, 7)                                       8.967e-02  2.645e-02
    ## lag(value, 14)                                      1.086e-01  2.645e-02
    ## lag(value, 21)                                      4.502e-02  2.659e-02
    ## lag(value, 28)                                      6.964e-02  2.639e-02
    ## lag(value, 35)                                      7.188e-02  2.653e-02
    ## lag(value, 42)                                      4.451e-02  2.660e-02
    ## lag(value, 49)                                      1.020e-01  2.652e-02
    ## month(date, label = TRUE).L                        -1.074e+01  5.468e+00
    ## month(date, label = TRUE).Q                         2.378e+00  5.294e+00
    ## month(date, label = TRUE).C                        -1.087e+01  5.359e+00
    ## month(date, label = TRUE)^4                        -7.396e+00  5.350e+00
    ## month(date, label = TRUE)^5                        -1.256e+01  5.311e+00
    ## month(date, label = TRUE)^6                        -8.865e-01  5.379e+00
    ## month(date, label = TRUE)^7                        -8.955e+00  5.271e+00
    ## month(date, label = TRUE)^8                        -2.236e+00  5.266e+00
    ## month(date, label = TRUE)^9                         4.224e+00  5.256e+00
    ## month(date, label = TRUE)^10                        5.148e+00  5.256e+00
    ## month(date, label = TRUE)^11                       -6.227e+00  5.269e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.168e+01  2.433e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.948e+00  2.546e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.314 0.020812 *  
    ## date                                                 2.616 0.008995 ** 
    ## lag(value, 1)                                        5.182 2.51e-07 ***
    ## lag(value, 7)                                        3.391 0.000716 ***
    ## lag(value, 14)                                       4.104 4.28e-05 ***
    ## lag(value, 21)                                       1.693 0.090642 .  
    ## lag(value, 28)                                       2.639 0.008409 ** 
    ## lag(value, 35)                                       2.710 0.006818 ** 
    ## lag(value, 42)                                       1.673 0.094513 .  
    ## lag(value, 49)                                       3.847 0.000125 ***
    ## month(date, label = TRUE).L                         -1.964 0.049714 *  
    ## month(date, label = TRUE).Q                          0.449 0.653408    
    ## month(date, label = TRUE).C                         -2.029 0.042656 *  
    ## month(date, label = TRUE)^4                         -1.382 0.167040    
    ## month(date, label = TRUE)^5                         -2.366 0.018138 *  
    ## month(date, label = TRUE)^6                         -0.165 0.869114    
    ## month(date, label = TRUE)^7                         -1.699 0.089567 .  
    ## month(date, label = TRUE)^8                         -0.425 0.671249    
    ## month(date, label = TRUE)^9                          0.804 0.421735    
    ## month(date, label = TRUE)^10                         0.980 0.327487    
    ## month(date, label = TRUE)^11                        -1.182 0.237495    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.801 1.74e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.729 0.006436 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.81 on 1426 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2591, Adjusted R-squared:  0.2477 
    ## F-statistic: 22.67 on 22 and 1426 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,464 × 2]> <tibble [28 × 2]> <split [1436|28]>
    ## 2 healthyR      <tibble [1,457 × 2]> <tibble [28 × 2]> <split [1429|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,403 × 2]> <tibble [28 × 2]> <split [1375|28]>
    ## 5 healthyverse  <tibble [1,374 × 2]> <tibble [28 × 2]> <split [1346|28]>
    ## 6 healthyR.ai   <tibble [1,200 × 2]> <tibble [28 × 2]> <split [1172|28]>
    ## 7 TidyDensity   <tibble [1,054 × 2]> <tibble [28 × 2]> <split [1026|28]>
    ## 8 tidyAML       <tibble [670 × 2]>   <tibble [28 × 2]> <split [642|28]> 
    ## 9 RandomWalker  <tibble [104 × 2]>   <tibble [28 × 2]> <split [76|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7275242 | 335.27653 | 0.6304164 | 145.01861 | 0.8576541 | 0.1456017 |
| healthyR.data | 2 | LM | Test | 0.7404855 | 329.44021 | 0.6416477 | 142.14175 | 0.8984503 | 0.0036883 |
| healthyR.data | 3 | EARTH | Test | 0.8440828 | 535.38439 | 0.7314172 | 137.10354 | 0.9436459 | 0.0036883 |
| healthyR.data | 4 | NNAR | Test | 0.7598109 | 169.26138 | 0.6583936 | 168.79541 | 1.0173798 | 0.0004078 |
| healthyR | 1 | ARIMA | Test | 0.6671555 | 149.56851 | 0.6555832 | 172.74916 | 0.7904819 | 0.0312295 |
| healthyR | 2 | LM | Test | 0.6495078 | 103.52974 | 0.6382416 | 182.28815 | 0.7989486 | 0.0345049 |
| healthyR | 3 | EARTH | Test | 0.6397883 | 105.75408 | 0.6286907 | 161.10146 | 0.7979387 | 0.0345049 |
| healthyR | 4 | NNAR | Test | 0.6294063 | 118.02362 | 0.6184888 | 148.15902 | 0.7882845 | 0.1143499 |
| healthyR.ts | 1 | ARIMA | Test | 0.8067736 | 137.55745 | 0.7173479 | 125.00601 | 0.9954595 | 0.0263506 |
| healthyR.ts | 2 | LM | Test | 0.8247971 | 160.30082 | 0.7333737 | 122.30478 | 1.0025918 | 0.0263506 |
| healthyR.ts | 3 | EARTH | Test | 0.8225494 | 157.88914 | 0.7313751 | 122.43722 | 1.0013221 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8025479 | 95.68866 | 0.7135906 | 178.08893 | 1.0061200 | 0.1491401 |
| healthyverse | 1 | ARIMA | Test | 0.4622999 | 188.72322 | 0.7444392 | 87.91089 | 0.5836187 | 0.3801860 |
| healthyverse | 2 | LM | Test | 0.5351483 | 309.77416 | 0.8617466 | 85.69048 | 0.6653250 | 0.0047788 |
| healthyverse | 3 | EARTH | Test | 0.5845360 | 360.20508 | 0.9412754 | 86.53826 | 0.7137471 | 0.0047788 |
| healthyverse | 4 | NNAR | Test | 0.5651759 | 213.20968 | 0.9101000 | 104.26604 | 0.7064090 | 0.0000618 |
| healthyR.ai | 1 | ARIMA | Test | 0.6540648 | 129.38904 | 0.8398613 | 173.32616 | 0.7406409 | 0.2391212 |
| healthyR.ai | 2 | LM | Test | 0.6667115 | 114.67174 | 0.8561004 | 152.44982 | 0.7745941 | 0.0278374 |
| healthyR.ai | 3 | EARTH | Test | 0.6385053 | 116.68736 | 0.8198819 | 133.11917 | 0.7713827 | 0.0278374 |
| healthyR.ai | 4 | NNAR | Test | 0.6588767 | 172.56190 | 0.8460400 | 149.60268 | 0.7541867 | 0.0625955 |
| TidyDensity | 1 | ARIMA | Test | 0.6488668 | 187.86195 | 0.8281737 | 118.08477 | 0.8043529 | 0.0000888 |
| TidyDensity | 2 | LM | Test | 0.7032808 | 316.36893 | 0.8976242 | 113.22853 | 0.8061942 | 0.0974829 |
| TidyDensity | 3 | EARTH | Test | 0.6442836 | 196.37284 | 0.8223240 | 120.58721 | 0.7901016 | 0.0974829 |
| TidyDensity | 4 | NNAR | Test | 0.6272303 | 98.47962 | 0.8005581 | 135.63048 | 0.8242577 | 0.1037577 |
| tidyAML | 1 | ARIMA | Test | 0.8200557 | 146.95779 | 0.9165261 | 102.39821 | 0.9560879 | 0.0393697 |
| tidyAML | 2 | LM | Test | 0.8672543 | 134.06878 | 0.9692770 | 108.11936 | 1.0092773 | 0.1219914 |
| tidyAML | 3 | EARTH | Test | 0.8126849 | 183.03607 | 0.9082882 | 94.92524 | 0.8940853 | 0.1219914 |
| tidyAML | 4 | NNAR | Test | 0.8411482 | 160.38019 | 0.9400999 | 101.60696 | 0.9305600 | 0.0139698 |
| RandomWalker | 1 | ARIMA | Test | 0.6435114 | 74.24135 | 0.3092023 | 68.61757 | 0.7802228 | 0.7713420 |
| RandomWalker | 2 | LM | Test | 1.2610580 | 97.48520 | 0.6059287 | 159.09282 | 1.4051053 | 0.0009997 |
| RandomWalker | 3 | EARTH | Test | 1.4052393 | 169.31568 | 0.6752068 | 108.98904 | 1.8080655 | 0.0009997 |
| RandomWalker | 4 | NNAR | Test | 3.2778970 | 301.99215 | 1.5750045 | 145.44585 | 4.7539643 | 0.0002834 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.728 335.  0.630 145.  0.858 0.146 
    ## 2 healthyR              4 NNAR        Test  0.629 118.  0.618 148.  0.788 0.114 
    ## 3 healthyR.ts           1 ARIMA       Test  0.807 138.  0.717 125.  0.995 0.0264
    ## 4 healthyverse          1 ARIMA       Test  0.462 189.  0.744  87.9 0.584 0.380 
    ## 5 healthyR.ai           1 ARIMA       Test  0.654 129.  0.840 173.  0.741 0.239 
    ## 6 TidyDensity           3 EARTH       Test  0.644 196.  0.822 121.  0.790 0.0975
    ## 7 tidyAML               3 EARTH       Test  0.813 183.  0.908  94.9 0.894 0.122 
    ## 8 RandomWalker          1 ARIMA       Test  0.644  74.2 0.309  68.6 0.780 0.771

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1436|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1429|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1375|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1346|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1172|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1026|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [642|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [76|28]>   <mdl_tm_t [1 × 5]>

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
