Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
31 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,944
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

The last day in the data set is 2025-01-29 22:14:44, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4210.65
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128944        |
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
| r_version     |     91962 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91962 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91962 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10910 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-29 | 2023-04-26 | 1529 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1142502.54 | 1531792.55 | 355 | 14701 | 260384 | 2367881 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10462.85 | 18480.06 | 1 | 334 | 3091 | 11949 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-29 22:14:44 | 2023-04-26 02:05:00 | 78134 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     40 |       60 |

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
    ## -155.42  -34.85   -9.38   27.10  806.77 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.822e+02  7.775e+01
    ## date                                                1.096e-02  4.121e-03
    ## lag(value, 1)                                       1.212e-01  2.521e-02
    ## lag(value, 7)                                       9.355e-02  2.617e-02
    ## lag(value, 14)                                      1.106e-01  2.627e-02
    ## lag(value, 21)                                      5.085e-02  2.638e-02
    ## lag(value, 28)                                      6.503e-02  2.626e-02
    ## lag(value, 35)                                      6.915e-02  2.641e-02
    ## lag(value, 42)                                      5.145e-02  2.647e-02
    ## lag(value, 49)                                      9.294e-02  2.634e-02
    ## month(date, label = TRUE).L                        -1.079e+01  5.299e+00
    ## month(date, label = TRUE).Q                         1.808e+00  5.175e+00
    ## month(date, label = TRUE).C                        -1.102e+01  5.257e+00
    ## month(date, label = TRUE)^4                        -7.728e+00  5.287e+00
    ## month(date, label = TRUE)^5                        -1.275e+01  5.294e+00
    ## month(date, label = TRUE)^6                        -9.257e-01  5.379e+00
    ## month(date, label = TRUE)^7                        -9.026e+00  5.286e+00
    ## month(date, label = TRUE)^8                        -2.210e+00  5.284e+00
    ## month(date, label = TRUE)^9                         4.195e+00  5.276e+00
    ## month(date, label = TRUE)^10                        5.102e+00  5.275e+00
    ## month(date, label = TRUE)^11                       -6.167e+00  5.289e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.416e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.207e+00  2.540e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.343 0.019273 *  
    ## date                                                 2.660 0.007905 ** 
    ## lag(value, 1)                                        4.808 1.68e-06 ***
    ## lag(value, 7)                                        3.575 0.000362 ***
    ## lag(value, 14)                                       4.211 2.69e-05 ***
    ## lag(value, 21)                                       1.928 0.054094 .  
    ## lag(value, 28)                                       2.477 0.013376 *  
    ## lag(value, 35)                                       2.618 0.008930 ** 
    ## lag(value, 42)                                       1.943 0.052166 .  
    ## lag(value, 49)                                       3.528 0.000432 ***
    ## month(date, label = TRUE).L                         -2.037 0.041828 *  
    ## month(date, label = TRUE).Q                          0.349 0.726876    
    ## month(date, label = TRUE).C                         -2.097 0.036181 *  
    ## month(date, label = TRUE)^4                         -1.462 0.144022    
    ## month(date, label = TRUE)^5                         -2.408 0.016180 *  
    ## month(date, label = TRUE)^6                         -0.172 0.863399    
    ## month(date, label = TRUE)^7                         -1.707 0.087943 .  
    ## month(date, label = TRUE)^8                         -0.418 0.675825    
    ## month(date, label = TRUE)^9                          0.795 0.426677    
    ## month(date, label = TRUE)^10                         0.967 0.333649    
    ## month(date, label = TRUE)^11                        -1.166 0.243814    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.814 1.63e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.838 0.004606 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.03 on 1457 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2596, Adjusted R-squared:  0.2485 
    ## F-statistic: 23.23 on 22 and 1457 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,494 × 2]> <tibble [28 × 2]> <split [1466|28]>
    ## 2 healthyR      <tibble [1,487 × 2]> <tibble [28 × 2]> <split [1459|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,433 × 2]> <tibble [28 × 2]> <split [1405|28]>
    ## 5 healthyverse  <tibble [1,404 × 2]> <tibble [28 × 2]> <split [1376|28]>
    ## 6 healthyR.ai   <tibble [1,230 × 2]> <tibble [28 × 2]> <split [1202|28]>
    ## 7 TidyDensity   <tibble [1,084 × 2]> <tibble [28 × 2]> <split [1056|28]>
    ## 8 tidyAML       <tibble [699 × 2]>   <tibble [28 × 2]> <split [671|28]> 
    ## 9 RandomWalker  <tibble [133 × 2]>   <tibble [28 × 2]> <split [105|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.0028358 | 202.70320 | 0.7327170 | 158.46407 | 1.1766903 | 0.0075466 |
| healthyR.data | 2 | LM | Test | 1.0233782 | 227.11338 | 0.7477262 | 157.82006 | 1.1871634 | 0.0050353 |
| healthyR.data | 3 | EARTH | Test | 1.0796396 | 264.85015 | 0.7888333 | 157.66032 | 1.2364150 | 0.0050353 |
| healthyR.data | 4 | NNAR | Test | 0.8207803 | 107.92385 | 0.5996990 | 169.45896 | 1.0003721 | 0.2469439 |
| healthyR | 1 | ARIMA | Test | 0.7106075 | 100.59237 | 0.6605553 | 153.26680 | 0.8943655 | 0.1461594 |
| healthyR | 2 | LM | Test | 0.7547959 | 103.34861 | 0.7016312 | 188.83406 | 0.9323841 | 0.0319951 |
| healthyR | 3 | EARTH | Test | 0.7553087 | 109.45816 | 0.7021079 | 171.30013 | 0.9354523 | 0.0319951 |
| healthyR | 4 | NNAR | Test | 0.6868493 | 112.90285 | 0.6384705 | 152.90759 | 0.8746085 | 0.1392282 |
| healthyR.ts | 1 | ARIMA | Test | 0.6395627 | 200.39222 | 0.5698974 | 117.83236 | 0.8901981 | 0.0136391 |
| healthyR.ts | 2 | LM | Test | 0.6724244 | 249.84747 | 0.5991796 | 117.89119 | 0.9007285 | 0.0136391 |
| healthyR.ts | 3 | EARTH | Test | 0.6769544 | 256.08416 | 0.6032161 | 117.93275 | 0.9028195 | 0.0136391 |
| healthyR.ts | 4 | NNAR | Test | 0.5612465 | 93.66819 | 0.5001119 | 167.60134 | 0.7815358 | 0.3204764 |
| healthyverse | 1 | ARIMA | Test | 0.6689860 | 308.41453 | 0.6441432 | 111.15116 | 0.7979802 | 0.0703194 |
| healthyverse | 2 | LM | Test | 0.7037402 | 376.31085 | 0.6776068 | 103.75477 | 0.8437723 | 0.0390602 |
| healthyverse | 3 | EARTH | Test | 0.7464061 | 462.13038 | 0.7186884 | 101.85523 | 0.9004693 | 0.0390602 |
| healthyverse | 4 | NNAR | Test | 0.6721639 | 241.49850 | 0.6472031 | 121.21387 | 0.8092457 | 0.0946773 |
| healthyR.ai | 1 | ARIMA | Test | 0.6245479 | 133.06193 | 0.6322366 | 169.18285 | 0.7965251 | 0.1309948 |
| healthyR.ai | 2 | LM | Test | 0.6664111 | 141.90932 | 0.6746152 | 157.93555 | 0.8549817 | 0.0007903 |
| healthyR.ai | 3 | EARTH | Test | 0.6759986 | 178.24208 | 0.6843207 | 136.05865 | 0.8984580 | 0.0007903 |
| healthyR.ai | 4 | NNAR | Test | 0.5892993 | 133.55185 | 0.5965541 | 151.39443 | 0.7539063 | 0.2475489 |
| TidyDensity | 1 | ARIMA | Test | 0.6246815 | 158.74070 | 0.6365716 | 103.07742 | 0.7683356 | 0.1702173 |
| TidyDensity | 2 | LM | Test | 0.6815113 | 204.50103 | 0.6944831 | 97.03638 | 0.8706201 | 0.0009692 |
| TidyDensity | 3 | EARTH | Test | 0.6278545 | 142.39076 | 0.6398050 | 99.52985 | 0.8170234 | 0.0009692 |
| TidyDensity | 4 | NNAR | Test | 0.6493293 | 100.69546 | 0.6616886 | 138.48715 | 0.7719432 | 0.1461974 |
| tidyAML | 1 | ARIMA | Test | 0.9131092 | 337.48970 | 0.8852949 | 100.90797 | 1.1270522 | 0.0024640 |
| tidyAML | 2 | LM | Test | 0.7823942 | 212.85068 | 0.7585617 | 104.56369 | 0.9812937 | 0.0000527 |
| tidyAML | 3 | EARTH | Test | 3.8744817 | 1276.21265 | 3.7564609 | 149.44297 | 4.1819184 | 0.0000527 |
| tidyAML | 4 | NNAR | Test | 0.8226454 | 237.97746 | 0.7975868 | 103.12306 | 1.0227113 | 0.0047499 |
| RandomWalker | 1 | ARIMA | Test | 1.6114295 | 234.33904 | 0.8317967 | 142.19840 | 1.9125554 | 0.0290214 |
| RandomWalker | 2 | LM | Test | 1.2093954 | 106.82713 | 0.6242725 | 156.40519 | 1.4297334 | 0.0019811 |
| RandomWalker | 3 | EARTH | Test | 1.2014341 | 101.82532 | 0.6201630 | 160.96432 | 1.4141036 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4481210 | 197.45510 | 0.7474992 | 137.88148 | 1.8942483 | 0.0097961 |

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
    ## 1 healthyR.d…         4 NNAR        Test  0.821 108.  0.600  169. 1.00   2.47e-1
    ## 2 healthyR            4 NNAR        Test  0.687 113.  0.638  153. 0.875  1.39e-1
    ## 3 healthyR.ts         4 NNAR        Test  0.561  93.7 0.500  168. 0.782  3.20e-1
    ## 4 healthyver…         1 ARIMA       Test  0.669 308.  0.644  111. 0.798  7.03e-2
    ## 5 healthyR.ai         4 NNAR        Test  0.589 134.  0.597  151. 0.754  2.48e-1
    ## 6 TidyDensity         1 ARIMA       Test  0.625 159.  0.637  103. 0.768  1.70e-1
    ## 7 tidyAML             2 LM          Test  0.782 213.  0.759  105. 0.981  5.27e-5
    ## 8 RandomWalk…         3 EARTH       Test  1.20  102.  0.620  161. 1.41  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1466|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1459|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1405|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1376|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1202|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1056|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [671|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [105|28]>  <mdl_tm_t [1 × 5]>

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
