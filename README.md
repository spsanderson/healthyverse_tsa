Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,491
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

The last day in the data set is 2025-01-07 22:30:48, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3682.92
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126491        |
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
| r_version     |     89955 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89955 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89955 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10769 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-07 | 2023-04-19 | 1507 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1145261.06 | 1533637.22 | 355 | 14701 | 260378 | 2367948 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10352.46 | 18030.07 | 1 | 317 | 3098 | 11845 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-07 22:30:48 | 2023-04-19 00:23:12 | 76615 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 48S |       60 |

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
    ## -154.93  -35.28   -9.90   27.04  803.99 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.955e+02  7.912e+01
    ## date                                                1.164e-02  4.192e-03
    ## lag(value, 1)                                       1.268e-01  2.542e-02
    ## lag(value, 7)                                       9.166e-02  2.644e-02
    ## lag(value, 14)                                      1.053e-01  2.645e-02
    ## lag(value, 21)                                      5.079e-02  2.659e-02
    ## lag(value, 28)                                      6.752e-02  2.641e-02
    ## lag(value, 35)                                      7.301e-02  2.653e-02
    ## lag(value, 42)                                      4.936e-02  2.660e-02
    ## lag(value, 49)                                      9.467e-02  2.644e-02
    ## month(date, label = TRUE).L                        -1.199e+01  5.421e+00
    ## month(date, label = TRUE).Q                         2.791e+00  5.259e+00
    ## month(date, label = TRUE).C                        -1.197e+01  5.327e+00
    ## month(date, label = TRUE)^4                        -7.063e+00  5.335e+00
    ## month(date, label = TRUE)^5                        -1.325e+01  5.311e+00
    ## month(date, label = TRUE)^6                        -6.869e-01  5.386e+00
    ## month(date, label = TRUE)^7                        -9.207e+00  5.282e+00
    ## month(date, label = TRUE)^8                        -2.130e+00  5.278e+00
    ## month(date, label = TRUE)^9                         4.135e+00  5.269e+00
    ## month(date, label = TRUE)^10                        5.180e+00  5.268e+00
    ## month(date, label = TRUE)^11                       -6.248e+00  5.282e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.429e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.320e+00  2.546e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.471 0.013603 *  
    ## date                                                 2.777 0.005561 ** 
    ## lag(value, 1)                                        4.990 6.79e-07 ***
    ## lag(value, 7)                                        3.467 0.000542 ***
    ## lag(value, 14)                                       3.982 7.17e-05 ***
    ## lag(value, 21)                                       1.911 0.056264 .  
    ## lag(value, 28)                                       2.556 0.010688 *  
    ## lag(value, 35)                                       2.752 0.006000 ** 
    ## lag(value, 42)                                       1.856 0.063713 .  
    ## lag(value, 49)                                       3.580 0.000355 ***
    ## month(date, label = TRUE).L                         -2.212 0.027092 *  
    ## month(date, label = TRUE).Q                          0.531 0.595652    
    ## month(date, label = TRUE).C                         -2.246 0.024831 *  
    ## month(date, label = TRUE)^4                         -1.324 0.185800    
    ## month(date, label = TRUE)^5                         -2.495 0.012695 *  
    ## month(date, label = TRUE)^6                         -0.128 0.898528    
    ## month(date, label = TRUE)^7                         -1.743 0.081546 .  
    ## month(date, label = TRUE)^8                         -0.404 0.686619    
    ## month(date, label = TRUE)^9                          0.785 0.432676    
    ## month(date, label = TRUE)^10                         0.983 0.325646    
    ## month(date, label = TRUE)^11                        -1.183 0.237044    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.747 2.27e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.876 0.004091 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.95 on 1435 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2591, Adjusted R-squared:  0.2478 
    ## F-statistic: 22.82 on 22 and 1435 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,472 × 2]> <tibble [28 × 2]> <split [1444|28]>
    ## 2 healthyR      <tibble [1,465 × 2]> <tibble [28 × 2]> <split [1437|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,411 × 2]> <tibble [28 × 2]> <split [1383|28]>
    ## 5 healthyverse  <tibble [1,382 × 2]> <tibble [28 × 2]> <split [1354|28]>
    ## 6 healthyR.ai   <tibble [1,208 × 2]> <tibble [28 × 2]> <split [1180|28]>
    ## 7 TidyDensity   <tibble [1,062 × 2]> <tibble [28 × 2]> <split [1034|28]>
    ## 8 tidyAML       <tibble [678 × 2]>   <tibble [28 × 2]> <split [650|28]> 
    ## 9 RandomWalker  <tibble [112 × 2]>   <tibble [28 × 2]> <split [84|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7924641 | 263.30167 | 0.6460851 | 151.98840 | 0.9098234 | 0.0908285 |
| healthyR.data | 2 | LM | Test | 0.8119237 | 284.92300 | 0.6619502 | 146.54150 | 0.9476770 | 0.0522457 |
| healthyR.data | 3 | EARTH | Test | 0.9396927 | 465.27707 | 0.7661185 | 142.13094 | 1.0457232 | 0.0522457 |
| healthyR.data | 4 | NNAR | Test | 0.7653526 | 116.58734 | 0.6239814 | 161.40900 | 1.0015275 | 0.0054308 |
| healthyR | 1 | ARIMA | Test | 0.6736215 | 140.28462 | 0.6490291 | 175.44899 | 0.7928395 | 0.0695085 |
| healthyR | 2 | LM | Test | 0.6683643 | 100.37682 | 0.6439637 | 182.16928 | 0.8200457 | 0.0001994 |
| healthyR | 3 | EARTH | Test | 0.6646231 | 98.08717 | 0.6403591 | 160.27635 | 0.8287768 | 0.0001994 |
| healthyR | 4 | NNAR | Test | 0.6273440 | 92.97167 | 0.6044410 | 151.83506 | 0.7925772 | 0.0886421 |
| healthyR.ts | 1 | ARIMA | Test | 0.8232644 | 127.81947 | 0.6455235 | 120.85033 | 1.0456370 | 0.0079678 |
| healthyR.ts | 2 | LM | Test | 0.8355828 | 153.43560 | 0.6551824 | 116.60497 | 1.0458482 | 0.0057621 |
| healthyR.ts | 3 | EARTH | Test | 0.8374723 | 156.36238 | 0.6566640 | 116.25267 | 1.0466902 | 0.0057621 |
| healthyR.ts | 4 | NNAR | Test | 0.8527446 | 95.48717 | 0.6686390 | 177.61778 | 1.0725735 | 0.0557747 |
| healthyverse | 1 | ARIMA | Test | 0.5620417 | 199.41693 | 0.7856287 | 100.43875 | 0.6975070 | 0.0343906 |
| healthyverse | 2 | LM | Test | 0.6135715 | 304.91647 | 0.8576577 | 97.06448 | 0.7550318 | 0.0381855 |
| healthyverse | 3 | EARTH | Test | 0.6872596 | 383.59873 | 0.9606599 | 96.45917 | 0.8691119 | 0.0381855 |
| healthyverse | 4 | NNAR | Test | 0.5696534 | 157.26856 | 0.7962684 | 111.96920 | 0.7039990 | 0.0139698 |
| healthyR.ai | 1 | ARIMA | Test | 0.6066531 | 92.77373 | 0.7121741 | 162.17820 | 0.7274835 | 0.1424222 |
| healthyR.ai | 2 | LM | Test | 0.6433102 | 116.92765 | 0.7552073 | 149.93754 | 0.7752549 | 0.0001745 |
| healthyR.ai | 3 | EARTH | Test | 0.6290726 | 146.30489 | 0.7384933 | 122.82642 | 0.8136983 | 0.0001745 |
| healthyR.ai | 4 | NNAR | Test | 0.6092277 | 105.68104 | 0.7151966 | 148.82240 | 0.7393571 | 0.0922303 |
| TidyDensity | 1 | ARIMA | Test | 0.7539890 | 157.61580 | 0.7316815 | 120.53086 | 0.8858301 | 0.0491217 |
| TidyDensity | 2 | LM | Test | 0.8000032 | 206.26601 | 0.7763342 | 114.11119 | 0.9276577 | 0.0355375 |
| TidyDensity | 3 | EARTH | Test | 0.7724133 | 147.60021 | 0.7495606 | 124.29453 | 0.9098680 | 0.0355375 |
| TidyDensity | 4 | NNAR | Test | 0.7379878 | 98.00322 | 0.7161536 | 137.36311 | 0.9135716 | 0.0978212 |
| tidyAML | 1 | ARIMA | Test | 0.8120153 | 160.82525 | 0.7719293 | 98.85269 | 0.9336925 | 0.1138654 |
| tidyAML | 2 | LM | Test | 0.8880996 | 139.19787 | 0.8442576 | 111.29307 | 1.0364654 | 0.0030298 |
| tidyAML | 3 | EARTH | Test | 0.8687054 | 183.03301 | 0.8258209 | 101.93757 | 0.9676339 | 0.0030298 |
| tidyAML | 4 | NNAR | Test | 0.8296892 | 149.69299 | 0.7887307 | 101.25488 | 0.9661832 | 0.0613188 |
| RandomWalker | 1 | ARIMA | Test | 0.7325118 | 101.59503 | 0.3481686 | 77.33402 | 1.0341870 | 0.4432821 |
| RandomWalker | 2 | LM | Test | 1.2283405 | 97.06055 | 0.5838399 | 164.50765 | 1.3899816 | 0.0384480 |
| RandomWalker | 3 | EARTH | Test | 1.2290570 | 98.31768 | 0.5841804 | 163.41799 | 1.3917821 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2525171 | 128.94394 | 0.5953312 | 131.82267 | 1.5223530 | 0.0115339 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.792 263.  0.646 152.  0.910 0.0908 
    ## 2 healthyR             4 NNAR        Test  0.627  93.0 0.604 152.  0.793 0.0886 
    ## 3 healthyR.ts          1 ARIMA       Test  0.823 128.  0.646 121.  1.05  0.00797
    ## 4 healthyverse         1 ARIMA       Test  0.562 199.  0.786 100.  0.698 0.0344 
    ## 5 healthyR.ai          1 ARIMA       Test  0.607  92.8 0.712 162.  0.727 0.142  
    ## 6 TidyDensity          1 ARIMA       Test  0.754 158.  0.732 121.  0.886 0.0491 
    ## 7 tidyAML              1 ARIMA       Test  0.812 161.  0.772  98.9 0.934 0.114  
    ## 8 RandomWalker         1 ARIMA       Test  0.733 102.  0.348  77.3 1.03  0.443

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1444|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1437|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1383|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1354|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1180|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1034|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [650|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [84|28]>   <mdl_tm_t [1 × 5]>

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
