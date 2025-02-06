Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 129,472
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

The last day in the data set is 2025-02-04 22:25:09, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4354.82
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 129472        |
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
| r_version     |     92388 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     92388 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     92388 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10920 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-04 | 2023-04-26 | 1535 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1141242.75 | 1531148.11 | 355 | 14701 | 260378 | 2367858 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10483.03 | 18479.48 | 1 | 336 | 3098 | 11961 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-04 22:25:09 | 2023-04-26 20:39:45 | 78405 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     10 |       60 |

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
    ## -155.36  -35.03   -9.48   26.95  807.71 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.803e+02  7.731e+01
    ## date                                                1.089e-02  4.098e-03
    ## lag(value, 1)                                       1.178e-01  2.520e-02
    ## lag(value, 7)                                       9.257e-02  2.619e-02
    ## lag(value, 14)                                      1.071e-01  2.630e-02
    ## lag(value, 21)                                      4.886e-02  2.640e-02
    ## lag(value, 28)                                      6.348e-02  2.625e-02
    ## lag(value, 35)                                      7.220e-02  2.642e-02
    ## lag(value, 42)                                      5.432e-02  2.649e-02
    ## lag(value, 49)                                      9.302e-02  2.637e-02
    ## month(date, label = TRUE).L                        -1.057e+01  5.278e+00
    ## month(date, label = TRUE).Q                         1.736e+00  5.176e+00
    ## month(date, label = TRUE).C                        -1.122e+01  5.260e+00
    ## month(date, label = TRUE)^4                        -7.531e+00  5.282e+00
    ## month(date, label = TRUE)^5                        -1.332e+01  5.283e+00
    ## month(date, label = TRUE)^6                        -6.285e-01  5.367e+00
    ## month(date, label = TRUE)^7                        -9.413e+00  5.284e+00
    ## month(date, label = TRUE)^8                        -2.069e+00  5.288e+00
    ## month(date, label = TRUE)^9                         4.066e+00  5.285e+00
    ## month(date, label = TRUE)^10                        5.275e+00  5.286e+00
    ## month(date, label = TRUE)^11                       -6.324e+00  5.299e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.167e+01  2.415e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.524e+00  2.540e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.332 0.019815 *  
    ## date                                                 2.657 0.007967 ** 
    ## lag(value, 1)                                        4.674 3.22e-06 ***
    ## lag(value, 7)                                        3.534 0.000422 ***
    ## lag(value, 14)                                       4.072 4.90e-05 ***
    ## lag(value, 21)                                       1.851 0.064342 .  
    ## lag(value, 28)                                       2.418 0.015724 *  
    ## lag(value, 35)                                       2.732 0.006364 ** 
    ## lag(value, 42)                                       2.051 0.040469 *  
    ## lag(value, 49)                                       3.527 0.000433 ***
    ## month(date, label = TRUE).L                         -2.002 0.045483 *  
    ## month(date, label = TRUE).Q                          0.335 0.737301    
    ## month(date, label = TRUE).C                         -2.133 0.033065 *  
    ## month(date, label = TRUE)^4                         -1.426 0.154162    
    ## month(date, label = TRUE)^5                         -2.521 0.011798 *  
    ## month(date, label = TRUE)^6                         -0.117 0.906791    
    ## month(date, label = TRUE)^7                         -1.781 0.075043 .  
    ## month(date, label = TRUE)^8                         -0.391 0.695670    
    ## month(date, label = TRUE)^9                          0.769 0.441806    
    ## month(date, label = TRUE)^10                         0.998 0.318419    
    ## month(date, label = TRUE)^11                        -1.193 0.232885    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.834 1.48e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.962 0.003103 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.15 on 1463 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2579, Adjusted R-squared:  0.2468 
    ## F-statistic: 23.12 on 22 and 1463 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,500 × 2]> <tibble [28 × 2]> <split [1472|28]>
    ## 2 healthyR      <tibble [1,493 × 2]> <tibble [28 × 2]> <split [1465|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,439 × 2]> <tibble [28 × 2]> <split [1411|28]>
    ## 5 healthyverse  <tibble [1,410 × 2]> <tibble [28 × 2]> <split [1382|28]>
    ## 6 healthyR.ai   <tibble [1,236 × 2]> <tibble [28 × 2]> <split [1208|28]>
    ## 7 TidyDensity   <tibble [1,090 × 2]> <tibble [28 × 2]> <split [1062|28]>
    ## 8 tidyAML       <tibble [705 × 2]>   <tibble [28 × 2]> <split [677|28]> 
    ## 9 RandomWalker  <tibble [139 × 2]>   <tibble [28 × 2]> <split [111|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8689934 | 111.22323 | 0.6496687 | 144.56478 | 1.0658737 | 0.0186264 |
| healthyR.data | 2 | LM | Test | 0.9560651 | 162.27438 | 0.7147644 | 143.61129 | 1.1382971 | 0.0383605 |
| healthyR.data | 3 | EARTH | Test | 0.9926843 | 191.65352 | 0.7421412 | 136.99853 | 1.2068479 | 0.0383605 |
| healthyR.data | 4 | NNAR | Test | 0.8479957 | 101.95738 | 0.6339706 | 171.58510 | 1.0071756 | 0.1943113 |
| healthyR | 1 | ARIMA | Test | 0.8197108 | 116.90542 | 0.7138910 | 163.54049 | 0.9997972 | 0.0087417 |
| healthyR | 2 | LM | Test | 0.8047632 | 101.31882 | 0.7008730 | 188.88051 | 0.9846279 | 0.0582936 |
| healthyR | 3 | EARTH | Test | 0.7950812 | 101.30841 | 0.6924409 | 175.44977 | 0.9793370 | 0.0582936 |
| healthyR | 4 | NNAR | Test | 0.7705079 | 105.35435 | 0.6710399 | 155.58828 | 0.9996754 | 0.0018181 |
| healthyR.ts | 1 | ARIMA | Test | 0.7810671 | 212.41683 | 0.6453709 | 133.71122 | 1.0327506 | 0.0309210 |
| healthyR.ts | 2 | LM | Test | 0.8194936 | 252.78918 | 0.6771214 | 134.02341 | 1.0503868 | 0.0309210 |
| healthyR.ts | 3 | EARTH | Test | 0.8257589 | 258.78270 | 0.6822983 | 134.18406 | 1.0534759 | 0.0309210 |
| healthyR.ts | 4 | NNAR | Test | 0.6433918 | 110.16140 | 0.5316142 | 166.57154 | 0.9156825 | 0.2196559 |
| healthyverse | 1 | ARIMA | Test | 0.6535045 | 194.61995 | 0.6598526 | 103.16906 | 0.7931638 | 0.0074608 |
| healthyverse | 2 | LM | Test | 0.6322145 | 259.37852 | 0.6383558 | 89.43348 | 0.7742299 | 0.0020418 |
| healthyverse | 3 | EARTH | Test | 0.9886453 | 160.28168 | 0.9982489 | 183.11677 | 1.1514998 | 0.0020418 |
| healthyverse | 4 | NNAR | Test | 0.6878361 | 161.02588 | 0.6945177 | 116.41343 | 0.8307009 | 0.0068821 |
| healthyR.ai | 1 | ARIMA | Test | 0.7423041 | 113.17028 | 0.7505069 | 185.29080 | 0.9069405 | 0.0259217 |
| healthyR.ai | 2 | LM | Test | 0.7238386 | 116.74452 | 0.7318373 | 152.07273 | 0.9182209 | 0.0353723 |
| healthyR.ai | 3 | EARTH | Test | 0.7122062 | 139.73287 | 0.7200764 | 130.06870 | 0.9353571 | 0.0353723 |
| healthyR.ai | 4 | NNAR | Test | 0.6918800 | 108.76691 | 0.6995256 | 159.07494 | 0.8547991 | 0.1560891 |
| TidyDensity | 1 | ARIMA | Test | 0.6661307 | 198.18828 | 0.7697391 | 116.79899 | 0.7810069 | 0.0626865 |
| TidyDensity | 2 | LM | Test | 0.7269413 | 234.69023 | 0.8400080 | 113.84065 | 0.8696536 | 0.0125359 |
| TidyDensity | 3 | EARTH | Test | 0.6264824 | 157.85223 | 0.7239240 | 112.72605 | 0.7806528 | 0.0125359 |
| TidyDensity | 4 | NNAR | Test | 0.5754760 | 101.44082 | 0.6649842 | 127.95041 | 0.7195347 | 0.1216806 |
| tidyAML | 1 | ARIMA | Test | 0.7223579 | 188.36373 | 0.7654802 | 87.69334 | 0.9011938 | 0.0784561 |
| tidyAML | 2 | LM | Test | 0.7613345 | 168.65935 | 0.8067835 | 93.02263 | 0.9439243 | 0.0001087 |
| tidyAML | 3 | EARTH | Test | 0.7249119 | 202.21666 | 0.7681866 | 82.65327 | 0.9409108 | 0.0001087 |
| tidyAML | 4 | NNAR | Test | 0.7694626 | 151.12027 | 0.8153968 | 96.92490 | 0.9670839 | 0.0000036 |
| RandomWalker | 1 | ARIMA | Test | 1.3611023 | 129.63292 | 0.6510713 | 170.06371 | 1.5022393 | 0.0044086 |
| RandomWalker | 2 | LM | Test | 1.3032471 | 97.35406 | 0.6233968 | 186.82619 | 1.4489667 | 0.0005394 |
| RandomWalker | 3 | EARTH | Test | 1.2918252 | 95.38172 | 0.6179332 | 166.65321 | 1.4563019 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4968178 | 166.23915 | 0.7159896 | 146.46998 | 1.7687291 | 0.0157598 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.848 102.  0.634 172.  1.01  1.94e-1
    ## 2 healthyR             3 EARTH       Test  0.795 101.  0.692 175.  0.979 5.83e-2
    ## 3 healthyR.ts          4 NNAR        Test  0.643 110.  0.532 167.  0.916 2.20e-1
    ## 4 healthyverse         2 LM          Test  0.632 259.  0.638  89.4 0.774 2.04e-3
    ## 5 healthyR.ai          4 NNAR        Test  0.692 109.  0.700 159.  0.855 1.56e-1
    ## 6 TidyDensity          4 NNAR        Test  0.575 101.  0.665 128.  0.720 1.22e-1
    ## 7 tidyAML              1 ARIMA       Test  0.722 188.  0.765  87.7 0.901 7.85e-2
    ## 8 RandomWalker         2 LM          Test  1.30   97.4 0.623 187.  1.45  5.39e-4

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1472|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1465|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1411|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1382|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1208|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1062|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [677|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [111|28]>  <mdl_tm_t [1 × 5]>

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
