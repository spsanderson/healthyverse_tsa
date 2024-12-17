Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,099
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

The last day in the data set is 2024-12-15 23:25:11, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3131.82
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124099        |
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
| r_version     |     87935 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     87935 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     87935 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10632 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-15 | 2023-04-05 | 1484 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1150363.97 | 1537649.33 | 355 | 14701 | 260378 | 2368012 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10348.43 | 17979.55 | 1 | 334 | 3101 | 11854 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-15 23:25:11 | 2023-04-05 21:47:08 | 75167 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 52S |       60 |

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
    ## -156.92  -34.78   -9.73   26.91  802.40 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -177.26841   80.76443
    ## date                                                  0.01065    0.00428
    ## lag(value, 1)                                         0.13525    0.02560
    ## lag(value, 7)                                         0.09373    0.02654
    ## lag(value, 14)                                        0.10538    0.02658
    ## lag(value, 21)                                        0.04372    0.02670
    ## lag(value, 28)                                        0.07319    0.02652
    ## lag(value, 35)                                        0.06617    0.02678
    ## lag(value, 42)                                        0.04313    0.02683
    ## lag(value, 49)                                        0.10929    0.02673
    ## month(date, label = TRUE).L                         -10.93538    5.52006
    ## month(date, label = TRUE).Q                           2.10521    5.38206
    ## month(date, label = TRUE).C                         -11.16897    5.44815
    ## month(date, label = TRUE)^4                          -7.62234    5.40194
    ## month(date, label = TRUE)^5                         -12.61784    5.34182
    ## month(date, label = TRUE)^6                          -1.02418    5.39340
    ## month(date, label = TRUE)^7                          -8.90949    5.27980
    ## month(date, label = TRUE)^8                          -2.35620    5.27176
    ## month(date, label = TRUE)^9                           4.28070    5.26145
    ## month(date, label = TRUE)^10                          5.14726    5.26085
    ## month(date, label = TRUE)^11                         -6.17679    5.27450
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.61966    2.44608
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    6.75756    2.55570
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.195 0.028334 *  
    ## date                                                 2.488 0.012972 *  
    ## lag(value, 1)                                        5.283 1.47e-07 ***
    ## lag(value, 7)                                        3.531 0.000428 ***
    ## lag(value, 14)                                       3.965 7.70e-05 ***
    ## lag(value, 21)                                       1.637 0.101777    
    ## lag(value, 28)                                       2.759 0.005868 ** 
    ## lag(value, 35)                                       2.471 0.013608 *  
    ## lag(value, 42)                                       1.607 0.108204    
    ## lag(value, 49)                                       4.089 4.57e-05 ***
    ## month(date, label = TRUE).L                         -1.981 0.047782 *  
    ## month(date, label = TRUE).Q                          0.391 0.695742    
    ## month(date, label = TRUE).C                         -2.050 0.040544 *  
    ## month(date, label = TRUE)^4                         -1.411 0.158454    
    ## month(date, label = TRUE)^5                         -2.362 0.018307 *  
    ## month(date, label = TRUE)^6                         -0.190 0.849419    
    ## month(date, label = TRUE)^7                         -1.687 0.091735 .  
    ## month(date, label = TRUE)^8                         -0.447 0.654982    
    ## month(date, label = TRUE)^9                          0.814 0.416013    
    ## month(date, label = TRUE)^10                         0.978 0.328040    
    ## month(date, label = TRUE)^11                        -1.171 0.241770    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.750 2.24e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.644 0.008281 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.86 on 1412 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2601, Adjusted R-squared:  0.2486 
    ## F-statistic: 22.56 on 22 and 1412 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,450 × 2]> <tibble [28 × 2]> <split [1422|28]>
    ## 2 healthyR      <tibble [1,443 × 2]> <tibble [28 × 2]> <split [1415|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,389 × 2]> <tibble [28 × 2]> <split [1361|28]>
    ## 5 healthyverse  <tibble [1,360 × 2]> <tibble [28 × 2]> <split [1332|28]>
    ## 6 healthyR.ai   <tibble [1,186 × 2]> <tibble [28 × 2]> <split [1158|28]>
    ## 7 TidyDensity   <tibble [1,040 × 2]> <tibble [28 × 2]> <split [1012|28]>
    ## 8 tidyAML       <tibble [656 × 2]>   <tibble [28 × 2]> <split [628|28]> 
    ## 9 RandomWalker  <tibble [90 × 2]>    <tibble [28 × 2]> <split [62|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7224051 | 173.08162 | 0.6064472 | 136.22387 | 0.8947881 | 0.1807971 |
| healthyR.data | 2 | LM | Test | 0.8097971 | 244.17539 | 0.6798113 | 141.69469 | 0.9458920 | 0.0248388 |
| healthyR.data | 3 | EARTH | Test | 0.8133823 | 290.17531 | 0.6828210 | 135.76026 | 0.9316635 | 0.0248388 |
| healthyR.data | 4 | NNAR | Test | 0.8446934 | 108.62488 | 0.7091062 | 161.78807 | 1.0963557 | 0.0027760 |
| healthyR | 1 | ARIMA | Test | 0.6123769 | 157.38537 | 0.7368904 | 154.69237 | 0.7513745 | 0.0459941 |
| healthyR | 2 | LM | Test | 0.6246826 | 114.38040 | 0.7516982 | 187.96059 | 0.7578237 | 0.0050868 |
| healthyR | 3 | EARTH | Test | 0.6547203 | 230.75403 | 0.7878433 | 144.25901 | 0.7970638 | 0.0050868 |
| healthyR | 4 | NNAR | Test | 0.6111543 | 162.12236 | 0.7354192 | 165.92982 | 0.7357017 | 0.0646544 |
| healthyR.ts | 1 | ARIMA | Test | 0.9217025 | 114.95158 | 0.8929080 | 118.64009 | 1.0885216 | 0.0248436 |
| healthyR.ts | 2 | LM | Test | 0.9026599 | 103.55403 | 0.8744603 | 124.29373 | 1.0496547 | 0.0248436 |
| healthyR.ts | 3 | EARTH | Test | 0.9036771 | 104.53309 | 0.8754458 | 123.52893 | 1.0527348 | 0.0248436 |
| healthyR.ts | 4 | NNAR | Test | 0.9134175 | 101.80798 | 0.8848818 | 183.55977 | 1.0669241 | 0.0033529 |
| healthyverse | 1 | ARIMA | Test | 0.4802973 | 190.56082 | 0.8619190 | 87.06202 | 0.6091231 | 0.3675443 |
| healthyverse | 2 | LM | Test | 0.5207353 | 326.04790 | 0.9344871 | 82.12150 | 0.6346660 | 0.0107984 |
| healthyverse | 3 | EARTH | Test | 0.5887228 | 190.73062 | 1.0564942 | 107.33497 | 0.7126986 | 0.0107984 |
| healthyverse | 4 | NNAR | Test | 0.5628419 | 195.79368 | 1.0100495 | 102.51681 | 0.6879663 | 0.1549689 |
| healthyR.ai | 1 | ARIMA | Test | 0.7405904 | 98.92364 | 1.0113781 | 160.68484 | 0.8350400 | 0.1092173 |
| healthyR.ai | 2 | LM | Test | 0.7631154 | 123.43946 | 1.0421391 | 169.63113 | 0.8237495 | 0.0000979 |
| healthyR.ai | 3 | EARTH | Test | 0.8179056 | 131.59121 | 1.1169626 | 147.33978 | 0.9474284 | 0.0000979 |
| healthyR.ai | 4 | NNAR | Test | 0.7247268 | 118.95241 | 0.9897142 | 161.64455 | 0.7966302 | 0.0923367 |
| TidyDensity | 1 | ARIMA | Test | 0.6674846 | 192.13932 | 0.6957559 | 116.79131 | 0.8042314 | 0.1683130 |
| TidyDensity | 2 | LM | Test | 0.7459799 | 272.33991 | 0.7775758 | 110.17365 | 0.8856494 | 0.0511089 |
| TidyDensity | 3 | EARTH | Test | 0.7208602 | 205.04206 | 0.7513922 | 118.38187 | 0.8558604 | 0.0511089 |
| TidyDensity | 4 | NNAR | Test | 0.7719255 | 118.34266 | 0.8046204 | 154.53210 | 0.9605326 | 0.0134454 |
| tidyAML | 1 | ARIMA | Test | 0.6655918 | 85.21558 | 0.8758767 | 90.17467 | 0.8277190 | 0.1264376 |
| tidyAML | 2 | LM | Test | 0.6859776 | 86.02108 | 0.9027032 | 86.16507 | 0.8645309 | 0.0262609 |
| tidyAML | 3 | EARTH | Test | 0.6475822 | 111.24724 | 0.8521773 | 77.57123 | 0.7819991 | 0.0262609 |
| tidyAML | 4 | NNAR | Test | 0.6543165 | 103.40657 | 0.8610392 | 81.14925 | 0.7843180 | 0.0663248 |
| RandomWalker | 1 | ARIMA | Test | 1.2839776 | 107.71340 | 0.5917289 | 171.32610 | 1.3943166 | 0.0905899 |
| RandomWalker | 2 | LM | Test | 1.3411670 | 99.63534 | 0.6180850 | 193.39120 | 1.4691871 | 0.0077228 |
| RandomWalker | 3 | EARTH | Test | 1.4264869 | 133.74736 | 0.6574051 | 109.85502 | 1.8458908 | 0.0077228 |
| RandomWalker | 4 | NNAR | Test | 1.3858757 | 134.45848 | 0.6386893 | 145.67933 | 1.5914728 | 0.0134437 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.722  173. 0.606 136.  0.895 0.181 
    ## 2 healthyR              4 NNAR        Test  0.611  162. 0.735 166.  0.736 0.0647
    ## 3 healthyR.ts           2 LM          Test  0.903  104. 0.874 124.  1.05  0.0248
    ## 4 healthyverse          1 ARIMA       Test  0.480  191. 0.862  87.1 0.609 0.368 
    ## 5 healthyR.ai           4 NNAR        Test  0.725  119. 0.990 162.  0.797 0.0923
    ## 6 TidyDensity           1 ARIMA       Test  0.667  192. 0.696 117.  0.804 0.168 
    ## 7 tidyAML               3 EARTH       Test  0.648  111. 0.852  77.6 0.782 0.0263
    ## 8 RandomWalker          1 ARIMA       Test  1.28   108. 0.592 171.  1.39  0.0906

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1422|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1415|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1361|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1332|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1158|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1012|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [628|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [62|28]>   <mdl_tm_t [1 × 5]>

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
