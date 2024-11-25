Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
25 November, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 122,027
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

The last day in the data set is 2024-11-23 22:02:25, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.099407^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 122027        |
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
| r_version     |     86306 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     86306 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     86306 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10482 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-11-23 | 2023-03-21 | 1462 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1156290.91 | 1541914.06 | 355 | 14701 | 260378 | 2368098 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10345.17 | 17998.89 | 1 | 317 | 3098 | 11834 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-11-23 22:02:25 | 2023-03-21 16:42:23 | 73950 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 0S |       60 |

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
    ## -156.31  -34.24   -9.42   26.95  800.84 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.767e+02  8.254e+01
    ## date                                                1.061e-02  4.375e-03
    ## lag(value, 1)                                       1.353e-01  2.576e-02
    ## lag(value, 7)                                       9.774e-02  2.670e-02
    ## lag(value, 14)                                      1.066e-01  2.689e-02
    ## lag(value, 21)                                      3.416e-02  2.701e-02
    ## lag(value, 28)                                      8.016e-02  2.692e-02
    ## lag(value, 35)                                      6.987e-02  2.708e-02
    ## lag(value, 42)                                      3.665e-02  2.710e-02
    ## lag(value, 49)                                      1.107e-01  2.685e-02
    ## month(date, label = TRUE).L                        -1.105e+01  5.612e+00
    ## month(date, label = TRUE).Q                         1.545e+00  5.522e+00
    ## month(date, label = TRUE).C                        -1.217e+01  5.575e+00
    ## month(date, label = TRUE)^4                        -9.009e+00  5.495e+00
    ## month(date, label = TRUE)^5                        -1.409e+01  5.421e+00
    ## month(date, label = TRUE)^6                        -2.317e+00  5.454e+00
    ## month(date, label = TRUE)^7                        -9.894e+00  5.318e+00
    ## month(date, label = TRUE)^8                        -2.909e+00  5.293e+00
    ## month(date, label = TRUE)^9                         3.980e+00  5.274e+00
    ## month(date, label = TRUE)^10                        5.038e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -6.208e+00  5.285e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.155e+01  2.464e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.587e+00  2.574e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.141 0.032429 *  
    ## date                                                 2.426 0.015390 *  
    ## lag(value, 1)                                        5.255 1.71e-07 ***
    ## lag(value, 7)                                        3.660 0.000261 ***
    ## lag(value, 14)                                       3.965 7.71e-05 ***
    ## lag(value, 21)                                       1.265 0.206238    
    ## lag(value, 28)                                       2.978 0.002954 ** 
    ## lag(value, 35)                                       2.580 0.009968 ** 
    ## lag(value, 42)                                       1.353 0.176381    
    ## lag(value, 49)                                       4.121 4.00e-05 ***
    ## month(date, label = TRUE).L                         -1.969 0.049108 *  
    ## month(date, label = TRUE).Q                          0.280 0.779680    
    ## month(date, label = TRUE).C                         -2.182 0.029266 *  
    ## month(date, label = TRUE)^4                         -1.640 0.101332    
    ## month(date, label = TRUE)^5                         -2.599 0.009457 ** 
    ## month(date, label = TRUE)^6                         -0.425 0.671020    
    ## month(date, label = TRUE)^7                         -1.860 0.063026 .  
    ## month(date, label = TRUE)^8                         -0.550 0.582689    
    ## month(date, label = TRUE)^9                          0.755 0.450598    
    ## month(date, label = TRUE)^10                         0.956 0.339375    
    ## month(date, label = TRUE)^11                        -1.175 0.240320    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.685 3.07e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.560 0.010583 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.97 on 1390 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2612, Adjusted R-squared:  0.2495 
    ## F-statistic: 22.34 on 22 and 1390 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,428 × 2]> <tibble [28 × 2]> <split [1400|28]>
    ## 2 healthyR      <tibble [1,421 × 2]> <tibble [28 × 2]> <split [1393|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,367 × 2]> <tibble [28 × 2]> <split [1339|28]>
    ## 5 healthyverse  <tibble [1,338 × 2]> <tibble [28 × 2]> <split [1310|28]>
    ## 6 healthyR.ai   <tibble [1,164 × 2]> <tibble [28 × 2]> <split [1136|28]>
    ## 7 TidyDensity   <tibble [1,018 × 2]> <tibble [28 × 2]> <split [990|28]> 
    ## 8 tidyAML       <tibble [634 × 2]>   <tibble [28 × 2]> <split [606|28]> 
    ## 9 RandomWalker  <tibble [68 × 2]>    <tibble [28 × 2]> <split [40|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8496818 | 100.70338 | 0.6257086 | 177.00966 | 0.9996348 | 0.0029055 |
| healthyR.data | 2 | LM | Test | 0.8377784 | 116.85693 | 0.6169429 | 136.23448 | 0.9583093 | 0.0071816 |
| healthyR.data | 3 | EARTH | Test | 0.8404591 | 118.49954 | 0.6189170 | 137.83052 | 0.9546572 | 0.0071816 |
| healthyR.data | 4 | NNAR | Test | 0.8703940 | 100.86469 | 0.6409611 | 170.98318 | 1.0429810 | 0.0637237 |
| healthyR | 1 | ARIMA | Test | 0.6325280 | 198.41787 | 0.7371383 | 159.82757 | 0.7629051 | 0.0735498 |
| healthyR | 2 | LM | Test | 0.6363854 | 127.64202 | 0.7416337 | 181.08841 | 0.8127874 | 0.0050279 |
| healthyR | 3 | EARTH | Test | 0.6620669 | 276.70975 | 0.7715625 | 151.83007 | 0.7914482 | 0.0050279 |
| healthyR | 4 | NNAR | Test | 0.6302812 | 163.43551 | 0.7345199 | 154.44769 | 0.7807389 | 0.0916097 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 0.9407726 | 280.68156 | 0.7843126 | 119.65400 | 1.0957072 | 0.0196404 |
| healthyR.ts | 2 | LM | Test | 0.8831619 | 197.53432 | 0.7362831 | 124.46523 | 1.0760938 | 0.0196046 |
| healthyR.ts | 3 | EARTH | Test | 0.8821626 | 201.21055 | 0.7354500 | 123.38329 | 1.0755706 | 0.0196046 |
| healthyR.ts | 4 | NNAR | Test | 0.9523907 | 125.24819 | 0.7939985 | 173.40494 | 1.1784028 | 0.0011328 |
| healthyverse | 1 | ARIMA | Test | 0.6099538 | 100.34475 | 0.8897454 | 134.35082 | 0.7017088 | 0.0005900 |
| healthyverse | 2 | LM | Test | 0.5109697 | 146.81873 | 0.7453564 | 82.04334 | 0.6585820 | 0.0071737 |
| healthyverse | 3 | EARTH | Test | 0.7739521 | 105.78127 | 1.1289713 | 166.18212 | 0.9154153 | 0.0071737 |
| healthyverse | 4 | NNAR | Test | 0.5506956 | 102.71836 | 0.8033050 | 106.72079 | 0.6668842 | 0.0340382 |
| healthyR.ai | 1 | ARIMA | Test | 0.7570757 | 110.91808 | 0.8142015 | 172.52403 | 0.8703922 | 0.0073690 |
| healthyR.ai | 2 | LM | Test | 0.7254105 | 118.41562 | 0.7801470 | 145.20861 | 0.8868287 | 0.0190644 |
| healthyR.ai | 3 | EARTH | Test | 0.7953729 | 168.14759 | 0.8553884 | 161.53469 | 0.8983244 | 0.0190644 |
| healthyR.ai | 4 | NNAR | Test | 0.7351738 | 107.71004 | 0.7906469 | 149.88420 | 0.8940075 | 0.0006327 |
| TidyDensity | 1 | ARIMA | Test | 0.7027552 | 494.59951 | 0.6512608 | 124.44866 | 0.8234012 | 0.0499425 |
| TidyDensity | 2 | LM | Test | 0.7862808 | 698.27108 | 0.7286660 | 127.48442 | 0.8835821 | 0.0211464 |
| TidyDensity | 3 | EARTH | Test | 0.7000055 | 451.41105 | 0.6487125 | 132.16913 | 0.8267211 | 0.0211464 |
| TidyDensity | 4 | NNAR | Test | 0.6290081 | 225.18989 | 0.5829175 | 142.36882 | 0.8191060 | 0.1325659 |
| tidyAML | 1 | ARIMA | Test | 0.5039748 | 277.62651 | 0.6605133 | 90.13665 | 0.6302072 | 0.1158645 |
| tidyAML | 2 | LM | Test | 0.5178646 | 264.14036 | 0.6787175 | 89.30666 | 0.6508561 | 0.0298020 |
| tidyAML | 3 | EARTH | Test | 0.5131239 | 291.18005 | 0.6725042 | 86.21756 | 0.6475378 | 0.0298020 |
| tidyAML | 4 | NNAR | Test | 0.5178664 | 346.06136 | 0.6787197 | 82.95054 | 0.6547129 | 0.0079522 |
| RandomWalker | 1 | ARIMA | Test | 1.0981693 | 92.91542 | 0.5624310 | 132.89495 | 1.3806621 | 0.0057849 |
| RandomWalker | 2 | LM | Test | 1.2980210 | 152.08693 | 0.6647857 | 117.55708 | 1.6743629 | 0.0058280 |
| RandomWalker | 3 | EARTH | Test | 3.4876292 | 614.80118 | 1.7862007 | 151.15175 | 4.0320109 | 0.0058280 |
| RandomWalker | 4 | NNAR | Test | 3.6081958 | 384.54048 | 1.8479493 | 152.87627 | 5.5252486 | 0.0284462 |

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
    ## 1 healthyR.da…         3 EARTH       Test  0.840 118.  0.619 138.  0.955 0.00718
    ## 2 healthyR             1 ARIMA       Test  0.633 198.  0.737 160.  0.763 0.0735 
    ## 3 healthyR.ts          3 EARTH       Test  0.882 201.  0.735 123.  1.08  0.0196 
    ## 4 healthyverse         2 LM          Test  0.511 147.  0.745  82.0 0.659 0.00717
    ## 5 healthyR.ai          1 ARIMA       Test  0.757 111.  0.814 173.  0.870 0.00737
    ## 6 TidyDensity          4 NNAR        Test  0.629 225.  0.583 142.  0.819 0.133  
    ## 7 tidyAML              1 ARIMA       Test  0.504 278.  0.661  90.1 0.630 0.116  
    ## 8 RandomWalker         1 ARIMA       Test  1.10   92.9 0.562 133.  1.38  0.00578

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1400|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1393|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1339|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1310|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1136|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [990|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [606|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [40|28]>   <mdl_tm_t [1 × 5]>

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
