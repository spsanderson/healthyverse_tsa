Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
13 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 123,476
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

The last day in the data set is 2024-12-10 22:26:02, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3010.84
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 123476        |
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
| r_version     |     87411 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     87411 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     87411 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10593 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-10 | 2023-03-30 | 1479 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1153084.8 | 1539265.78 | 355 | 14701 | 260378 | 2368012 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10361.9 | 17994.44 | 1 | 333 | 3115 | 11896 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-10 22:26:02 | 2023-03-30 23:26:28 | 74800 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     53 |       60 |

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
    ## -156.89  -34.68   -9.55   26.59  801.51 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.672e+02  8.107e+01
    ## date                                                1.011e-02  4.296e-03
    ## lag(value, 1)                                       1.367e-01  2.563e-02
    ## lag(value, 7)                                       9.424e-02  2.657e-02
    ## lag(value, 14)                                      1.061e-01  2.660e-02
    ## lag(value, 21)                                      4.159e-02  2.673e-02
    ## lag(value, 28)                                      7.749e-02  2.664e-02
    ## lag(value, 35)                                      6.640e-02  2.682e-02
    ## lag(value, 42)                                      3.864e-02  2.695e-02
    ## lag(value, 49)                                      1.111e-01  2.676e-02
    ## month(date, label = TRUE).L                        -1.149e+01  5.539e+00
    ## month(date, label = TRUE).Q                         1.314e+00  5.418e+00
    ## month(date, label = TRUE).C                        -1.192e+01  5.481e+00
    ## month(date, label = TRUE)^4                        -8.166e+00  5.421e+00
    ## month(date, label = TRUE)^5                        -1.303e+01  5.353e+00
    ## month(date, label = TRUE)^6                        -1.238e+00  5.397e+00
    ## month(date, label = TRUE)^7                        -9.082e+00  5.281e+00
    ## month(date, label = TRUE)^8                        -2.388e+00  5.272e+00
    ## month(date, label = TRUE)^9                         4.259e+00  5.261e+00
    ## month(date, label = TRUE)^10                        5.115e+00  5.261e+00
    ## month(date, label = TRUE)^11                       -6.153e+00  5.274e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.163e+01  2.447e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.680e+00  2.559e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.062 0.039410 *  
    ## date                                                 2.353 0.018779 *  
    ## lag(value, 1)                                        5.334 1.12e-07 ***
    ## lag(value, 7)                                        3.547 0.000402 ***
    ## lag(value, 14)                                       3.988 6.99e-05 ***
    ## lag(value, 21)                                       1.556 0.119999    
    ## lag(value, 28)                                       2.909 0.003687 ** 
    ## lag(value, 35)                                       2.476 0.013413 *  
    ## lag(value, 42)                                       1.434 0.151893    
    ## lag(value, 49)                                       4.152 3.49e-05 ***
    ## month(date, label = TRUE).L                         -2.075 0.038190 *  
    ## month(date, label = TRUE).Q                          0.242 0.808440    
    ## month(date, label = TRUE).C                         -2.175 0.029792 *  
    ## month(date, label = TRUE)^4                         -1.506 0.132231    
    ## month(date, label = TRUE)^5                         -2.435 0.015014 *  
    ## month(date, label = TRUE)^6                         -0.229 0.818656    
    ## month(date, label = TRUE)^7                         -1.720 0.085697 .  
    ## month(date, label = TRUE)^8                         -0.453 0.650603    
    ## month(date, label = TRUE)^9                          0.809 0.418394    
    ## month(date, label = TRUE)^10                         0.972 0.331095    
    ## month(date, label = TRUE)^11                        -1.167 0.243572    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.752 2.22e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.610 0.009138 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.86 on 1407 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2604, Adjusted R-squared:  0.2488 
    ## F-statistic: 22.52 on 22 and 1407 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,445 × 2]> <tibble [28 × 2]> <split [1417|28]>
    ## 2 healthyR      <tibble [1,438 × 2]> <tibble [28 × 2]> <split [1410|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,384 × 2]> <tibble [28 × 2]> <split [1356|28]>
    ## 5 healthyverse  <tibble [1,355 × 2]> <tibble [28 × 2]> <split [1327|28]>
    ## 6 healthyR.ai   <tibble [1,181 × 2]> <tibble [28 × 2]> <split [1153|28]>
    ## 7 TidyDensity   <tibble [1,035 × 2]> <tibble [28 × 2]> <split [1007|28]>
    ## 8 tidyAML       <tibble [651 × 2]>   <tibble [28 × 2]> <split [623|28]> 
    ## 9 RandomWalker  <tibble [85 × 2]>    <tibble [28 × 2]> <split [57|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7952219 | 145.01803 | 0.6870695 | 156.76096 | 0.9705095 | 0.0764152 |
| healthyR.data | 2 | LM | Test | 0.8275628 | 242.54022 | 0.7150120 | 144.95637 | 0.9428013 | 0.0073773 |
| healthyR.data | 3 | EARTH | Test | 0.8265982 | 253.90153 | 0.7141785 | 144.64410 | 0.9403952 | 0.0073773 |
| healthyR.data | 4 | NNAR | Test | 0.8913404 | 117.30454 | 0.7701156 | 156.73039 | 1.1377505 | 0.0589105 |
| healthyR | 1 | ARIMA | Test | 0.6947903 | 162.46431 | 0.8072996 | 152.74034 | 0.8314922 | 0.0471482 |
| healthyR | 2 | LM | Test | 0.6530909 | 111.21582 | 0.7588476 | 188.05504 | 0.7978812 | 0.0082925 |
| healthyR | 3 | EARTH | Test | 0.7259533 | 243.76716 | 0.8435088 | 154.81335 | 0.8634266 | 0.0082925 |
| healthyR | 4 | NNAR | Test | 0.6963756 | 189.92313 | 0.8091415 | 171.78349 | 0.8170566 | 0.0001197 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 1.0973197 | 154.87879 | 0.8669879 | 135.07630 | 1.2516080 | 0.0126011 |
| healthyR.ts | 2 | LM | Test | 1.0369617 | 129.97459 | 0.8192993 | 140.53867 | 1.1788278 | 0.0126011 |
| healthyR.ts | 3 | EARTH | Test | 1.0401402 | 131.55409 | 0.8218106 | 139.89106 | 1.1831929 | 0.0126011 |
| healthyR.ts | 4 | NNAR | Test | 1.0121786 | 149.66877 | 0.7997183 | 190.14014 | 1.1141333 | 0.0282556 |
| healthyverse | 1 | ARIMA | Test | 0.5766104 | 147.84099 | 1.0307861 | 102.10904 | 0.6882090 | 0.3215684 |
| healthyverse | 2 | LM | Test | 0.5373755 | 221.94702 | 0.9606474 | 80.25260 | 0.6553179 | 0.0767883 |
| healthyverse | 3 | EARTH | Test | 0.6325666 | 153.56845 | 1.1308172 | 107.47288 | 0.7361653 | 0.0767883 |
| healthyverse | 4 | NNAR | Test | 0.6457500 | 156.34284 | 1.1543846 | 112.85009 | 0.7416357 | 0.0164443 |
| healthyR.ai | 1 | ARIMA | Test | 0.7997497 | 113.25099 | 1.0206422 | 164.80852 | 0.8937494 | 0.0996335 |
| healthyR.ai | 2 | LM | Test | 0.7628559 | 117.10988 | 0.9735583 | 160.96673 | 0.8536685 | 0.0200558 |
| healthyR.ai | 3 | EARTH | Test | 0.8777701 | 145.23056 | 1.1202120 | 159.10294 | 0.9946942 | 0.0200558 |
| healthyR.ai | 4 | NNAR | Test | 0.7774862 | 124.76860 | 0.9922295 | 167.49228 | 0.8434183 | 0.0535434 |
| TidyDensity | 1 | ARIMA | Test | 0.7228205 | 235.52069 | 0.7190706 | 120.93548 | 0.8774207 | 0.0002429 |
| TidyDensity | 2 | LM | Test | 0.7859211 | 291.49761 | 0.7818438 | 120.44073 | 0.9382918 | 0.0071383 |
| TidyDensity | 3 | EARTH | Test | 0.7265742 | 240.48852 | 0.7228048 | 121.63967 | 0.8744928 | 0.0071383 |
| TidyDensity | 4 | NNAR | Test | 0.6692985 | 114.34325 | 0.6658263 | 144.12124 | 0.8337547 | 0.0071852 |
| tidyAML | 1 | ARIMA | Test | 0.7160718 | 86.22105 | 0.7919585 | 96.79861 | 0.8855710 | 0.1201893 |
| tidyAML | 2 | LM | Test | 0.7261535 | 91.05723 | 0.8031086 | 93.14916 | 0.8918339 | 0.0487264 |
| tidyAML | 3 | EARTH | Test | 0.6712593 | 109.71105 | 0.7423970 | 81.10917 | 0.8124736 | 0.0487264 |
| tidyAML | 4 | NNAR | Test | 0.6817359 | 97.97878 | 0.7539838 | 87.33977 | 0.8290533 | 0.0634736 |
| RandomWalker | 1 | ARIMA | Test | 1.3249551 | 100.00000 | 0.6093817 | 200.00000 | 1.4706729 | NA |
| RandomWalker | 2 | LM | Test | 1.3266791 | 99.29588 | 0.6101746 | 192.56915 | 1.4746018 | 0.0137429 |
| RandomWalker | 3 | EARTH | Test | 1.3536257 | 133.59802 | 0.6225681 | 114.05156 | 1.7187045 | 0.0137429 |
| RandomWalker | 4 | NNAR | Test | 1.5948158 | 167.64474 | 0.7334978 | 153.05779 | 1.8535844 | 0.0004085 |

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
    ## 1 healthyR.d…         3 EARTH       Test  0.827  254. 0.714 145.  0.940  0.00738
    ## 2 healthyR            2 LM          Test  0.653  111. 0.759 188.  0.798  0.00829
    ## 3 healthyR.ts         4 NNAR        Test  1.01   150. 0.800 190.  1.11   0.0283 
    ## 4 healthyver…         2 LM          Test  0.537  222. 0.961  80.3 0.655  0.0768 
    ## 5 healthyR.ai         4 NNAR        Test  0.777  125. 0.992 167.  0.843  0.0535 
    ## 6 TidyDensity         4 NNAR        Test  0.669  114. 0.666 144.  0.834  0.00719
    ## 7 tidyAML             3 EARTH       Test  0.671  110. 0.742  81.1 0.812  0.0487 
    ## 8 RandomWalk…         1 ARIMA       Test  1.32   100  0.609 200   1.47  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1417|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1410|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1356|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1327|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1153|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1007|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [623|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [57|28]>   <mdl_tm_t [1 × 5]>

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
