Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,157
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

The last day in the data set is 2024-12-16 23:40:52, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3156.09
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124157        |
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
| r_version     |     87983 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     87983 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     87983 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10640 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-16 | 2023-04-06 | 1485 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1150627.45 | 1537655.27 | 355 | 14701 | 260378 | 2368012 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10355.34 | 17994.79 | 1 | 334 | 3103 | 11862 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-16 23:40:52 | 2023-04-06 01:18:48 | 75222 |

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
    ## -156.97  -34.73   -9.82   26.97  802.41 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.762e+02  8.066e+01
    ## date                                                1.059e-02  4.274e-03
    ## lag(value, 1)                                       1.352e-01  2.559e-02
    ## lag(value, 7)                                       9.389e-02  2.653e-02
    ## lag(value, 14)                                      1.052e-01  2.656e-02
    ## lag(value, 21)                                      4.396e-02  2.668e-02
    ## lag(value, 28)                                      7.333e-02  2.651e-02
    ## lag(value, 35)                                      6.592e-02  2.676e-02
    ## lag(value, 42)                                      4.326e-02  2.682e-02
    ## lag(value, 49)                                      1.094e-01  2.671e-02
    ## month(date, label = TRUE).L                        -1.100e+01  5.514e+00
    ## month(date, label = TRUE).Q                         2.025e+00  5.374e+00
    ## month(date, label = TRUE).C                        -1.125e+01  5.439e+00
    ## month(date, label = TRUE)^4                        -7.682e+00  5.397e+00
    ## month(date, label = TRUE)^5                        -1.266e+01  5.338e+00
    ## month(date, label = TRUE)^6                        -1.050e+00  5.391e+00
    ## month(date, label = TRUE)^7                        -8.923e+00  5.278e+00
    ## month(date, label = TRUE)^8                        -2.365e+00  5.270e+00
    ## month(date, label = TRUE)^9                         4.280e+00  5.260e+00
    ## month(date, label = TRUE)^10                        5.148e+00  5.259e+00
    ## month(date, label = TRUE)^11                       -6.176e+00  5.273e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.160e+01  2.444e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.774e+00  2.554e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.184 0.029106 *  
    ## date                                                 2.477 0.013351 *  
    ## lag(value, 1)                                        5.284 1.46e-07 ***
    ## lag(value, 7)                                        3.539 0.000415 ***
    ## lag(value, 14)                                       3.960 7.87e-05 ***
    ## lag(value, 21)                                       1.647 0.099688 .  
    ## lag(value, 28)                                       2.766 0.005750 ** 
    ## lag(value, 35)                                       2.463 0.013895 *  
    ## lag(value, 42)                                       1.613 0.107017    
    ## lag(value, 49)                                       4.096 4.45e-05 ***
    ## month(date, label = TRUE).L                         -1.994 0.046290 *  
    ## month(date, label = TRUE).Q                          0.377 0.706378    
    ## month(date, label = TRUE).C                         -2.069 0.038740 *  
    ## month(date, label = TRUE)^4                         -1.423 0.154823    
    ## month(date, label = TRUE)^5                         -2.372 0.017834 *  
    ## month(date, label = TRUE)^6                         -0.195 0.845540    
    ## month(date, label = TRUE)^7                         -1.691 0.091143 .  
    ## month(date, label = TRUE)^8                         -0.449 0.653656    
    ## month(date, label = TRUE)^9                          0.814 0.415886    
    ## month(date, label = TRUE)^10                         0.979 0.327845    
    ## month(date, label = TRUE)^11                        -1.171 0.241667    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.746 2.29e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.652 0.008094 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.85 on 1413 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2601, Adjusted R-squared:  0.2486 
    ## F-statistic: 22.58 on 22 and 1413 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,451 × 2]> <tibble [28 × 2]> <split [1423|28]>
    ## 2 healthyR      <tibble [1,444 × 2]> <tibble [28 × 2]> <split [1416|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,390 × 2]> <tibble [28 × 2]> <split [1362|28]>
    ## 5 healthyverse  <tibble [1,361 × 2]> <tibble [28 × 2]> <split [1333|28]>
    ## 6 healthyR.ai   <tibble [1,187 × 2]> <tibble [28 × 2]> <split [1159|28]>
    ## 7 TidyDensity   <tibble [1,041 × 2]> <tibble [28 × 2]> <split [1013|28]>
    ## 8 tidyAML       <tibble [657 × 2]>   <tibble [28 × 2]> <split [629|28]> 
    ## 9 RandomWalker  <tibble [91 × 2]>    <tibble [28 × 2]> <split [63|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7446101 | 193.81346 | 0.5987245 | 140.28294 | 0.9100684 | 0.1149146 |
| healthyR.data | 2 | LM | Test | 0.7974606 | 243.06225 | 0.6412205 | 140.64919 | 0.9413523 | 0.0274966 |
| healthyR.data | 3 | EARTH | Test | 0.8110542 | 301.28201 | 0.6521508 | 135.13063 | 0.9293256 | 0.0274966 |
| healthyR.data | 4 | NNAR | Test | 0.8450380 | 125.31814 | 0.6794764 | 162.34993 | 1.1050598 | 0.0070509 |
| healthyR | 1 | ARIMA | Test | 0.6280233 | 144.57153 | 0.7295339 | 155.74842 | 0.7619817 | 0.0504225 |
| healthyR | 2 | LM | Test | 0.6459054 | 114.72319 | 0.7503063 | 190.53685 | 0.7679513 | 0.0174240 |
| healthyR | 3 | EARTH | Test | 0.6760890 | 218.80951 | 0.7853687 | 144.19651 | 0.8140434 | 0.0174240 |
| healthyR | 4 | NNAR | Test | 0.6224161 | 153.62816 | 0.7230203 | 163.91730 | 0.7455035 | 0.0624671 |
| healthyR.ts | 1 | ARIMA | Test | 0.8899980 | 109.49830 | 0.8332487 | 116.00312 | 1.0586116 | 0.0119535 |
| healthyR.ts | 2 | LM | Test | 0.8764477 | 100.28084 | 0.8205624 | 119.74818 | 1.0338743 | 0.0119535 |
| healthyR.ts | 3 | EARTH | Test | 0.8774577 | 101.35232 | 0.8215081 | 119.10082 | 1.0365177 | 0.0119535 |
| healthyR.ts | 4 | NNAR | Test | 0.8916595 | 97.74903 | 0.8348043 | 179.99227 | 1.0629613 | 0.0031193 |
| healthyverse | 1 | ARIMA | Test | 0.4154198 | 142.46635 | 0.7585052 | 80.73483 | 0.5434934 | 0.4962761 |
| healthyverse | 2 | LM | Test | 0.5213006 | 321.76690 | 0.9518304 | 82.21302 | 0.6348335 | 0.0112961 |
| healthyverse | 3 | EARTH | Test | 0.5875867 | 190.66201 | 1.0728607 | 106.87538 | 0.7105405 | 0.0112961 |
| healthyverse | 4 | NNAR | Test | 0.5655184 | 193.82531 | 1.0325666 | 103.32415 | 0.6883216 | 0.1438404 |
| healthyR.ai | 1 | ARIMA | Test | 0.7180456 | 88.80907 | 0.9498911 | 157.00250 | 0.8114214 | 0.1830978 |
| healthyR.ai | 2 | LM | Test | 0.7566909 | 122.97299 | 1.0010144 | 168.58916 | 0.8201748 | 0.0024599 |
| healthyR.ai | 3 | EARTH | Test | 0.8110216 | 129.75420 | 1.0728876 | 148.43705 | 0.9378689 | 0.0024599 |
| healthyR.ai | 4 | NNAR | Test | 0.6945235 | 110.89005 | 0.9187741 | 157.43421 | 0.7747174 | 0.1394307 |
| TidyDensity | 1 | ARIMA | Test | 0.6884577 | 205.00633 | 0.7484576 | 117.04488 | 0.8188792 | 0.1640210 |
| TidyDensity | 2 | LM | Test | 0.7844625 | 281.35516 | 0.8528292 | 117.23184 | 0.9091482 | 0.0331185 |
| TidyDensity | 3 | EARTH | Test | 0.7440006 | 213.41059 | 0.8088411 | 123.77966 | 0.8704785 | 0.0331185 |
| TidyDensity | 4 | NNAR | Test | 0.7902217 | 124.94712 | 0.8590904 | 160.30072 | 0.9694352 | 0.0333572 |
| tidyAML | 1 | ARIMA | Test | 0.6910736 | 81.62666 | 0.9270839 | 92.59519 | 0.8550486 | 0.2049129 |
| tidyAML | 2 | LM | Test | 0.7142708 | 87.04294 | 0.9582033 | 88.57462 | 0.8817106 | 0.0254755 |
| tidyAML | 3 | EARTH | Test | 0.6644557 | 105.24372 | 0.8913756 | 78.73958 | 0.7992490 | 0.0254755 |
| tidyAML | 4 | NNAR | Test | 0.6652689 | 106.50477 | 0.8924665 | 80.30061 | 0.7892093 | 0.0498139 |
| RandomWalker | 1 | ARIMA | Test | 1.2986850 | 108.56289 | 0.5927961 | 176.19901 | 1.3956895 | 0.0964459 |
| RandomWalker | 2 | LM | Test | 1.3581587 | 100.24911 | 0.6199434 | 195.60845 | 1.4731348 | 0.0113455 |
| RandomWalker | 3 | EARTH | Test | 1.2486761 | 99.82168 | 0.5699691 | 115.03190 | 1.5460331 | 0.0113455 |
| RandomWalker | 4 | NNAR | Test | 2.3016744 | 230.84558 | 1.0506194 | 151.20425 | 2.6670325 | 0.0215927 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.745  194. 0.599 140.  0.910 0.115 
    ## 2 healthyR              4 NNAR        Test  0.622  154. 0.723 164.  0.746 0.0625
    ## 3 healthyR.ts           2 LM          Test  0.876  100. 0.821 120.  1.03  0.0120
    ## 4 healthyverse          1 ARIMA       Test  0.415  142. 0.759  80.7 0.543 0.496 
    ## 5 healthyR.ai           4 NNAR        Test  0.695  111. 0.919 157.  0.775 0.139 
    ## 6 TidyDensity           1 ARIMA       Test  0.688  205. 0.748 117.  0.819 0.164 
    ## 7 tidyAML               4 NNAR        Test  0.665  107. 0.892  80.3 0.789 0.0498
    ## 8 RandomWalker          1 ARIMA       Test  1.30   109. 0.593 176.  1.40  0.0964

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1423|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1416|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1362|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1333|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1159|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1013|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [629|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [63|28]>   <mdl_tm_t [1 × 5]>

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
