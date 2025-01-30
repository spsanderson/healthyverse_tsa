Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
30 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 128,846
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

The last day in the data set is 2025-01-28 23:45:16, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.257978^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 128846        |
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
| r_version     |     91879 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91879 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91879 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10909 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-28 | 2023-04-25 | 1528 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1142632.94 | 1531918.91 | 355 | 14701 | 260384 | 2367888 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10460.56 | 18482.47 | 1 | 333 | 3091 | 11931 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-28 23:45:16 | 2023-04-25 11:27:22 | 78059 |

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
    ## -155.98  -35.00   -9.36   27.15  805.13 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -184.75781   77.72576
    ## date                                                  0.01108    0.00412
    ## lag(value, 1)                                         0.11909    0.02523
    ## lag(value, 7)                                         0.09654    0.02622
    ## lag(value, 14)                                        0.11155    0.02626
    ## lag(value, 21)                                        0.05098    0.02637
    ## lag(value, 28)                                        0.06760    0.02629
    ## lag(value, 35)                                        0.06992    0.02640
    ## lag(value, 42)                                        0.04905    0.02650
    ## lag(value, 49)                                        0.09370    0.02633
    ## month(date, label = TRUE).L                         -11.17193    5.30123
    ## month(date, label = TRUE).Q                           2.12389    5.17578
    ## month(date, label = TRUE).C                         -11.36651    5.25823
    ## month(date, label = TRUE)^4                          -7.45470    5.28677
    ## month(date, label = TRUE)^5                         -12.91211    5.29248
    ## month(date, label = TRUE)^6                          -0.74778    5.37759
    ## month(date, label = TRUE)^7                          -9.10134    5.28360
    ## month(date, label = TRUE)^8                          -2.13485    5.28109
    ## month(date, label = TRUE)^9                           4.20700    5.27291
    ## month(date, label = TRUE)^10                          5.06925    5.27249
    ## month(date, label = TRUE)^11                         -6.14145    5.28592
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.58431    2.41476
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    7.22841    2.53830
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.377 0.017580 *  
    ## date                                                 2.690 0.007222 ** 
    ## lag(value, 1)                                        4.720 2.59e-06 ***
    ## lag(value, 7)                                        3.682 0.000240 ***
    ## lag(value, 14)                                       4.248 2.30e-05 ***
    ## lag(value, 21)                                       1.933 0.053381 .  
    ## lag(value, 28)                                       2.571 0.010237 *  
    ## lag(value, 35)                                       2.648 0.008176 ** 
    ## lag(value, 42)                                       1.851 0.064397 .  
    ## lag(value, 49)                                       3.558 0.000386 ***
    ## month(date, label = TRUE).L                         -2.107 0.035252 *  
    ## month(date, label = TRUE).Q                          0.410 0.681608    
    ## month(date, label = TRUE).C                         -2.162 0.030807 *  
    ## month(date, label = TRUE)^4                         -1.410 0.158733    
    ## month(date, label = TRUE)^5                         -2.440 0.014818 *  
    ## month(date, label = TRUE)^6                         -0.139 0.889427    
    ## month(date, label = TRUE)^7                         -1.723 0.085180 .  
    ## month(date, label = TRUE)^8                         -0.404 0.686092    
    ## month(date, label = TRUE)^9                          0.798 0.425087    
    ## month(date, label = TRUE)^10                         0.961 0.336484    
    ## month(date, label = TRUE)^11                        -1.162 0.245486    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.797 1.77e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.848 0.004465 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58 on 1456 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2609, Adjusted R-squared:  0.2498 
    ## F-statistic: 23.37 on 22 and 1456 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,493 × 2]> <tibble [28 × 2]> <split [1465|28]>
    ## 2 healthyR      <tibble [1,486 × 2]> <tibble [28 × 2]> <split [1458|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,432 × 2]> <tibble [28 × 2]> <split [1404|28]>
    ## 5 healthyverse  <tibble [1,403 × 2]> <tibble [28 × 2]> <split [1375|28]>
    ## 6 healthyR.ai   <tibble [1,229 × 2]> <tibble [28 × 2]> <split [1201|28]>
    ## 7 TidyDensity   <tibble [1,083 × 2]> <tibble [28 × 2]> <split [1055|28]>
    ## 8 tidyAML       <tibble [698 × 2]>   <tibble [28 × 2]> <split [670|28]> 
    ## 9 RandomWalker  <tibble [132 × 2]>   <tibble [28 × 2]> <split [104|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.1078661 | 259.74121 | 0.8256129 | 164.46228 | 1.2552381 | 0.0392643 |
| healthyR.data | 2 | LM | Test | 1.0752982 | 230.32840 | 0.8013423 | 162.72479 | 1.2254833 | 0.0109730 |
| healthyR.data | 3 | EARTH | Test | 1.2430136 | 335.74095 | 0.9263286 | 161.36569 | 1.3821806 | 0.0109730 |
| healthyR.data | 4 | NNAR | Test | 0.8908768 | 112.09882 | 0.6639063 | 171.41974 | 1.0717617 | 0.0031176 |
| healthyR | 1 | ARIMA | Test | 0.7705185 | 107.12244 | 0.7063941 | 170.26117 | 0.9760408 | 0.0000457 |
| healthyR | 2 | LM | Test | 0.7872646 | 104.15657 | 0.7217466 | 187.62375 | 0.9764623 | 0.0462996 |
| healthyR | 3 | EARTH | Test | 0.8009081 | 120.15397 | 0.7342547 | 162.97770 | 0.9961033 | 0.0462996 |
| healthyR | 4 | NNAR | Test | 0.7867454 | 132.80781 | 0.7212707 | 168.03156 | 0.9890188 | 0.0046786 |
| healthyR.ts | 1 | ARIMA | Test | 0.6965665 | 158.91249 | 0.6024032 | 123.31564 | 0.9860076 | 0.0520291 |
| healthyR.ts | 2 | LM | Test | 0.7321028 | 241.15453 | 0.6331355 | 118.91228 | 0.9783519 | 0.0520291 |
| healthyR.ts | 3 | EARTH | Test | 0.7355993 | 246.81533 | 0.6361594 | 118.84495 | 0.9790927 | 0.0520291 |
| healthyR.ts | 4 | NNAR | Test | 0.7781148 | 113.62859 | 0.6729276 | 186.37399 | 1.0826936 | 0.0009348 |
| healthyverse | 1 | ARIMA | Test | 0.7197361 | 263.22892 | 0.7203071 | 108.24858 | 0.8593898 | 0.0016125 |
| healthyverse | 2 | LM | Test | 0.7311335 | 288.45548 | 0.7317136 | 103.68099 | 0.8794305 | 0.1036050 |
| healthyverse | 3 | EARTH | Test | 0.7752384 | 347.31760 | 0.7758535 | 101.56199 | 0.9426324 | 0.1036050 |
| healthyverse | 4 | NNAR | Test | 0.7284128 | 205.14468 | 0.7289908 | 123.63002 | 0.8626819 | 0.0031688 |
| healthyR.ai | 1 | ARIMA | Test | 0.6939963 | 136.48878 | 0.6969535 | 172.09325 | 0.8999893 | 0.0030353 |
| healthyR.ai | 2 | LM | Test | 0.7045533 | 142.92623 | 0.7075555 | 156.69390 | 0.9105283 | 0.0294861 |
| healthyR.ai | 3 | EARTH | Test | 0.7432584 | 215.38240 | 0.7464255 | 130.06111 | 0.9975936 | 0.0294861 |
| healthyR.ai | 4 | NNAR | Test | 0.7314963 | 145.61622 | 0.7346133 | 168.78768 | 0.9518507 | 0.0291119 |
| TidyDensity | 1 | ARIMA | Test | 0.6916241 | 146.41923 | 0.7170674 | 112.36109 | 0.8799619 | 0.0286964 |
| TidyDensity | 2 | LM | Test | 0.7032125 | 203.81703 | 0.7290822 | 98.28447 | 0.8934519 | 0.0427981 |
| TidyDensity | 3 | EARTH | Test | 0.6459377 | 124.99496 | 0.6697003 | 103.25198 | 0.8478499 | 0.0427981 |
| TidyDensity | 4 | NNAR | Test | 0.7381082 | 106.42713 | 0.7652616 | 143.89906 | 0.9057380 | 0.0025081 |
| tidyAML | 1 | ARIMA | Test | 0.8786266 | 308.30346 | 0.7831459 | 98.51787 | 1.0837217 | 0.0258040 |
| tidyAML | 2 | LM | Test | 0.8316378 | 210.06697 | 0.7412633 | 107.06429 | 1.0282202 | 0.0100067 |
| tidyAML | 3 | EARTH | Test | 0.9578397 | 348.72985 | 0.8537509 | 101.18194 | 1.1604676 | 0.0100067 |
| tidyAML | 4 | NNAR | Test | 0.8567301 | 225.43285 | 0.7636289 | 103.91919 | 1.0633924 | 0.0177797 |
| RandomWalker | 1 | ARIMA | Test | 1.5198903 | 176.43173 | 0.7574761 | 145.89795 | 1.8166727 | 0.0246080 |
| RandomWalker | 2 | LM | Test | 1.2586604 | 98.93968 | 0.6272855 | 167.24930 | 1.4581256 | 0.0104466 |
| RandomWalker | 3 | EARTH | Test | 1.2617004 | 100.89557 | 0.6288006 | 165.33083 | 1.4628825 | NA |
| RandomWalker | 4 | NNAR | Test | 3.7963197 | 653.20362 | 1.8919928 | 157.86183 | 4.6904094 | 0.0155841 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.891 112.  0.664  171. 1.07  3.12e-3
    ## 2 healthyR             1 ARIMA       Test  0.771 107.  0.706  170. 0.976 4.57e-5
    ## 3 healthyR.ts          2 LM          Test  0.732 241.  0.633  119. 0.978 5.20e-2
    ## 4 healthyverse         1 ARIMA       Test  0.720 263.  0.720  108. 0.859 1.61e-3
    ## 5 healthyR.ai          1 ARIMA       Test  0.694 136.  0.697  172. 0.900 3.04e-3
    ## 6 TidyDensity          3 EARTH       Test  0.646 125.  0.670  103. 0.848 4.28e-2
    ## 7 tidyAML              2 LM          Test  0.832 210.  0.741  107. 1.03  1.00e-2
    ## 8 RandomWalker         2 LM          Test  1.26   98.9 0.627  167. 1.46  1.04e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1465|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1458|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1404|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1375|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1201|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1055|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [670|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [104|28]>  <mdl_tm_t [1 × 5]>

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
