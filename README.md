Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
09 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 113,304
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

The last day in the data set is 2024-09-07 23:22:01, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -755.77
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 113304        |
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
| r_version     |     79714 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79714 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79714 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9680 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-07 | 2023-02-04 |     1385 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1176709.61 | 1557694.13 | 355 | 14701 | 274361 | 2373465 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10254.38 |   17952.17 |   1 |   317 |   3064 |   11365 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-07 23:22:01 | 2023-02-04 09:36:02 |    68513 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     42 |       60 |

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
    ## -151.24  -34.44   -9.43   26.08  800.52 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.642e+02  8.697e+01
    ## date                                                9.895e-03  4.608e-03
    ## lag(value, 1)                                       1.582e-01  2.636e-02
    ## lag(value, 7)                                       9.968e-02  2.738e-02
    ## lag(value, 14)                                      1.095e-01  2.749e-02
    ## lag(value, 21)                                      2.942e-02  2.763e-02
    ## lag(value, 28)                                      8.630e-02  2.745e-02
    ## lag(value, 35)                                      6.922e-02  2.758e-02
    ## lag(value, 42)                                      4.324e-02  2.764e-02
    ## lag(value, 49)                                      8.969e-02  2.754e-02
    ## month(date, label = TRUE).L                        -1.038e+01  5.690e+00
    ## month(date, label = TRUE).Q                         2.762e+00  5.530e+00
    ## month(date, label = TRUE).C                        -1.096e+01  5.666e+00
    ## month(date, label = TRUE)^4                        -9.177e+00  5.653e+00
    ## month(date, label = TRUE)^5                        -1.631e+01  5.581e+00
    ## month(date, label = TRUE)^6                        -4.360e+00  5.685e+00
    ## month(date, label = TRUE)^7                        -1.008e+01  5.527e+00
    ## month(date, label = TRUE)^8                        -4.161e-01  5.521e+00
    ## month(date, label = TRUE)^9                         7.079e+00  5.487e+00
    ## month(date, label = TRUE)^10                        7.426e+00  5.367e+00
    ## month(date, label = TRUE)^11                       -4.742e+00  5.270e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.156e+01  2.506e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.010e+00  2.594e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.887 0.059317 .  
    ## date                                                 2.148 0.031935 *  
    ## lag(value, 1)                                        6.003 2.51e-09 ***
    ## lag(value, 7)                                        3.641 0.000283 ***
    ## lag(value, 14)                                       3.984 7.14e-05 ***
    ## lag(value, 21)                                       1.065 0.287106    
    ## lag(value, 28)                                       3.144 0.001702 ** 
    ## lag(value, 35)                                       2.510 0.012190 *  
    ## lag(value, 42)                                       1.565 0.117918    
    ## lag(value, 49)                                       3.257 0.001155 ** 
    ## month(date, label = TRUE).L                         -1.825 0.068256 .  
    ## month(date, label = TRUE).Q                          0.499 0.617560    
    ## month(date, label = TRUE).C                         -1.934 0.053334 .  
    ## month(date, label = TRUE)^4                         -1.623 0.104768    
    ## month(date, label = TRUE)^5                         -2.923 0.003526 ** 
    ## month(date, label = TRUE)^6                         -0.767 0.443276    
    ## month(date, label = TRUE)^7                         -1.823 0.068503 .  
    ## month(date, label = TRUE)^8                         -0.075 0.939938    
    ## month(date, label = TRUE)^9                          1.290 0.197183    
    ## month(date, label = TRUE)^10                         1.384 0.166738    
    ## month(date, label = TRUE)^11                        -0.900 0.368322    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.615 4.31e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.317 0.020669 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.48 on 1313 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2603, Adjusted R-squared:  0.2479 
    ## F-statistic:    21 on 22 and 1313 DF,  p-value: < 2.2e-16

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,353 × 2]> <tibble [28 × 2]> <split [1325|28]>
    ## 2 healthyR      <tibble [1,345 × 2]> <tibble [28 × 2]> <split [1317|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,291 × 2]> <tibble [28 × 2]> <split [1263|28]>
    ## 5 healthyverse  <tibble [1,262 × 2]> <tibble [28 × 2]> <split [1234|28]>
    ## 6 healthyR.ai   <tibble [1,088 × 2]> <tibble [28 × 2]> <split [1060|28]>
    ## 7 TidyDensity   <tibble [942 × 2]>   <tibble [28 × 2]> <split [914|28]> 
    ## 8 tidyAML       <tibble [558 × 2]>   <tibble [28 × 2]> <split [530|28]>

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

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.5766965 | 101.12621 | 0.7036149 | 156.67865 | 0.8092954 | 0.0089945 |
| healthyR.data |         2 | LM          | Test  | 0.8380153 | 317.79916 | 1.0224444 | 170.22232 | 0.9695691 | 0.0691261 |
| healthyR.data |         3 | EARTH       | Test  | 0.5371032 | 130.87325 | 0.6553080 | 118.63943 | 0.8010740 | 0.0691261 |
| healthyR.data |         4 | NNAR        | Test  | 0.5722057 | 137.29403 | 0.6981358 | 146.00564 | 0.8017602 | 0.0025035 |
| healthyR      |         1 | ARIMA       | Test  | 0.7778318 |  91.61813 | 0.7570035 | 149.29918 | 0.9813242 | 0.0425858 |
| healthyR      |         2 | LM          | Test  | 0.8944357 | 117.09987 | 0.8704851 | 191.39122 | 1.0810246 | 0.0894302 |
| healthyR      |         3 | EARTH       | Test  | 0.6916101 |  71.67310 | 0.6730906 |  97.99548 | 0.9601569 | 0.0894302 |
| healthyR      |         4 | NNAR        | Test  | 0.8539775 | 110.47993 | 0.8311102 | 174.91581 | 1.0382124 | 0.0808920 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.1981901 | 286.34172 | 1.2381145 | 174.63187 | 1.4202281 | 0.0538164 |
| healthyR.ts   |         2 | LM          | Test  | 0.6647961 | 130.26276 | 0.6869475 |  92.47174 | 0.9372231 | 0.0330531 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7818810 | 104.16253 | 0.8079337 | 135.69511 | 1.0419139 | 0.0330531 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8315837 | 148.29255 | 0.8592925 | 181.32220 | 1.0375067 | 0.1669613 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5874462 | 302.71149 | 0.7216892 | 103.28140 | 0.7079736 | 0.1017698 |
| healthyverse  |         2 | LM          | Test  | 0.6439336 | 446.75527 | 0.7910851 | 100.73772 | 0.7730888 | 0.0021385 |
| healthyverse  |         3 | EARTH       | Test  | 0.5906169 | 312.44351 | 0.7255844 | 105.20876 | 0.7256221 | 0.0021385 |
| healthyverse  |         4 | NNAR        | Test  | 0.6030841 | 250.86429 | 0.7409007 | 117.94675 | 0.7502535 | 0.0000024 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7504945 | 115.29261 | 0.6824342 | 166.50373 | 0.9816621 | 0.0065321 |
| healthyR.ai   |         2 | LM          | Test  | 0.7636171 | 182.84666 | 0.6943667 | 134.80730 | 1.0221477 | 0.0179501 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.3340272 | 749.50715 | 1.2130478 | 148.58296 | 1.6563767 | 0.0179501 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7337103 | 122.93372 | 0.6671721 | 140.92227 | 0.9854957 | 0.0137629 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5006204 | 341.95238 | 0.7576629 |  87.15416 | 0.6156471 | 0.1082600 |
| TidyDensity   |         2 | LM          | Test  | 0.5481456 | 406.87576 | 0.8295898 |  89.96147 | 0.6512768 | 0.0123593 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8092583 | 186.70157 | 1.2247703 | 184.56286 | 0.9990822 | 0.0123593 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5855786 | 107.02225 | 0.8862427 | 132.70479 | 0.7783525 | 0.1028101 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5927161 | 174.02480 | 0.8877506 | 126.93661 | 0.6743223 | 0.0489339 |
| tidyAML       |         2 | LM          | Test  | 0.5932481 | 170.15372 | 0.8885474 | 113.58820 | 0.6858582 | 0.0009100 |
| tidyAML       |         3 | EARTH       | Test  | 1.4835789 | 636.80491 | 2.2220553 | 119.30519 | 1.7317106 | 0.0009100 |
| tidyAML       |         4 | NNAR        | Test  | 0.6018343 | 242.23225 | 0.9014075 | 100.77361 | 0.7292815 | 0.0438944 |

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
    ##   # A tibble: 7 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         3 EARTH       Test  0.537 131.  0.655 119.  0.801 0.0691 
    ## 2 healthyR             3 EARTH       Test  0.692  71.7 0.673  98.0 0.960 0.0894 
    ## 3 healthyR.ts          2 LM          Test  0.665 130.  0.687  92.5 0.937 0.0331 
    ## 4 healthyverse         1 ARIMA       Test  0.587 303.  0.722 103.  0.708 0.102  
    ## 5 healthyR.ai          1 ARIMA       Test  0.750 115.  0.682 167.  0.982 0.00653
    ## 6 TidyDensity          1 ARIMA       Test  0.501 342.  0.758  87.2 0.616 0.108  
    ## 7 tidyAML              1 ARIMA       Test  0.593 174.  0.888 127.  0.674 0.0489

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1325|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1317|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1263|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1234|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1060|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [914|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [530|28]>  <mdl_tm_t [1 × 5]>

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
