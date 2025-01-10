Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,691
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

The last day in the data set is 2025-01-08 23:31:58, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3707.94
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126691        |
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
| r_version     |     90134 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     90134 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     90134 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10770 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-08 | 2023-04-19 | 1508 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144038.62 | 1533337.96 | 355 | 14701 | 258984 | 2367916 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10342.76 | 18021.94 | 1 | 317 | 3091 | 11830 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-08 23:31:58 | 2023-04-19 09:47:08 | 76664 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 32S |       60 |

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
    ## -155.02  -35.20   -9.91   27.04  803.72 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.961e+02  7.906e+01
    ## date                                                1.167e-02  4.189e-03
    ## lag(value, 1)                                       1.267e-01  2.540e-02
    ## lag(value, 7)                                       9.208e-02  2.637e-02
    ## lag(value, 14)                                      1.055e-01  2.644e-02
    ## lag(value, 21)                                      5.050e-02  2.655e-02
    ## lag(value, 28)                                      6.772e-02  2.639e-02
    ## lag(value, 35)                                      7.303e-02  2.652e-02
    ## lag(value, 42)                                      4.922e-02  2.658e-02
    ## lag(value, 49)                                      9.486e-02  2.642e-02
    ## month(date, label = TRUE).L                        -1.206e+01  5.412e+00
    ## month(date, label = TRUE).Q                         2.848e+00  5.252e+00
    ## month(date, label = TRUE).C                        -1.202e+01  5.320e+00
    ## month(date, label = TRUE)^4                        -7.019e+00  5.330e+00
    ## month(date, label = TRUE)^5                        -1.328e+01  5.308e+00
    ## month(date, label = TRUE)^6                        -6.649e-01  5.383e+00
    ## month(date, label = TRUE)^7                        -9.214e+00  5.280e+00
    ## month(date, label = TRUE)^8                        -2.125e+00  5.276e+00
    ## month(date, label = TRUE)^9                         4.138e+00  5.267e+00
    ## month(date, label = TRUE)^10                        5.178e+00  5.267e+00
    ## month(date, label = TRUE)^11                       -6.246e+00  5.280e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.153e+01  2.429e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.324e+00  2.545e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.480 0.013242 *  
    ## date                                                 2.786 0.005407 ** 
    ## lag(value, 1)                                        4.987 6.87e-07 ***
    ## lag(value, 7)                                        3.492 0.000494 ***
    ## lag(value, 14)                                       3.991 6.92e-05 ***
    ## lag(value, 21)                                       1.902 0.057346 .  
    ## lag(value, 28)                                       2.566 0.010387 *  
    ## lag(value, 35)                                       2.754 0.005966 ** 
    ## lag(value, 42)                                       1.852 0.064295 .  
    ## lag(value, 49)                                       3.590 0.000342 ***
    ## month(date, label = TRUE).L                         -2.229 0.025973 *  
    ## month(date, label = TRUE).Q                          0.542 0.587641    
    ## month(date, label = TRUE).C                         -2.260 0.023973 *  
    ## month(date, label = TRUE)^4                         -1.317 0.188143    
    ## month(date, label = TRUE)^5                         -2.502 0.012473 *  
    ## month(date, label = TRUE)^6                         -0.124 0.901722    
    ## month(date, label = TRUE)^7                         -1.745 0.081213 .  
    ## month(date, label = TRUE)^8                         -0.403 0.687262    
    ## month(date, label = TRUE)^9                          0.786 0.432235    
    ## month(date, label = TRUE)^10                         0.983 0.325722    
    ## month(date, label = TRUE)^11                        -1.183 0.237054    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.749 2.25e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.878 0.004060 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.93 on 1436 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2606, Adjusted R-squared:  0.2493 
    ## F-statistic: 23.01 on 22 and 1436 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,473 × 2]> <tibble [28 × 2]> <split [1445|28]>
    ## 2 healthyR      <tibble [1,466 × 2]> <tibble [28 × 2]> <split [1438|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,412 × 2]> <tibble [28 × 2]> <split [1384|28]>
    ## 5 healthyverse  <tibble [1,383 × 2]> <tibble [28 × 2]> <split [1355|28]>
    ## 6 healthyR.ai   <tibble [1,209 × 2]> <tibble [28 × 2]> <split [1181|28]>
    ## 7 TidyDensity   <tibble [1,063 × 2]> <tibble [28 × 2]> <split [1035|28]>
    ## 8 tidyAML       <tibble [679 × 2]>   <tibble [28 × 2]> <split [651|28]> 
    ## 9 RandomWalker  <tibble [113 × 2]>   <tibble [28 × 2]> <split [85|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8259535 | 301.37230 | 0.7010292 | 152.14569 | 0.9419527 | 0.0434197 |
| healthyR.data | 2 | LM | Test | 0.8207652 | 293.94133 | 0.6966255 | 149.47986 | 0.9573740 | 0.0501195 |
| healthyR.data | 3 | EARTH | Test | 0.9809373 | 492.71662 | 0.8325718 | 146.05042 | 1.0916779 | 0.0501195 |
| healthyR.data | 4 | NNAR | Test | 0.7602974 | 132.18887 | 0.6453034 | 166.24029 | 0.9846746 | 0.0208645 |
| healthyR | 1 | ARIMA | Test | 0.7016135 | 138.62352 | 0.6882017 | 176.88997 | 0.8168664 | 0.0880368 |
| healthyR | 2 | LM | Test | 0.7069790 | 101.09503 | 0.6934646 | 183.17095 | 0.8538119 | 0.0058074 |
| healthyR | 3 | EARTH | Test | 0.7078648 | 100.44293 | 0.6943335 | 161.35560 | 0.8688363 | 0.0058074 |
| healthyR | 4 | NNAR | Test | 0.6721831 | 100.62885 | 0.6593339 | 150.37655 | 0.8340881 | 0.0761370 |
| healthyR.ts | 1 | ARIMA | Test | 0.8490844 | 117.22230 | 0.6812478 | 123.43799 | 1.0845725 | 0.0045812 |
| healthyR.ts | 2 | LM | Test | 0.8469416 | 151.58702 | 0.6795286 | 114.24603 | 1.0626952 | 0.0026700 |
| healthyR.ts | 3 | EARTH | Test | 0.8478845 | 154.44316 | 0.6802851 | 113.84081 | 1.0621621 | 0.0026700 |
| healthyR.ts | 4 | NNAR | Test | 0.8795370 | 91.27488 | 0.7056810 | 168.62584 | 1.1176013 | 0.0507774 |
| healthyverse | 1 | ARIMA | Test | 0.5696149 | 246.25040 | 0.8113617 | 103.87500 | 0.6897574 | 0.0433980 |
| healthyverse | 2 | LM | Test | 0.6144485 | 321.41283 | 0.8752229 | 101.14856 | 0.7558796 | 0.0222753 |
| healthyverse | 3 | EARTH | Test | 3.8522710 | 1819.53680 | 5.4871902 | 157.03842 | 4.2179907 | 0.0222753 |
| healthyverse | 4 | NNAR | Test | 0.5322201 | 177.27763 | 0.7580964 | 110.54570 | 0.6482026 | 0.1051040 |
| healthyR.ai | 1 | ARIMA | Test | 0.6540511 | 95.97401 | 0.7339378 | 168.41527 | 0.7707784 | 0.1370514 |
| healthyR.ai | 2 | LM | Test | 0.6865670 | 113.95240 | 0.7704252 | 150.00095 | 0.8230595 | 0.0105735 |
| healthyR.ai | 3 | EARTH | Test | 0.6787799 | 140.27635 | 0.7616869 | 125.13859 | 0.8678323 | 0.0105735 |
| healthyR.ai | 4 | NNAR | Test | 0.6482957 | 103.92408 | 0.7274794 | 146.26774 | 0.7830709 | 0.0941856 |
| TidyDensity | 1 | ARIMA | Test | 0.7900375 | 152.23031 | 0.7843386 | 121.72616 | 0.9178147 | 0.0372571 |
| TidyDensity | 2 | LM | Test | 0.8170993 | 192.69733 | 0.8112052 | 112.43353 | 0.9422586 | 0.0161024 |
| TidyDensity | 3 | EARTH | Test | 0.8111632 | 139.70358 | 0.8053119 | 126.23930 | 0.9448263 | 0.0161024 |
| TidyDensity | 4 | NNAR | Test | 0.7945169 | 95.44849 | 0.7887856 | 147.57122 | 0.9695695 | 0.0955557 |
| tidyAML | 1 | ARIMA | Test | 0.8328065 | 270.92417 | 0.7998026 | 104.86156 | 0.9407583 | 0.0914275 |
| tidyAML | 2 | LM | Test | 0.8780835 | 218.24069 | 0.8432853 | 115.06682 | 1.0275662 | 0.0071977 |
| tidyAML | 3 | EARTH | Test | 0.8780835 | 303.68659 | 0.8432853 | 106.81132 | 0.9743048 | 0.0071977 |
| tidyAML | 4 | NNAR | Test | 0.8402053 | 250.60339 | 0.8069082 | 107.12188 | 0.9716468 | 0.0397313 |
| RandomWalker | 1 | ARIMA | Test | 0.7851967 | 110.99711 | 0.3743610 | 85.98623 | 1.0516909 | 0.4230575 |
| RandomWalker | 2 | LM | Test | 1.2294679 | 100.78633 | 0.5861777 | 160.62777 | 1.3953136 | 0.0138499 |
| RandomWalker | 3 | EARTH | Test | 1.2287360 | 98.92861 | 0.5858288 | 162.46660 | 1.3924010 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2407016 | 118.36461 | 0.5915337 | 132.45375 | 1.5158360 | 0.0225395 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.826 301.  0.701 152.  0.942 0.0434 
    ## 2 healthyR             1 ARIMA       Test  0.702 139.  0.688 177.  0.817 0.0880 
    ## 3 healthyR.ts          3 EARTH       Test  0.848 154.  0.680 114.  1.06  0.00267
    ## 4 healthyverse         4 NNAR        Test  0.532 177.  0.758 111.  0.648 0.105  
    ## 5 healthyR.ai          1 ARIMA       Test  0.654  96.0 0.734 168.  0.771 0.137  
    ## 6 TidyDensity          1 ARIMA       Test  0.790 152.  0.784 122.  0.918 0.0373 
    ## 7 tidyAML              1 ARIMA       Test  0.833 271.  0.800 105.  0.941 0.0914 
    ## 8 RandomWalker         1 ARIMA       Test  0.785 111.  0.374  86.0 1.05  0.423

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1445|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1438|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1384|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1355|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1181|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1035|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [651|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [85|28]>   <mdl_tm_t [1 × 5]>

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
