Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 127,948
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

The last day in the data set is 2025-01-20 23:13:07, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3995.62
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 127948        |
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
| r_version     |     91117 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     91117 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     91117 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10863 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-20 | 2023-04-21 | 1520 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1144115.30 | 1533352.81 | 355 | 14701 | 260378 | 2367916 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10383.44 | 18119.66 | 1 | 334 | 3098 | 11931 | 162589 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-20 23:13:07 | 2023-04-21 15:35:57 | 77485 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     24 |       60 |

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
    ## -154.95  -35.08   -9.72   27.33  805.09 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.920e+02  7.811e+01
    ## date                                                1.148e-02  4.139e-03
    ## lag(value, 1)                                       1.232e-01  2.531e-02
    ## lag(value, 7)                                       9.315e-02  2.625e-02
    ## lag(value, 14)                                      1.090e-01  2.626e-02
    ## lag(value, 21)                                      4.888e-02  2.647e-02
    ## lag(value, 28)                                      6.457e-02  2.631e-02
    ## lag(value, 35)                                      7.238e-02  2.644e-02
    ## lag(value, 42)                                      4.900e-02  2.650e-02
    ## lag(value, 49)                                      9.441e-02  2.635e-02
    ## month(date, label = TRUE).L                        -1.150e+01  5.335e+00
    ## month(date, label = TRUE).Q                         2.385e+00  5.198e+00
    ## month(date, label = TRUE).C                        -1.153e+01  5.275e+00
    ## month(date, label = TRUE)^4                        -7.344e+00  5.298e+00
    ## month(date, label = TRUE)^5                        -1.305e+01  5.292e+00
    ## month(date, label = TRUE)^6                        -7.916e-01  5.375e+00
    ## month(date, label = TRUE)^7                        -9.128e+00  5.277e+00
    ## month(date, label = TRUE)^8                        -2.174e+00  5.274e+00
    ## month(date, label = TRUE)^9                         4.155e+00  5.266e+00
    ## month(date, label = TRUE)^10                        5.136e+00  5.265e+00
    ## month(date, label = TRUE)^11                       -6.202e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.171e+01  2.419e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.202e+00  2.537e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.458 0.014082 *  
    ## date                                                 2.773 0.005633 ** 
    ## lag(value, 1)                                        4.870 1.24e-06 ***
    ## lag(value, 7)                                        3.549 0.000399 ***
    ## lag(value, 14)                                       4.150 3.52e-05 ***
    ## lag(value, 21)                                       1.846 0.065051 .  
    ## lag(value, 28)                                       2.454 0.014227 *  
    ## lag(value, 35)                                       2.738 0.006266 ** 
    ## lag(value, 42)                                       1.849 0.064631 .  
    ## lag(value, 49)                                       3.582 0.000352 ***
    ## month(date, label = TRUE).L                         -2.156 0.031217 *  
    ## month(date, label = TRUE).Q                          0.459 0.646374    
    ## month(date, label = TRUE).C                         -2.186 0.028965 *  
    ## month(date, label = TRUE)^4                         -1.386 0.165898    
    ## month(date, label = TRUE)^5                         -2.465 0.013802 *  
    ## month(date, label = TRUE)^6                         -0.147 0.882926    
    ## month(date, label = TRUE)^7                         -1.730 0.083889 .  
    ## month(date, label = TRUE)^8                         -0.412 0.680276    
    ## month(date, label = TRUE)^9                          0.789 0.430193    
    ## month(date, label = TRUE)^10                         0.975 0.329526    
    ## month(date, label = TRUE)^11                        -1.175 0.240210    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.838 1.45e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.838 0.004596 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.91 on 1448 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2596, Adjusted R-squared:  0.2484 
    ## F-statistic: 23.08 on 22 and 1448 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,485 × 2]> <tibble [28 × 2]> <split [1457|28]>
    ## 2 healthyR      <tibble [1,478 × 2]> <tibble [28 × 2]> <split [1450|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,424 × 2]> <tibble [28 × 2]> <split [1396|28]>
    ## 5 healthyverse  <tibble [1,395 × 2]> <tibble [28 × 2]> <split [1367|28]>
    ## 6 healthyR.ai   <tibble [1,221 × 2]> <tibble [28 × 2]> <split [1193|28]>
    ## 7 TidyDensity   <tibble [1,075 × 2]> <tibble [28 × 2]> <split [1047|28]>
    ## 8 tidyAML       <tibble [691 × 2]>   <tibble [28 × 2]> <split [663|28]> 
    ## 9 RandomWalker  <tibble [125 × 2]>   <tibble [28 × 2]> <split [97|28]>

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
| healthyR.data | 1 | ARIMA | Test | 1.0085990 | 235.7567 | 0.7487645 | 169.2138 | 1.1821701 | 0.0052569 |
| healthyR.data | 2 | LM | Test | 1.0638616 | 293.8384 | 0.7897904 | 168.4617 | 1.2117073 | 0.1359864 |
| healthyR.data | 3 | EARTH | Test | 1.1988131 | 418.9268 | 0.8899759 | 164.0915 | 1.3374377 | 0.1359864 |
| healthyR.data | 4 | NNAR | Test | 0.8805316 | 106.5469 | 0.6536897 | 161.3980 | 1.1103862 | 0.0017481 |
| healthyR | 1 | ARIMA | Test | 0.7430474 | 118.8402 | 0.6321848 | 165.4247 | 0.9149688 | 0.0019292 |
| healthyR | 2 | LM | Test | 0.7375258 | 100.9086 | 0.6274870 | 183.6967 | 0.9188335 | 0.0022631 |
| healthyR | 3 | EARTH | Test | 0.7405516 | 105.7813 | 0.6300613 | 164.9408 | 0.9266921 | 0.0022631 |
| healthyR | 4 | NNAR | Test | 0.7662173 | 135.0410 | 0.6518977 | 163.0772 | 0.9470991 | 0.0031154 |
| healthyR.ts | 1 | ARIMA | Test | 0.8101806 | 246.1561 | 0.6018347 | 123.7517 | 1.0675590 | 0.0002541 |
| healthyR.ts | 2 | LM | Test | 0.8317417 | 273.5318 | 0.6178512 | 124.6493 | 1.0750689 | 0.0007720 |
| healthyR.ts | 3 | EARTH | Test | 0.8373953 | 280.6644 | 0.6220509 | 124.8358 | 1.0774266 | 0.0007720 |
| healthyR.ts | 4 | NNAR | Test | 0.8177613 | 103.5978 | 0.6074660 | 172.3283 | 1.1088286 | 0.0001134 |
| healthyverse | 1 | ARIMA | Test | 0.7028161 | 228.3412 | 0.6536118 | 110.6587 | 0.8389507 | 0.0408674 |
| healthyverse | 2 | LM | Test | 0.7391568 | 293.9181 | 0.6874083 | 106.1178 | 0.8745535 | 0.0003061 |
| healthyverse | 3 | EARTH | Test | 0.7892521 | 358.4051 | 0.7339965 | 104.7939 | 0.9342447 | 0.0003061 |
| healthyverse | 4 | NNAR | Test | 0.7026996 | 169.6342 | 0.6535035 | 123.4346 | 0.8507531 | 0.0583546 |
| healthyR.ai | 1 | ARIMA | Test | 0.6801313 | 108.6795 | 0.6721165 | 185.1240 | 0.8136972 | 0.0093520 |
| healthyR.ai | 2 | LM | Test | 0.6713032 | 134.9701 | 0.6633925 | 152.9408 | 0.8288604 | 0.0209047 |
| healthyR.ai | 3 | EARTH | Test | 0.6791788 | 174.2705 | 0.6711752 | 133.4777 | 0.8617626 | 0.0209047 |
| healthyR.ai | 4 | NNAR | Test | 0.6861339 | 136.7083 | 0.6780484 | 158.0896 | 0.8444773 | 0.0004653 |
| TidyDensity | 1 | ARIMA | Test | 0.7442679 | 144.3795 | 0.6772039 | 118.3445 | 0.8909346 | 0.0208654 |
| TidyDensity | 2 | LM | Test | 0.7826473 | 170.1017 | 0.7121251 | 110.4198 | 0.9574319 | 0.0017368 |
| TidyDensity | 3 | EARTH | Test | 0.7363964 | 132.3078 | 0.6700417 | 116.0214 | 0.8954271 | 0.0017368 |
| TidyDensity | 4 | NNAR | Test | 0.7594086 | 117.1667 | 0.6909803 | 148.6597 | 0.8997607 | 0.0055115 |
| tidyAML | 1 | ARIMA | Test | 0.9103139 | 213.3872 | 0.7449782 | 110.2330 | 1.0468878 | 0.0716984 |
| tidyAML | 2 | LM | Test | 0.9645389 | 208.3222 | 0.7893546 | 117.8043 | 1.1024856 | 0.0501688 |
| tidyAML | 3 | EARTH | Test | 0.9282068 | 294.2570 | 0.7596213 | 102.8362 | 1.0492419 | 0.0501688 |
| tidyAML | 4 | NNAR | Test | 0.9436508 | 231.3589 | 0.7722603 | 112.7008 | 1.0921317 | 0.0068783 |
| RandomWalker | 1 | ARIMA | Test | 1.1538351 | 218.2752 | 0.5631386 | 130.2005 | 1.3855411 | 0.1426238 |
| RandomWalker | 2 | LM | Test | 1.2292467 | 112.4891 | 0.5999439 | 154.7877 | 1.4515586 | 0.0009678 |
| RandomWalker | 3 | EARTH | Test | 1.2164433 | 102.3162 | 0.5936951 | 158.2276 | 1.4361816 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3183476 | 160.6145 | 0.6434303 | 148.7648 | 1.5898891 | 0.0020750 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.881  107. 0.654  161. 1.11  1.75e-3
    ## 2 healthyR             1 ARIMA       Test  0.743  119. 0.632  165. 0.915 1.93e-3
    ## 3 healthyR.ts          1 ARIMA       Test  0.810  246. 0.602  124. 1.07  2.54e-4
    ## 4 healthyverse         1 ARIMA       Test  0.703  228. 0.654  111. 0.839 4.09e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.680  109. 0.672  185. 0.814 9.35e-3
    ## 6 TidyDensity          1 ARIMA       Test  0.744  144. 0.677  118. 0.891 2.09e-2
    ## 7 tidyAML              1 ARIMA       Test  0.910  213. 0.745  110. 1.05  7.17e-2
    ## 8 RandomWalker         1 ARIMA       Test  1.15   218. 0.563  130. 1.39  1.43e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1457|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1450|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1396|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1367|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1193|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1047|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [663|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [97|28]>   <mdl_tm_t [1 × 5]>

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
