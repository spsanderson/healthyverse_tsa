Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 132,400
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

The last day in the data set is 2025-02-24 23:55:04, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -4836.32
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 132400        |
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
| r_version     |     94954 |          0.28 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     94954 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     94954 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11184 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-24 | 2023-05-10 | 1555 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1139124.53 | 1528258.68 | 355 | 14701 | 260657 | 2367818 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10394.79 | 18393.82 | 1 | 291 | 3064 | 11905 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-24 23:55:04 | 2023-05-10 12:04:28 | 80314 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     50 |       60 |

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
    ## -154.26  -34.83  -10.51   26.90  809.98 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.192e+02  7.652e+01
    ## date                                                1.295e-02  4.058e-03
    ## lag(value, 1)                                       1.125e-01  2.511e-02
    ## lag(value, 7)                                       8.762e-02  2.613e-02
    ## lag(value, 14)                                      1.017e-01  2.614e-02
    ## lag(value, 21)                                      6.515e-02  2.630e-02
    ## lag(value, 28)                                      5.888e-02  2.629e-02
    ## lag(value, 35)                                      7.458e-02  2.642e-02
    ## lag(value, 42)                                      5.420e-02  2.649e-02
    ## lag(value, 49)                                      8.625e-02  2.634e-02
    ## month(date, label = TRUE).L                        -1.271e+01  5.228e+00
    ## month(date, label = TRUE).Q                         2.530e+00  5.199e+00
    ## month(date, label = TRUE).C                        -1.085e+01  5.293e+00
    ## month(date, label = TRUE)^4                        -8.983e+00  5.275e+00
    ## month(date, label = TRUE)^5                        -1.136e+01  5.230e+00
    ## month(date, label = TRUE)^6                        -2.638e+00  5.315e+00
    ## month(date, label = TRUE)^7                        -7.895e+00  5.263e+00
    ## month(date, label = TRUE)^8                        -3.009e+00  5.301e+00
    ## month(date, label = TRUE)^9                         4.519e+00  5.315e+00
    ## month(date, label = TRUE)^10                        5.058e+00  5.322e+00
    ## month(date, label = TRUE)^11                       -6.280e+00  5.336e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.180e+01  2.417e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.857e+00  2.544e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.865 0.004227 ** 
    ## date                                                 3.192 0.001444 ** 
    ## lag(value, 1)                                        4.482 7.95e-06 ***
    ## lag(value, 7)                                        3.353 0.000819 ***
    ## lag(value, 14)                                       3.889 0.000105 ***
    ## lag(value, 21)                                       2.477 0.013365 *  
    ## lag(value, 28)                                       2.240 0.025240 *  
    ## lag(value, 35)                                       2.823 0.004825 ** 
    ## lag(value, 42)                                       2.046 0.040970 *  
    ## lag(value, 49)                                       3.275 0.001082 ** 
    ## month(date, label = TRUE).L                         -2.432 0.015144 *  
    ## month(date, label = TRUE).Q                          0.487 0.626526    
    ## month(date, label = TRUE).C                         -2.049 0.040604 *  
    ## month(date, label = TRUE)^4                         -1.703 0.088751 .  
    ## month(date, label = TRUE)^5                         -2.172 0.030003 *  
    ## month(date, label = TRUE)^6                         -0.496 0.619760    
    ## month(date, label = TRUE)^7                         -1.500 0.133775    
    ## month(date, label = TRUE)^8                         -0.568 0.570413    
    ## month(date, label = TRUE)^9                          0.850 0.395384    
    ## month(date, label = TRUE)^10                         0.950 0.342062    
    ## month(date, label = TRUE)^11                        -1.177 0.239438    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.883 1.16e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.089 0.002047 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.55 on 1483 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2594, Adjusted R-squared:  0.2484 
    ## F-statistic: 23.61 on 22 and 1483 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,548 × 2]> <tibble [28 × 2]> <split [1520|28]>
    ## 2 healthyR      <tibble [1,541 × 2]> <tibble [28 × 2]> <split [1513|28]>
    ## 3 healthyR.ts   <tibble [1,485 × 2]> <tibble [28 × 2]> <split [1457|28]>
    ## 4 healthyverse  <tibble [1,456 × 2]> <tibble [28 × 2]> <split [1428|28]>
    ## 5 healthyR.ai   <tibble [1,280 × 2]> <tibble [28 × 2]> <split [1252|28]>
    ## 6 TidyDensity   <tibble [1,131 × 2]> <tibble [28 × 2]> <split [1103|28]>
    ## 7 tidyAML       <tibble [739 × 2]>   <tibble [28 × 2]> <split [711|28]> 
    ## 8 RandomWalker  <tibble [161 × 2]>   <tibble [28 × 2]> <split [133|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7024676 | 102.72076 | 0.6392679 | 159.8025 | 0.8336444 | 0.0679174 |
| healthyR.data | 2 | LM | Test | 0.7992591 | 132.84706 | 0.7273512 | 139.7079 | 0.9456323 | 0.0318051 |
| healthyR.data | 3 | EARTH | Test | 2.8654651 | 713.71350 | 2.6076643 | 156.0379 | 3.2857399 | 0.0318051 |
| healthyR.data | 4 | NNAR | Test | 0.7347953 | 108.44177 | 0.6686871 | 183.9205 | 0.8531958 | 0.0029130 |
| healthyR | 1 | ARIMA | Test | 0.9823127 | 109.48362 | 0.7346597 | 165.9977 | 1.1479716 | 0.0328376 |
| healthyR | 2 | LM | Test | 0.9748189 | 108.08513 | 0.7290552 | 189.1968 | 1.1298134 | 0.0918349 |
| healthyR | 3 | EARTH | Test | 0.9828361 | 113.71367 | 0.7350512 | 185.2581 | 1.1365826 | 0.0918349 |
| healthyR | 4 | NNAR | Test | 1.0156394 | 128.32779 | 0.7595843 | 175.5306 | 1.1732584 | 0.0172310 |
| healthyR.ts | 1 | ARIMA | Test | 1.0495545 | 110.35097 | 0.6295151 | 138.5163 | 1.2503383 | 0.0840011 |
| healthyR.ts | 2 | LM | Test | 1.0371699 | 117.96782 | 0.6220869 | 130.3176 | 1.2293045 | 0.0840011 |
| healthyR.ts | 3 | EARTH | Test | 1.0340715 | 121.79919 | 0.6202285 | 127.5384 | 1.2226065 | 0.0840011 |
| healthyR.ts | 4 | NNAR | Test | 1.1133660 | 138.70998 | 0.6677888 | 183.8649 | 1.3195422 | 0.0215799 |
| healthyverse | 1 | ARIMA | Test | 0.6511345 | 268.53176 | 0.7667524 | 116.9884 | 0.8162729 | 0.0015598 |
| healthyverse | 2 | LM | Test | 0.7068944 | 344.22039 | 0.8324133 | 113.1735 | 0.8873017 | 0.0500267 |
| healthyverse | 3 | EARTH | Test | 0.6332415 | 250.07013 | 0.7456823 | 117.8740 | 0.7781736 | 0.0500267 |
| healthyverse | 4 | NNAR | Test | 0.5754679 | 129.34096 | 0.6776502 | 118.4895 | 0.7013037 | 0.1978513 |
| healthyR.ai | 1 | ARIMA | Test | 0.8294377 | 98.67880 | 0.6458965 | 174.6845 | 0.9939466 | 0.0047592 |
| healthyR.ai | 2 | LM | Test | 0.8588826 | 102.81620 | 0.6688258 | 154.2726 | 1.0411832 | 0.0631934 |
| healthyR.ai | 3 | EARTH | Test | 0.8683584 | 105.29578 | 0.6762047 | 148.2744 | 1.0627586 | 0.0631934 |
| healthyR.ai | 4 | NNAR | Test | 0.8339510 | 102.96235 | 0.6494111 | 153.2042 | 1.0117209 | 0.0175033 |
| TidyDensity | 1 | ARIMA | Test | 0.7476257 | 128.17256 | 0.6814804 | 119.8503 | 0.8680995 | 0.0243281 |
| TidyDensity | 2 | LM | Test | 0.7580421 | 158.70169 | 0.6909752 | 110.2272 | 0.8757131 | 0.0551446 |
| TidyDensity | 3 | EARTH | Test | 0.7725800 | 117.23388 | 0.7042268 | 129.4611 | 0.9023804 | 0.0551446 |
| TidyDensity | 4 | NNAR | Test | 0.7591026 | 92.68874 | 0.6919419 | 149.3983 | 0.9311227 | 0.0696205 |
| tidyAML | 1 | ARIMA | Test | 0.6597682 | 139.16459 | 0.6801209 | 102.1573 | 0.7834546 | 0.1228013 |
| tidyAML | 2 | LM | Test | 0.7026786 | 144.95708 | 0.7243549 | 100.7245 | 0.8339918 | 0.0008672 |
| tidyAML | 3 | EARTH | Test | 0.7580931 | 174.92158 | 0.7814789 | 101.6074 | 0.8691229 | 0.0008672 |
| tidyAML | 4 | NNAR | Test | 0.6611614 | 133.81149 | 0.6815570 | 101.8614 | 0.7941726 | 0.0838673 |
| RandomWalker | 1 | ARIMA | Test | 1.3632186 | 102.36016 | 0.5958822 | 145.3884 | 1.6538501 | 0.0259175 |
| RandomWalker | 2 | LM | Test | 1.3275031 | 103.80929 | 0.5802704 | 198.4238 | 1.5121442 | 0.0077979 |
| RandomWalker | 3 | EARTH | Test | 1.2864303 | 84.84042 | 0.5623170 | 149.9388 | 1.5251200 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3632370 | 140.44006 | 0.5958902 | 161.8124 | 1.5873728 | 0.0967408 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.702 103.  0.639  160. 0.834 0.0679 
    ## 2 healthyR             2 LM          Test  0.975 108.  0.729  189. 1.13  0.0918 
    ## 3 healthyR.ts          3 EARTH       Test  1.03  122.  0.620  128. 1.22  0.0840 
    ## 4 healthyverse         4 NNAR        Test  0.575 129.  0.678  118. 0.701 0.198  
    ## 5 healthyR.ai          1 ARIMA       Test  0.829  98.7 0.646  175. 0.994 0.00476
    ## 6 TidyDensity          1 ARIMA       Test  0.748 128.  0.681  120. 0.868 0.0243 
    ## 7 tidyAML              1 ARIMA       Test  0.660 139.  0.680  102. 0.783 0.123  
    ## 8 RandomWalker         2 LM          Test  1.33  104.  0.580  198. 1.51  0.00780

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1520|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1513|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1457|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1428|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1252|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1103|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [711|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [133|28]>  <mdl_tm_t [1 × 5]>

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
