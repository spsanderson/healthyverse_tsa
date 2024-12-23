Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 124,713
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

The last day in the data set is 2024-12-21 23:58:14, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3276.37
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 124713        |
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
| r_version     |     88461 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88461 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88461 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10708 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-21 | 2023-04-08 | 1490 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1148582.36 | 1536579.61 | 355 | 14701 | 260377 | 2368012 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10362.07 | 17991.79 | 1 | 336 | 3101 | 11931 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-21 23:58:14 | 2023-04-08 11:31:02 | 75531 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 22S |       60 |

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
    ## -155.78  -34.64   -9.67   26.89  803.41 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.820e+02  8.035e+01
    ## date                                                1.091e-02  4.258e-03
    ## lag(value, 1)                                       1.344e-01  2.556e-02
    ## lag(value, 7)                                       9.177e-02  2.649e-02
    ## lag(value, 14)                                      1.066e-01  2.653e-02
    ## lag(value, 21)                                      4.412e-02  2.666e-02
    ## lag(value, 28)                                      7.105e-02  2.647e-02
    ## lag(value, 35)                                      6.965e-02  2.662e-02
    ## lag(value, 42)                                      4.437e-02  2.678e-02
    ## lag(value, 49)                                      1.036e-01  2.660e-02
    ## month(date, label = TRUE).L                        -1.080e+01  5.497e+00
    ## month(date, label = TRUE).Q                         2.261e+00  5.343e+00
    ## month(date, label = TRUE).C                        -1.097e+01  5.409e+00
    ## month(date, label = TRUE)^4                        -7.471e+00  5.379e+00
    ## month(date, label = TRUE)^5                        -1.258e+01  5.329e+00
    ## month(date, label = TRUE)^6                        -9.340e-01  5.388e+00
    ## month(date, label = TRUE)^7                        -8.924e+00  5.277e+00
    ## month(date, label = TRUE)^8                        -2.273e+00  5.270e+00
    ## month(date, label = TRUE)^9                         4.233e+00  5.260e+00
    ## month(date, label = TRUE)^10                        5.150e+00  5.260e+00
    ## month(date, label = TRUE)^11                       -6.198e+00  5.273e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.164e+01  2.440e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.957e+00  2.552e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.265 0.023662 *  
    ## date                                                 2.563 0.010489 *  
    ## lag(value, 1)                                        5.258 1.68e-07 ***
    ## lag(value, 7)                                        3.464 0.000548 ***
    ## lag(value, 14)                                       4.017 6.21e-05 ***
    ## lag(value, 21)                                       1.655 0.098075 .  
    ## lag(value, 28)                                       2.684 0.007361 ** 
    ## lag(value, 35)                                       2.617 0.008968 ** 
    ## lag(value, 42)                                       1.657 0.097770 .  
    ## lag(value, 49)                                       3.897 0.000102 ***
    ## month(date, label = TRUE).L                         -1.965 0.049629 *  
    ## month(date, label = TRUE).Q                          0.423 0.672274    
    ## month(date, label = TRUE).C                         -2.027 0.042810 *  
    ## month(date, label = TRUE)^4                         -1.389 0.165121    
    ## month(date, label = TRUE)^5                         -2.360 0.018405 *  
    ## month(date, label = TRUE)^6                         -0.173 0.862406    
    ## month(date, label = TRUE)^7                         -1.691 0.091044 .  
    ## month(date, label = TRUE)^8                         -0.431 0.666377    
    ## month(date, label = TRUE)^9                          0.805 0.421114    
    ## month(date, label = TRUE)^10                         0.979 0.327666    
    ## month(date, label = TRUE)^11                        -1.175 0.240077    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.768 2.05e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.726 0.006485 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.85 on 1418 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2594, Adjusted R-squared:  0.2479 
    ## F-statistic: 22.57 on 22 and 1418 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,456 × 2]> <tibble [28 × 2]> <split [1428|28]>
    ## 2 healthyR      <tibble [1,449 × 2]> <tibble [28 × 2]> <split [1421|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,395 × 2]> <tibble [28 × 2]> <split [1367|28]>
    ## 5 healthyverse  <tibble [1,366 × 2]> <tibble [28 × 2]> <split [1338|28]>
    ## 6 healthyR.ai   <tibble [1,192 × 2]> <tibble [28 × 2]> <split [1164|28]>
    ## 7 TidyDensity   <tibble [1,046 × 2]> <tibble [28 × 2]> <split [1018|28]>
    ## 8 tidyAML       <tibble [662 × 2]>   <tibble [28 × 2]> <split [634|28]> 
    ## 9 RandomWalker  <tibble [96 × 2]>    <tibble [28 × 2]> <split [68|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7352115 | 194.16305 | 0.6350211 | 142.44502 | 0.9173392 | 0.0756399 |
| healthyR.data | 2 | LM | Test | 0.7777085 | 280.38639 | 0.6717269 | 138.59081 | 0.9269242 | 0.0097053 |
| healthyR.data | 3 | EARTH | Test | 0.7963264 | 324.47078 | 0.6878077 | 136.78456 | 0.9260858 | 0.0097053 |
| healthyR.data | 4 | NNAR | Test | 0.8368800 | 139.78833 | 0.7228348 | 165.93818 | 1.0901430 | 0.0005698 |
| healthyR | 1 | ARIMA | Test | 0.6293324 | 130.66988 | 0.7000797 | 150.20054 | 0.7655544 | 0.0679209 |
| healthyR | 2 | LM | Test | 0.6399632 | 105.54514 | 0.7119055 | 189.22510 | 0.7621945 | 0.0106463 |
| healthyR | 3 | EARTH | Test | 0.6786138 | 171.64610 | 0.7549011 | 144.86862 | 0.8153538 | 0.0106463 |
| healthyR | 4 | NNAR | Test | 0.5879923 | 117.20014 | 0.6540923 | 148.59180 | 0.7311658 | 0.0834193 |
| healthyR.ts | 1 | ARIMA | Test | 0.8580657 | 109.51266 | 0.8737960 | 115.83867 | 1.0252770 | 0.0599835 |
| healthyR.ts | 2 | LM | Test | 0.8469292 | 101.72952 | 0.8624552 | 118.90457 | 1.0063808 | 0.0599835 |
| healthyR.ts | 3 | EARTH | Test | 0.8479007 | 102.82738 | 0.8634446 | 118.24935 | 1.0088668 | 0.0599835 |
| healthyR.ts | 4 | NNAR | Test | 0.8306113 | 92.97769 | 0.8458382 | 175.35711 | 0.9991663 | 0.0269582 |
| healthyverse | 1 | ARIMA | Test | 0.4406833 | 176.76869 | 0.8352173 | 84.19417 | 0.5632884 | 0.4247914 |
| healthyverse | 2 | LM | Test | 0.5083934 | 326.34636 | 0.9635469 | 82.20876 | 0.6140827 | 0.0322530 |
| healthyverse | 3 | EARTH | Test | 0.5759761 | 193.57945 | 1.0916348 | 107.52175 | 0.6981479 | 0.0322530 |
| healthyverse | 4 | NNAR | Test | 0.5565271 | 186.80517 | 1.0547735 | 104.40436 | 0.6824754 | 0.1272871 |
| healthyR.ai | 1 | ARIMA | Test | 0.6937588 | 92.98924 | 0.9182517 | 159.34726 | 0.8001116 | 0.1814229 |
| healthyR.ai | 2 | LM | Test | 0.7236180 | 122.36111 | 0.9577730 | 166.10731 | 0.8023486 | 0.0191604 |
| healthyR.ai | 3 | EARTH | Test | 0.7841442 | 145.38955 | 1.0378849 | 141.34949 | 0.9392287 | 0.0191604 |
| healthyR.ai | 4 | NNAR | Test | 0.6759304 | 106.53540 | 0.8946543 | 156.08764 | 0.7692435 | 0.1163623 |
| TidyDensity | 1 | ARIMA | Test | 0.6332614 | 194.75470 | 0.7415358 | 110.13196 | 0.7682372 | 0.2282682 |
| TidyDensity | 2 | LM | Test | 0.7283225 | 273.10739 | 0.8528504 | 109.00308 | 0.8581541 | 0.0489913 |
| TidyDensity | 3 | EARTH | Test | 0.6872312 | 200.02747 | 0.8047334 | 114.03622 | 0.8286596 | 0.0489913 |
| TidyDensity | 4 | NNAR | Test | 0.6766213 | 112.94656 | 0.7923094 | 141.52158 | 0.8505544 | 0.0618405 |
| tidyAML | 1 | ARIMA | Test | 0.6926509 | 96.07175 | 0.9588810 | 95.83354 | 0.8384172 | 0.1471854 |
| tidyAML | 2 | LM | Test | 0.7231888 | 102.76494 | 1.0011564 | 90.51817 | 0.9046123 | 0.0056144 |
| tidyAML | 3 | EARTH | Test | 0.6911941 | 132.12300 | 0.9568642 | 83.49046 | 0.8145049 | 0.0056144 |
| tidyAML | 4 | NNAR | Test | 0.7182273 | 127.19264 | 0.9942880 | 91.55030 | 0.8429585 | 0.0061409 |
| RandomWalker | 1 | ARIMA | Test | 1.4078862 | 100.00000 | 0.6392971 | 200.00000 | 1.5032617 | NA |
| RandomWalker | 2 | LM | Test | 1.4522602 | 104.00116 | 0.6594466 | 183.58019 | 1.5443731 | 0.0013703 |
| RandomWalker | 3 | EARTH | Test | 1.6263713 | 128.01489 | 0.7385074 | 158.40319 | 1.7830001 | 0.0013703 |
| RandomWalker | 4 | NNAR | Test | 1.3997722 | 100.48176 | 0.6356127 | 148.64148 | 1.6283386 | 0.0041032 |

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
    ## 1 healthyR.d…         1 ARIMA       Test  0.735 194.  0.635 142.  0.917  0.0756 
    ## 2 healthyR            4 NNAR        Test  0.588 117.  0.654 149.  0.731  0.0834 
    ## 3 healthyR.ts         4 NNAR        Test  0.831  93.0 0.846 175.  0.999  0.0270 
    ## 4 healthyver…         1 ARIMA       Test  0.441 177.  0.835  84.2 0.563  0.425  
    ## 5 healthyR.ai         4 NNAR        Test  0.676 107.  0.895 156.  0.769  0.116  
    ## 6 TidyDensity         1 ARIMA       Test  0.633 195.  0.742 110.  0.768  0.228  
    ## 7 tidyAML             3 EARTH       Test  0.691 132.  0.957  83.5 0.815  0.00561
    ## 8 RandomWalk…         1 ARIMA       Test  1.41  100   0.639 200   1.50  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1428|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1421|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1367|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1338|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1164|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1018|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [634|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [68|28]>   <mdl_tm_t [1 × 5]>

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
