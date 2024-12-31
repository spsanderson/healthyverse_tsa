Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
30 December, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 125,333
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

The last day in the data set is 2024-12-28 20:10:04, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.18322^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 125333        |
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
| r_version     |     88969 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     88969 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     88969 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10737 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-12-28 | 2023-04-12 | 1497 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1146543.06 | 1535249.82 | 355 | 14701 | 258949 | 2367968 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10335.02 | 17971.14 | 1 | 317 | 3098 | 11871 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-12-28 20:10:04 | 2023-04-12 21:56:31 | 75879 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 33S |       60 |

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
    ## -155.89  -34.80   -9.60   27.05  803.75 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.825e+02  7.985e+01
    ## date                                                1.094e-02  4.231e-03
    ## lag(value, 1)                                       1.324e-01  2.551e-02
    ## lag(value, 7)                                       8.958e-02  2.645e-02
    ## lag(value, 14)                                      1.085e-01  2.646e-02
    ## lag(value, 21)                                      4.481e-02  2.660e-02
    ## lag(value, 28)                                      7.007e-02  2.641e-02
    ## lag(value, 35)                                      7.224e-02  2.654e-02
    ## lag(value, 42)                                      4.418e-02  2.661e-02
    ## lag(value, 49)                                      1.024e-01  2.653e-02
    ## month(date, label = TRUE).L                        -1.085e+01  5.472e+00
    ## month(date, label = TRUE).Q                         2.240e+00  5.301e+00
    ## month(date, label = TRUE).C                        -1.102e+01  5.366e+00
    ## month(date, label = TRUE)^4                        -7.504e+00  5.354e+00
    ## month(date, label = TRUE)^5                        -1.264e+01  5.314e+00
    ## month(date, label = TRUE)^6                        -9.369e-01  5.381e+00
    ## month(date, label = TRUE)^7                        -8.982e+00  5.273e+00
    ## month(date, label = TRUE)^8                        -2.248e+00  5.267e+00
    ## month(date, label = TRUE)^9                         4.224e+00  5.257e+00
    ## month(date, label = TRUE)^10                        5.149e+00  5.257e+00
    ## month(date, label = TRUE)^11                       -6.235e+00  5.271e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.169e+01  2.434e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.973e+00  2.547e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.286 0.022415 *  
    ## date                                                 2.587 0.009786 ** 
    ## lag(value, 1)                                        5.190 2.40e-07 ***
    ## lag(value, 7)                                        3.387 0.000727 ***
    ## lag(value, 14)                                       4.101 4.34e-05 ***
    ## lag(value, 21)                                       1.685 0.092252 .  
    ## lag(value, 28)                                       2.654 0.008052 ** 
    ## lag(value, 35)                                       2.722 0.006572 ** 
    ## lag(value, 42)                                       1.660 0.097076 .  
    ## lag(value, 49)                                       3.859 0.000119 ***
    ## month(date, label = TRUE).L                         -1.982 0.047690 *  
    ## month(date, label = TRUE).Q                          0.422 0.672724    
    ## month(date, label = TRUE).C                         -2.053 0.040227 *  
    ## month(date, label = TRUE)^4                         -1.401 0.161287    
    ## month(date, label = TRUE)^5                         -2.378 0.017516 *  
    ## month(date, label = TRUE)^6                         -0.174 0.861791    
    ## month(date, label = TRUE)^7                         -1.704 0.088675 .  
    ## month(date, label = TRUE)^8                         -0.427 0.669637    
    ## month(date, label = TRUE)^9                          0.803 0.421913    
    ## month(date, label = TRUE)^10                         0.979 0.327516    
    ## month(date, label = TRUE)^11                        -1.183 0.236975    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.803 1.72e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.737 0.006269 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.82 on 1425 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2593, Adjusted R-squared:  0.2479 
    ## F-statistic: 22.67 on 22 and 1425 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,463 × 2]> <tibble [28 × 2]> <split [1435|28]>
    ## 2 healthyR      <tibble [1,456 × 2]> <tibble [28 × 2]> <split [1428|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,402 × 2]> <tibble [28 × 2]> <split [1374|28]>
    ## 5 healthyverse  <tibble [1,373 × 2]> <tibble [28 × 2]> <split [1345|28]>
    ## 6 healthyR.ai   <tibble [1,199 × 2]> <tibble [28 × 2]> <split [1171|28]>
    ## 7 TidyDensity   <tibble [1,053 × 2]> <tibble [28 × 2]> <split [1025|28]>
    ## 8 tidyAML       <tibble [669 × 2]>   <tibble [28 × 2]> <split [641|28]> 
    ## 9 RandomWalker  <tibble [103 × 2]>   <tibble [28 × 2]> <split [75|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7512892 | 276.06882 | 0.6423219 | 146.10154 | 0.9032876 | 0.1560276 |
| healthyR.data | 2 | LM | Test | 0.7789848 | 320.82205 | 0.6660006 | 142.39619 | 0.9352298 | 0.0000896 |
| healthyR.data | 3 | EARTH | Test | 0.8317882 | 447.68847 | 0.7111453 | 137.35205 | 0.9426715 | 0.0000896 |
| healthyR.data | 4 | NNAR | Test | 0.7949147 | 137.00537 | 0.6796200 | 163.35502 | 1.0571327 | 0.0581731 |
| healthyR | 1 | ARIMA | Test | 0.6845297 | 109.57272 | 0.6393010 | 162.20997 | 0.8253559 | 0.1279228 |
| healthyR | 2 | LM | Test | 0.6876339 | 103.37306 | 0.6422000 | 184.97568 | 0.8383174 | 0.0024461 |
| healthyR | 3 | EARTH | Test | 0.7619940 | 201.15339 | 0.7116470 | 151.62828 | 0.9044401 | 0.0024461 |
| healthyR | 4 | NNAR | Test | 0.6707714 | 125.97413 | 0.6264517 | 150.52204 | 0.8254674 | 0.1592788 |
| healthyR.ts | 1 | ARIMA | Test | 0.8716362 | 106.77533 | 0.7523472 | 123.54243 | 1.0568014 | 0.0012310 |
| healthyR.ts | 2 | LM | Test | 0.8770471 | 109.50823 | 0.7570176 | 123.01344 | 1.0605857 | 0.0012310 |
| healthyR.ts | 3 | EARTH | Test | 0.8800255 | 111.04559 | 0.7595884 | 122.69364 | 1.0629495 | 0.0012310 |
| healthyR.ts | 4 | NNAR | Test | 0.8626589 | 95.95527 | 0.7445986 | 181.92234 | 1.0499834 | 0.0968218 |
| healthyverse | 1 | ARIMA | Test | 0.4420756 | 182.51867 | 0.6969940 | 86.71796 | 0.5593268 | 0.3691800 |
| healthyverse | 2 | LM | Test | 0.5219516 | 311.78184 | 0.8229297 | 84.99794 | 0.6482289 | 0.0136093 |
| healthyverse | 3 | EARTH | Test | 0.5649724 | 346.57705 | 0.8907581 | 86.38075 | 0.6935173 | 0.0136093 |
| healthyverse | 4 | NNAR | Test | 0.5318133 | 196.99379 | 0.8384780 | 102.13136 | 0.6666577 | 0.0411925 |
| healthyR.ai | 1 | ARIMA | Test | 0.6653480 | 128.85485 | 0.8187794 | 171.65850 | 0.7527934 | 0.2761759 |
| healthyR.ai | 2 | LM | Test | 0.6760141 | 114.66186 | 0.8319050 | 153.51862 | 0.7844036 | 0.0011944 |
| healthyR.ai | 3 | EARTH | Test | 0.7740477 | 137.58914 | 0.9525455 | 160.55631 | 0.8823656 | 0.0011944 |
| healthyR.ai | 4 | NNAR | Test | 0.6545889 | 140.85029 | 0.8055392 | 149.84532 | 0.7538098 | 0.1307235 |
| TidyDensity | 1 | ARIMA | Test | 0.6506011 | 164.22907 | 0.7930470 | 115.73989 | 0.7989363 | 0.0284760 |
| TidyDensity | 2 | LM | Test | 0.7259885 | 294.45749 | 0.8849402 | 113.27628 | 0.8350178 | 0.0278475 |
| TidyDensity | 3 | EARTH | Test | 0.6676528 | 193.36448 | 0.8138321 | 119.10307 | 0.8086893 | 0.0278475 |
| TidyDensity | 4 | NNAR | Test | 0.6563480 | 92.54167 | 0.8000521 | 137.82601 | 0.8502633 | 0.0566444 |
| tidyAML | 1 | ARIMA | Test | 0.7898577 | 134.62458 | 0.8508003 | 101.99188 | 0.9238380 | 0.0878900 |
| tidyAML | 2 | LM | Test | 0.8506144 | 132.61701 | 0.9162449 | 107.84513 | 0.9862195 | 0.0446157 |
| tidyAML | 3 | EARTH | Test | 0.8033253 | 177.87091 | 0.8653070 | 95.77115 | 0.8808955 | 0.0446157 |
| tidyAML | 4 | NNAR | Test | 0.8285583 | 164.01300 | 0.8924869 | 100.74210 | 0.9110526 | 0.0018118 |
| RandomWalker | 1 | ARIMA | Test | 0.5956779 | 69.72598 | 0.2874097 | 62.34718 | 0.7463458 | 0.7626567 |
| RandomWalker | 2 | LM | Test | 1.2607464 | 94.64668 | 0.6082998 | 169.19903 | 1.3901292 | 0.0027678 |
| RandomWalker | 3 | EARTH | Test | 1.2364179 | 144.15989 | 0.5965615 | 103.70805 | 1.6086568 | 0.0027678 |
| RandomWalker | 4 | NNAR | Test | 14.3729851 | 1244.26758 | 6.9348474 | 166.06322 | 18.5161457 | 0.0014447 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.751 276.  0.642 146.  0.903 0.156 
    ## 2 healthyR              1 ARIMA       Test  0.685 110.  0.639 162.  0.825 0.128 
    ## 3 healthyR.ts           4 NNAR        Test  0.863  96.0 0.745 182.  1.05  0.0968
    ## 4 healthyverse          1 ARIMA       Test  0.442 183.  0.697  86.7 0.559 0.369 
    ## 5 healthyR.ai           1 ARIMA       Test  0.665 129.  0.819 172.  0.753 0.276 
    ## 6 TidyDensity           1 ARIMA       Test  0.651 164.  0.793 116.  0.799 0.0285
    ## 7 tidyAML               3 EARTH       Test  0.803 178.  0.865  95.8 0.881 0.0446
    ## 8 RandomWalker          1 ARIMA       Test  0.596  69.7 0.287  62.3 0.746 0.763

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1435|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1428|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1374|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1345|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1171|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1025|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [641|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [75|28]>   <mdl_tm_t [1 × 5]>

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
